//
// Copyright (C) 2014 The Mercury Team
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.ThreadFactory;
import java.util.*;

/**
 * This class manages execution of Mercury threads and tasks.
 *
 * The pool attempts to reduce overheads, especially in embarrassingly
 * parallel workloads, by re-using threads and restricting the overall
 * number of threads.  By default a thread is created for each hardware
 * thread available.  If all threads are making progress then no new threads
 * will be created even if tasks are waiting, reducing overheads.  But if
 * one or more threads block on a barrier, channel, mvar or semaphore, then
 * new threads may be created to avoid deadlocks and attempt to keep all
 * processors busy.
 *
 * TODO: Currently the thread pool does not know when a thread is blocked in
 * foreign code or performing IO.
 *
 * Java's API provides several different thread pools, see
 * java.util.concurrent.Executors, none of which are suitable for our needs
 * so we have implemented our own in this class.  Specifically the fixed
 * thread pool is unsuitable as we want to be able to temporarily exceed the
 * normal number of threads to overcome deadlocks (and to keep processors
 * busy); and the cached thread pools, which are also very similar to the
 * ThreadPoolExecutor class, create threads (up to a maximum number) even if
 * all the processors are busy.  Additionally we cannot instrument this code
 * as easily for parallel profiling.
 *
 * The JavaInternal class holds the reference to the thread pool.  The pool
 * should be executed by calling run() from the primordial thread.  run()
 * will exit once all the work has been completed; it will shutdown the
 * worker threads as it exits.  The runMain() method provides a convenient
 * wrapper for run which also executes a task, such as the main/2 predicate
 * of the application.
 */
public class MercuryThreadPool
    implements Runnable
{
    public static final boolean         debug = false;

    private static MercuryThreadPool    instance;

    private MercuryThreadFactory        thread_factory;

    /*
     * Locking:
     * Rather than synchronize on the 'this' pointer we create explicit
     * locks.  The numbers of threads and list of threads are protected by
     * the threads_lock lock.  The queue of tasks is protected by the
     * tasks_lock.  Separate condition variables are also used,  This avoids
     * threads waiting on conditions to not be woken up by a different
     * condition, for example for example a thread can wait for the pool to
     * shutdown without being woken when new work arrives.
     *
     * Safety: If you acquire more than one lock you must acquire locks in
     *         this order: tasks_lock, threads_lock then main_loop_lock.  If
     *         you wait on any condition you must only hold that condition's
     *         lock.
     */

    /*
     * Worker threads
     */
    private int                         thread_pool_size;
    private int                         user_specified_size;
    /*
     * The sum of num_threads_* is the total number of threads in the pool
     */
    private int                         num_threads_working;
    private volatile int                num_threads_waiting;
    private int                         num_threads_blocked;
    private int                         num_threads_other;
    private LinkedList<MercuryThread>   threads;
    private Lock                        threads_lock;

    /*
     * Tasks
     */
    private Deque<Task>             tasks;
    private boolean                 should_shutdown;
    private long                    num_tasks_submitted;
    private long                    num_tasks_completed;
    private Lock                    tasks_lock;
    private Condition               thread_wait_for_task_condition;

    /*
     * Main loop condition.
     */
    private Lock                    main_loop_lock;
    private Condition               main_loop_condition;

    /**
     * Package-private constructor
     */
    MercuryThreadPool(int size)
    {
        thread_factory = new MercuryThreadFactory(this);

        user_specified_size = size;
        thread_pool_size = size;
        num_threads_working = 0;
        num_threads_waiting = 0;
        num_threads_blocked = 0;
        num_threads_other = 0;
        threads = new LinkedList<MercuryThread>();
        threads_lock = new ReentrantLock();

        /*
         * The task queue is for times longer than the number of threads.
         * This decision is arbitrary and should be revised after doing some
         * benchmarking.  This capacity is just the initial size.  The
         * ArrayDeque task will grow as needed.
         */
        tasks = new ArrayDeque<Task>(size*4);
        should_shutdown = false;
        num_tasks_submitted = 0;
        num_tasks_completed = 0;
        tasks_lock = new ReentrantLock();
        thread_wait_for_task_condition = tasks_lock.newCondition();

        main_loop_lock = new ReentrantLock();
        main_loop_condition = main_loop_lock.newCondition();
    }

    /**
     * Create a new thread to execute the given task.
     * @param task The task the new thread should execute.
     * @return The task.
     */
    public void submitExclusiveThread(Task task)
    {
        Thread t = thread_factory.newThread(task);
        t.start();
    }

    /**
     * Submit a task for execution.
     * @param task The task.
     * @return The task.
     */
    public void submit(Task task)
    {
        tasks_lock.lock();
        try {
            tasks.offer(task);
            task.scheduled();
            num_tasks_submitted++;
            thread_wait_for_task_condition.signal();
        } finally {
            tasks_lock.unlock();
        }

        signalMainLoop();
    }

    /**
     * Wait for a task to arrive.
     * This call will block if there are no tasks available to execute.
     * @return A task to execute.
     */
    public Task workerGetTask()
            throws InterruptedException
    {
        Task    task;

        tasks_lock.lock();
        try {
            do {
                if (tooManyThreadsWaiting()) {
                    /*
                     * We already have plenty of threads waiting for
                     * work.  Ask this one to shutdown.
                     */
                    return null;
                }

                task = tasks.poll();
                if (task != null) {
                    return task;
                }

                /*
                 * XXX: If there are tasks currently being executed that
                 * spawn other tasks while should_shutdown is true, then
                 * there's a possibility that this could deadlock as we
                 * don't check that here.
                 */
                if (should_shutdown) {
                    return null;
                }

                thread_wait_for_task_condition.await();
            } while (true);
        } finally {
            tasks_lock.unlock();
        }
    }

    protected boolean tooManyThreadsWaiting()
    {
        /*
         * num_threads_waiting is volatile because we use it to perform a
         * double checked lock optimisation.
         */
        if (num_threads_waiting > thread_pool_size) {
            threads_lock.lock();
            // Recheck with lock.
            try {
                return num_threads_waiting > thread_pool_size;
            } finally {
                threads_lock.unlock();
            }
        } else {
            return false;
        }
    }

    public void updateThreadCounts(ThreadStatus old, ThreadStatus new_)
    {
        threads_lock.lock();
        try {
            switch (old) {
                case WORKING:
                    num_threads_working--;
                    break;
                case IDLE:
                    num_threads_waiting--;
                    break;
                case BLOCKED:
                    num_threads_blocked--;
                    break;
                case OTHER:
                    num_threads_other--;
                    break;
                default:
                    assert false : old;
            }

            switch (new_) {
                case WORKING:
                    num_threads_working++;
                    break;
                case IDLE:
                    num_threads_waiting++;
                    break;
                case BLOCKED:
                    num_threads_blocked++;
                    break;
                case OTHER:
                    num_threads_other++;
                    break;
                default:
                    assert false : new_;
            }
        } finally {
            threads_lock.unlock();
        }

        switch (new_) {
            case BLOCKED:
                signalMainLoop();
                break;
            default:
                break;
        }
    }

    public void threadShutdown(MercuryWorkerThread thread,
        ThreadStatus state)
    {
        threads_lock.lock();
        try {
            switch (state) {
                case WORKING:
                    num_threads_working--;
                    break;
                case IDLE:
                    num_threads_waiting--;
                    break;
                case BLOCKED:
                    num_threads_blocked--;
                    break;
                case OTHER:
                    num_threads_other--;
                    break;
                default:
                    assert false : state;
            }
            threads.remove(thread);
        } finally {
            threads_lock.unlock();
        }
    }

    public void taskDone(Task task)
    {
        incrementCompletedTasks();
    }

    public void taskFailed(Task task, Exception e)
    {
        incrementCompletedTasks();
    }

    protected void incrementCompletedTasks()
    {
        long num_tasks_submitted;
        long num_tasks_completed;

        tasks_lock.lock();
        try {
            this.num_tasks_completed++;
            num_tasks_submitted = this.num_tasks_submitted;
            num_tasks_completed = this.num_tasks_completed;
        } finally {
            tasks_lock.unlock();
        }

        if (num_tasks_submitted == num_tasks_completed) {
            signalMainLoop();
        }
    }

    protected void signalMainLoop()
    {
        main_loop_lock.lock();
        try {
            main_loop_condition.signal();
        } finally {
            main_loop_lock.unlock();
        }
    }

    /**
     * Warm up the thread pool by starting some initial threads (currently
     * one).
     */
    protected void startupInitialThreads()
    {
        MercuryWorkerThread t = thread_factory.newWorkerThread();

        threads_lock.lock();
        try {
            threads.add(t);
            num_threads_other++;
        } finally {
            threads_lock.unlock();
        }

        t.start();
    }

    /**
     * Check threads.
     * Checks the numbers and status of the worker threads and starts more
     * threads if required.
     */
    protected void checkThreads()
    {
        int num_new_threads;
        int num_tasks_waiting;
        List<MercuryWorkerThread> new_threads =
            new LinkedList<MercuryWorkerThread>();

        /*
         * If necessary poll the Java runtime to see if the number of
         * available processors has changed.  I don't know if this actually
         * changes in practice however the Java API says that one can and
         * should poll it.
         */
        thread_pool_size = (user_specified_size > 0) ? user_specified_size :
            Runtime.getRuntime().availableProcessors();

        tasks_lock.lock();
        try {
            num_tasks_waiting = tasks.size();
        } finally {
            tasks_lock.unlock();
        }

        if (num_tasks_waiting > 0) {
            /*
             * If we have fewer than the default number of threads then
             * start some new threads.
             */
            threads_lock.lock();
            try {
                int num_threads = num_threads_working + num_threads_waiting +
                    num_threads_blocked + num_threads_other;
                int num_threads_limit = thread_pool_size +
                    num_threads_blocked;
                // Determine the number of new threads that we want.
                num_new_threads = num_tasks_waiting - num_threads_other -
                    num_threads_waiting;
                if (num_new_threads + num_threads > num_threads_limit) {
                    /*
                     * The number of threads that we want, plus the number
                     * we already have, exceeds the number that we're
                     * allowed to have.
                     */
                    num_new_threads = num_threads_limit - num_threads;
                }
                if (debug) {
                    System.err.println("Pool has " +
                        num_threads_working + " working threads, " +
                        num_threads_blocked + " blocked threads, " +
                        num_threads_waiting + " idle threads, " +
                        num_threads_other + " other (starting up) threads. " +
                        "will create " + num_new_threads + " new threads.");
                }

                for (int i = 0; i < num_new_threads; i++) {
                    MercuryWorkerThread t = thread_factory.newWorkerThread();
                    new_threads.add(t);
                    threads.add(t);
                    num_threads_other++;
                }
            } finally {
                threads_lock.unlock();
            }

            /*
             * Start the threads while we're not holding the lock, this makes
             * the above critical section smaller.
             */
            for (MercuryWorkerThread t : new_threads) {
                t.start();
            }
        }
    }

    /**
     * Get the number of currently queued tasks.
     * @return true if all tasks have been completed.
     */
    protected boolean checkTasks()
    {
        long    num_tasks_submitted;
        long    num_tasks_completed;
        boolean done;

        tasks_lock.lock();
        try {
            num_tasks_submitted = this.num_tasks_submitted;
            num_tasks_completed = this.num_tasks_completed;

            done = (num_tasks_submitted == num_tasks_completed);
        } finally {
            tasks_lock.unlock();
        }

        return done;
    }

    /**
     * Run the thread pool.  This is usually called by runMain()
     */
    public void run()
    {
        boolean done = false;
        long    num_tasks_submitted;
        long    num_tasks_completed;
        boolean tasks_locked = false;
        boolean main_loop_locked = false;

        try {
            do {
                /*
                 * Have all the tasks been completed?
                 */
                tasks_lock.lock();
                try {
                    num_tasks_submitted = this.num_tasks_submitted;
                    num_tasks_completed = this.num_tasks_completed;
                    done = (num_tasks_submitted == num_tasks_completed);

                    if (!done) {
                        /*
                         * Start new threads if we have fewer than the
                         * thread_pool_size
                         */
                        checkThreads();

                        /*
                         * Acquire the main loop lock while we're still
                         * holding tasks_lock.  This prevents a race whereby
                         * we release the locks below and then the last task
                         * finishes but we don't get its signal on
                         * main_loop_condition because we weren't holding
                         * the lock.
                         *
                         * To preserve the locking order we must NOT
                         * reacquire tasks_lock after releasing them while
                         * still holding the main loop lock.  This must also
                         * be executed after checkThreads();
                         */
                        main_loop_lock.lock();
                        main_loop_locked = true;
                    }
                } finally {
                    tasks_lock.unlock();
                    tasks_locked = false;
                }

                if (!done) {
                    if (!main_loop_locked) {
                        main_loop_lock.lock();
                        main_loop_locked = true;
                    }
                    try {
                        main_loop_condition.await();
                    } catch (InterruptedException e) {
                        continue;
                    } finally {
                        main_loop_lock.unlock();
                        main_loop_locked = false;
                    }
                }

                /*
                 * If we don't execute the if branch above, then the
                 * main_lock cannot be held because neither of the two
                 * places where it could have been acquired would be
                 * executed because done == true.
                 */
            } while (!done);
        } finally {
            if (main_loop_locked) {
                main_loop_lock.unlock();
            }
            if (tasks_locked) {
                tasks_lock.unlock();
            }
        }

        shutdown();
    }

    protected void shutdown()
    {
        tasks_lock.lock();
        try {
            should_shutdown = true;
            thread_wait_for_task_condition.signalAll();
        } finally {
            tasks_lock.unlock();
        }

    }

    /**
     * Run the main/2 predicate and wait for its completion.
     */
    public void runMain(Runnable run_main)
    {
        Task main_task = new Task(run_main);
        startupInitialThreads();
        submit(main_task);
        try {
            /*
             * This thread (the primordial thread) operates the thread pool
             * until the program is finished
             */
            run();
            jmercury.runtime.JavaInternal.run_finalisers();
        } catch (jmercury.runtime.Exception e) {
            JavaInternal.reportUncaughtException(e);
        }
    }

    /**
     * This class creates and names Mercury threads.
     * The factory is responsible for creating threads with unique IDs,
     */
    private static class MercuryThreadFactory implements ThreadFactory
    {
        public MercuryThreadPool    pool;
        public volatile int         next_thread_id;

        /**
         * Create a new thread factory.
         */
        public MercuryThreadFactory(MercuryThreadPool pool)
        {
            this.pool = pool;
            next_thread_id = 0;
        }

        /**
         * Create a new thread to execute the given task.
         * @param runnable The task the new thread should execute.
         */
        public MercuryThread newThread(final Runnable runnable) {
            return new NativeThread(allocateThreadId(), runnable);
        }

        public MercuryWorkerThread newWorkerThread() {
            return new MercuryWorkerThread(pool, allocateThreadId());
        }

        /**
         * Allocate a unique ID for a thread.
         */
        protected synchronized int allocateThreadId() {
            return next_thread_id++;
        }
    }
}

