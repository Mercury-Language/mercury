// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2016, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
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
 * number of threads. By default a thread is created for each hardware
 * thread available. If all threads are making progress then no new threads
 * will be created even if tasks are waiting, reducing overheads. But if
 * one or more threads block on a barrier, channel, mvar or semaphore, then
 * new threads may be created to avoid deadlocks and attempt to keep all
 * processors busy.
 *
 * TODO: Currently the thread pool does not know when a thread is blocked
 * in foreign code or performing IO.
 *
 * Java's API provides several different thread pools, see
 * java.util.concurrent.Executors, none of which are suitable for our needs
 * so we have implemented our own in this class. Specifically the fixed
 * thread pool is unsuitable as we want to be able to temporarily exceed the
 * normal number of threads to overcome deadlocks (and to keep processors
 * busy); and the cached thread pools, which are also very similar to the
 * ThreadPoolExecutor class, create threads (up to a maximum number) even if
 * all the processors are busy. Additionally we cannot instrument this code
 * as easily for parallel profiling.
 *
 * The JavaInternal class holds the reference to the thread pool. The pool
 * should be executed by calling run() from the primordial thread. run()
 * will exit once all the work has been completed; it will shutdown the
 * worker threads as it exits. The runMain() method provides a convenient
 * wrapper for run which also executes a task, such as the main/2 predicate
 * of the application.
 */
public class MercuryThreadPool
    implements Runnable
{
    public static final boolean         debug = false;

    private MercuryThreadFactory        thread_factory;

    // Locking:
    // Rather than synchronize on the 'this' pointer we create explicit locks.
    // The numbers of threads and list of threads are protected by the
    // threads_lock lock. The queue of tasks is protected by the tasks_lock.
    // Separate condition variables are also used. This avoids threads waiting
    // on conditions to not be woken up by a different condition, for example
    // for example a thread can wait for the pool to shutdown without
    // being woken when new work arrives.
    //
    // Safety: If you acquire more than one lock you must acquire locks in
    // this order: tasks_lock, threads_lock then main_loop_lock. If you wait
    // on any condition, you must only hold that condition's lock.

    // Worker threads.
    private int                         thread_pool_size;
    private int                         user_specified_size;
    // The sum of num_threads_* is the total number of threads in the pool.
    private int                         num_threads_working;
    private volatile int                num_threads_waiting;
    private int                         num_threads_blocked;
    private int                         num_threads_other;
    private LinkedList<MercuryThread>   threads;
    private Lock                        threads_lock;

    // Tasks.
    private Deque<Task>             tasks;
    private long                    num_tasks_submitted;
    private long                    num_tasks_completed;
    private Lock                    tasks_lock;
    private Condition               thread_wait_for_task_condition;

    // Has a shutdown request been received (protected by tasks_lock)?
    private boolean                 shutdown_request;
    // Shutdown because the program is aborting (if true, then don't run
    // finalisers).
    private boolean                 shutdown_abort;
    // True if worker threads should exit (the pool is shutting down).
    private boolean                 shutdown_now;
    // True if the thread pool is running (including starting up and
    // shutting down).
    private boolean                 running;

    // Main loop condition.
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

        // The task queue is four times longer than the number of threads.
        // This decision is arbitrary and should be revised after doing some
        // benchmarking. This capacity is just the initial size.
        // The ArrayDeque task will grow as needed.
        tasks = new ArrayDeque<Task>(size*4);
        shutdown_request = false;
        shutdown_abort = false;
        shutdown_now = false;
        running = false;
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
            if (!running) {
                startup();
            }
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
                    // We already have plenty of threads waiting for work.
                    // Ask this one to shutdown.
                    return null;
                }

                task = tasks.poll();
                if (task != null) {
                    return task;
                }

                // XXX: If there are tasks currently being executed that
                // spawn other tasks while should_shutdown is true, then
                // there is a possibility that this could deadlock as we
                // don't check that here.
                if (shutdown_now) {
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
        // num_threads_waiting is volatile because we use it to perform
        // a double checked lock optimisation.
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

        signalMainLoop();
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
            // There may be more than one thread waiting on this condition
            // such as when more than one thread calls waitForShutdown().
            // I can't imagine this happening, but it is allowed.
            main_loop_condition.signalAll();
        } finally {
            main_loop_lock.unlock();
        }
    }

    /**
     * Warm up the thread pool by starting some initial threads.
     * Currently starts a single thread, other threads are started on demand.
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

        // If necessary, poll the Java runtime to see if the number of
        // available processors has changed. I don't know if this actually
        // changes in practice, however the Java API says that one can and
        // should poll it.
        thread_pool_size = (user_specified_size > 0) ? user_specified_size :
            Runtime.getRuntime().availableProcessors();

        tasks_lock.lock();
        try {
            num_tasks_waiting = tasks.size();
        } finally {
            tasks_lock.unlock();
        }

        if (num_tasks_waiting > 0) {
            // If we have fewer than the default number of threads,
            // then start some new threads.
            threads_lock.lock();
            try {
                int num_threads = numThreads();
                int num_threads_limit = thread_pool_size +
                    num_threads_blocked;
                // Determine the number of new threads that we want.
                num_new_threads = num_tasks_waiting - num_threads_other -
                    num_threads_waiting;
                if (num_new_threads + num_threads > num_threads_limit) {
                    // The number of threads that we want, plus the number
                    // we already have, exceeds the number that we are
                    // allowed to have.
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

            // Start the threads while we are not holding the lock;
            // this makes the above critical section smaller.
            for (MercuryWorkerThread t : new_threads) {
                t.start();
            }
        }
    }

    /**
     * Get the total number of threads.
     * Caller must hold threads lock.
     */
    private int numThreads() {
        return num_threads_working + num_threads_waiting +
            num_threads_blocked + num_threads_other;
    }

    /**
     * Run the thread pool.
     * The calling thread is used to "run" the thread pool. Its main job is
     * to keep the correct number of worker threads alive. It does not return
     * until the thread pool is stopped (with a call to shutdown()).
     * run() is usually called by runMain(), and shutdown() is usually
     * called by the main task itself.
     */
    public void run()
    {
        boolean will_shutdown = false;
        boolean tasks_locked = false;
        boolean main_loop_locked = false;

        try {
            do {
                // Have all the tasks been completed?
                tasks_lock.lock();
                tasks_locked = true;
                try {
                    boolean okay_to_shutdown;
                    long    num_tasks_submitted;
                    long    num_tasks_completed;

                    num_tasks_submitted = this.num_tasks_submitted;
                    num_tasks_completed = this.num_tasks_completed;
                    okay_to_shutdown =
                        (num_tasks_submitted == num_tasks_completed) ||
                        shutdown_abort;
                    will_shutdown = okay_to_shutdown && shutdown_request;

                    if (!will_shutdown) {
                        // Start new threads if we have fewer than the
                        // thread_pool_size
                        checkThreads();

                        // Acquire the main loop lock while we are still
                        // holding tasks_lock. This prevents a race whereby
                        // we release the locks below and then the last task
                        // finishes but we don't get its signal on
                        // main_loop_condition because we weren't holding
                        // the lock.
                        //
                        // To preserve the locking order we must NOT
                        // reacquire tasks_lock after releasing them while
                        // still holding the main loop lock. This must also
                        // be executed after checkThreads();
                        main_loop_lock.lock();
                        main_loop_locked = true;
                    }
                } finally {
                    tasks_lock.unlock();
                    tasks_locked = false;
                }

                if (!will_shutdown) {
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

                // If we don't execute the if branch above, then the
                // main_lock cannot be held because neither of the two places
                // where it could have been acquired would be executed
                // because done == true.
            } while (!will_shutdown);
        } finally {
            if (main_loop_locked) {
                main_loop_lock.unlock();
            }
            if (tasks_locked) {
                tasks_lock.unlock();
            }
        }

        doShutdown();
    }

    /**
     * Perform the shutdown.
     */
    private void doShutdown()
    {
        tasks_lock.lock();
        try {
            shutdown_now = true;
            running = false;
            thread_wait_for_task_condition.signalAll();
        } finally {
            tasks_lock.unlock();
        }
    }

    /**
     * Wait for all the worker threads to exit.
     * Even though the JVM is not supposed to exit until all the non-daemon
     * threads have exited the effects of some threads may get lost.
     * I suspect this may be because the main thread closes stdout and stderr.
     * Waiting for the worker threads fixes the problem - pbone.
     */
    public boolean waitForShutdown()
    {
        boolean has_shutdown = false;

        if (shutdown_request) {
            do {
                main_loop_lock.lock();
                try {
                    has_shutdown = numThreads() == 0;
                    if (!has_shutdown) {
                        main_loop_condition.await();
                    }
                } catch (InterruptedException e) {
                    continue;
                } finally {
                    main_loop_lock.unlock();
                }
            } while (!has_shutdown);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Start the thread pool in its own thread.
     * Normally the thread pool ie executed directly by the main thread.
     * However, when Mercury is used as a library by a native Java application,
     * this is not true, and the thread pool runs in a thread of its own.
     */
    public MercuryThread startup()
    {
        MercuryThread thread;

        tasks_lock.lock();
        try {
            if (running) {
                return null;
            }
            running = true;
        } finally {
            tasks_lock.unlock();
        }

        startupInitialThreads();
        thread = thread_factory.newThread(new Runnable() {
            public void run() {
                MercuryThreadPool.this.startupInitialThreads();
                MercuryThreadPool.this.run();
            }
        });
        thread.start();
        return thread;
    }

    /**
     * Request that the thread pool shutdown.
     * The thread pool will shutdown if: shutdown() has
     * been called (implicitly when main/2 is written in Mercury) and there
     * are no remaining tasks either queued or running (spawn_native tasks
     * are not included).
     *
     * This method is asynchronous, it will not wait for the thread pool to
     * shutdown.
     */
    public boolean shutdown(boolean abort)
    {
        tasks_lock.lock();
        try {
            if (running && !shutdown_request) {
                shutdown_request = true;
                shutdown_abort = abort;
            } else {
                return false;
            }
        } finally {
            tasks_lock.unlock();
        }

        signalMainLoop();
        return true;
    }

    /**
     * Run the main/2 predicate and wait for its completion.
     * This method implicitly starts and stops the thread pool.
     */
    public void runMain(final Runnable run_main)
    {
        Task main_task;
        Runnable run_main_and_shutdown;

        run_main_and_shutdown = new Runnable() {
            public void run() {
                boolean aborted = true;

                try {
                    run_main.run();
                    aborted = false;
                } finally {
                    shutdown(aborted);
                }
            }
        };
        main_task = new Task(run_main_and_shutdown);

        tasks_lock.lock();
        try {
            if (running) {
                throw new ThreadPoolStateError("ThreadPool is already running");
            }
            running = true;
        } finally {
            tasks_lock.unlock();
        }

        startupInitialThreads();
        submit(main_task);
        try {
            // This thread (the primordial thread) operates the thread pool
            // until the program is finished
            run();
            if (!shutdown_abort) {
                jmercury.runtime.JavaInternal.run_finalisers();
            }
            // We always wait for the thread pool to shutdown as worker threads
            // may either be completing work or reporting the reason why
            // the runtime is aborting.
            waitForShutdown();
        } catch (jmercury.runtime.Exception e) {
            JavaInternal.reportUncaughtException(e);
        }
    }

    /**
     * This class creates and names Mercury threads.
     * The factory is responsible for creating threads with unique IDs.
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

    /**
     * The thread pool is in the wrong state for the action the caller tried
     * to perform.
     */
    public static class ThreadPoolStateError extends Exception {
        public ThreadPoolStateError(String message) {
            super(message);
        }
    }
}
