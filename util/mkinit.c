/////////////////////////////////////////////////////////////////////////////
// vim:sw=4 ts=4 expandtab
//
// Copyright (C) 1995-2008, 2010-2012 The University of Melbourne.
// This file may only be copied under the terms of the GNU General
// Public License - see the file COPYING in the Mercury distribution.

// File: mkinit.c
// Main authors: zs, fjh
//
// Given a list of .c or .init files on the command line, this program
// produces the initialization file (usually called *_init.c) on stdout.
// The initialization file is a small C program that calls the initialization
// functions for all the modules in a Mercury program.
//
// Alternatively, if invoked with the -k option, this program produces a list
// of initialization directives on stdout. This mode of operation is used
// when building .init files for libraries.
//
// If invoked with the -s option, this program produces a standalone runtime
// interface on stdout. This mode of operation is used when using Mercury
// libraries from applications written in foreign languages.
//
// NOTE: any changes to this program may need to be reflected in the
// following places:
//
//      - scripts/c2init.in
//      - compiler/compile_target_code.m
//          in particular the predicates make_init_obj/7 and
//          make_standalone_interface/3.

/////////////////////////////////////////////////////////////////////////////

// mercury_std.h includes mercury_regs.h, and must precede system headers.
#include    "mercury_conf.h"
#include    "mercury_std.h"
#include    "mercury_array_macros.h"
#include    "mkinit_common.h"

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <ctype.h>
#include    <errno.h>

#ifdef MR_HAVE_SYS_STAT_H
  #include  <sys/stat.h>
#endif

#ifdef MR_HAVE_UNISTD_H
  #include  <unistd.h>
#else
  #include  "getopt.h"
#endif

// Adjustable limits.
#define MAXCALLS    40      // maximum number of calls per function

// These are used to collect lists of strings.

static const char if_need_to_init[] =
    "#if defined(MR_MAY_NEED_INITIALIZATION)\n";

static const char if_need_term_size[] =
    "#if defined(MR_RECORD_TERM_SIZES)\n";

static const char if_need_deep_prof[] =
    "#if defined(MR_DEEP_PROFILING)\n";

static const char if_need_threadscope[] =
    "#if defined(MR_THREADSCOPE)\n";

typedef enum
{
    TASK_OUTPUT_INIT_PROG = 0,
    TASK_OUTPUT_LIB_INIT  = 1,
    TASK_OUTPUT_STANDALONE_INIT = 2
} Task;

typedef enum
{
    PURPOSE_INIT = 0,
    PURPOSE_TYPE_TABLE = 1,
    PURPOSE_DEBUGGER = 2,
    PURPOSE_COMPLEXITY = 3,
    PURPOSE_PROC_STATIC = 4,
    PURPOSE_THREADSCOPE_STRING_TABLE = 5,
    PURPOSE_REQ_INIT = 6,
    PURPOSE_REQ_FINAL = 7
} Purpose;

const char  *main_func_name[] =
{
    "init_modules",
    "init_modules_type_tables",
    "init_modules_debugger",
    "init_modules_complexity_procs",
    "write_out_proc_statics",
    "init_modules_threadscope_string_table",
    "init_modules_required",
    "final_modules_required"
};

const char  *module_suffix[] =
{
    "init",
    "init_type_tables",
    "init_debugger",
    "init_complexity_procs",
    "write_out_proc_statics",
    "init_threadscope_string_table",
    "",
    "",
};

const char  *init_suffix[] =
{
    "",
    "_type_tables",
    "_debugger",
    "_complexity",
    "write_out_proc_statics",
    "",
    "",
    ""
};

const char  *bunch_function_guard[] =
{
    if_need_to_init,
    NULL,
    if_need_to_init,
    if_need_term_size,
    if_need_deep_prof,
    if_need_threadscope,
    NULL,
    NULL,
};

const char  *main_func_guard[] =
{
    NULL,
    NULL,
    NULL,
    if_need_term_size,
    if_need_deep_prof,
    if_need_threadscope,
    NULL,
    NULL,
};

const char  *main_func_body_guard[] =
{
    if_need_to_init,
    NULL,
    if_need_to_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

const char  *main_func_arg_defn[] =
{
    "void",
    "void",
    "void",
    "void",
    "FILE *deep_fp, FILE *procrep_fp",
    "void",
    "void",
    "void"
};

const char  *main_func_arg_decl[] =
{
    "void",
    "void",
    "void",
    "void",
    "FILE *, FILE *",
    "void",
    "void",
    "void"
};

const char  *main_func_arg[] =
{
    "",
    "",
    "",
    "",
    "deep_fp, procrep_fp",
    "",
    "",
    ""
};

// Macros.

#define SYS_PREFIX_1    "sys_init"
#define SYS_PREFIX_2    "mercury_sys_init"

#define matches_prefix(s, prefix)                       \
            (strncmp((s), (prefix), sizeof(prefix)-1) == 0)

#define sys_init_prefix(s)                              \
            ( matches_prefix(s, SYS_PREFIX_1) ||        \
              matches_prefix(s, SYS_PREFIX_2) )

// Global variables.

// List of names of the modules to call all the usual initialization
// functions for: "init", "init_type_tables", "init_debugger" and (with
// the right #defines) "init_complexity_procs" and "write_out_proc_statics".

static const char   **std_modules = NULL;
static int          std_module_max = 0;
static int          std_module_next = 0;
#define MR_INIT_STD_MODULE_SIZE     100

// List of names of handwritten modules, for which we call a limited set
// of initialization functions: "init", "init_type_tables" and (with
// the right #defines) "write_out_proc_statics". We don't call
// "init_debugger" functions since handwritten modules don't have module
// layouts, and we don't generate "init_complexity_procs" since they have
// no Mercury code to measure the complexity of.

static const char   **special_modules = NULL;
static int          special_module_max = 0;
static int          special_module_next = 0;
#define MR_INIT_SPECIAL_MODULE_SIZE     10

// The concatenation of std_modules and special_modules; created with the
// right size (std_module_next + special_module_next).
static const char   **std_and_special_modules = NULL;

// List of names of modules that have initialization functions that should
// always be run. This is currently used to initialize the states of constraint
// solvers. We call an "init_required" function for each such module.
static const char   **req_init_modules = NULL;
static int          req_init_module_max = 0;
static int          req_init_module_next = 0;
#define MR_INIT_REQ_MODULE_SIZE     10

// List of names of modules that have finalisation functions that should
// always be run. We call a "final_required" function for each such module.
static const char   **req_final_modules = NULL;
static int          req_final_module_max = 0;
static int          req_final_module_next = 0;
#define MR_FINAL_REQ_MODULE_SIZE    10

// List of names of environment variables whose values should be sampled
// at initialization. The list is not ordered, but must not contain duplicates,
// since that would lead to duplicate definitions of C global variables and
// hence errors from the C compiler.
static const char   **mercury_env_vars = NULL;
static int          mercury_env_var_max = 0;
static int          mercury_env_var_next = 0;
#define MR_ENV_VAR_LIST_SIZE    10

// This should be kept in sync with the code of c_global_var_name
// in llds_out.m and global_var_name in mlds_to_c.m.
const char  *envvar_prefix = "mercury_envvar_";

// Options and arguments, set by parse_options().
static const char   *output_file_name = NULL;
static const char   *entry_point = "mercury__main_2_0";
static const char   *hl_entry_point = "main_2_p_0";
static const char   *grade = "";
static int          maxcalls = MAXCALLS;
static MR_bool      output_main_func = MR_TRUE;
static MR_bool      need_initialization_code = MR_FALSE;
static MR_bool      need_tracing = MR_FALSE;
static Task         output_task = TASK_OUTPUT_INIT_PROG;
static const char   *experimental_complexity = NULL;

static int          num_experimental_complexity_procs = 0;

// List of options to pass to the runtime.
static String_List  *runtime_flags = NULL;

// Pointer to tail of the runtime_flags list.
static String_List  **runtime_flags_tail = &runtime_flags;

// List of functions to always execute at initialization.
static String_List  *always_exec_funcs = NULL;

// Pointer to tail of the always_exec_funcs list.
static String_List  **always_exec_funcs_tail = &always_exec_funcs;

// Code fragments to put in the output file.
static const char header1[] =
    "/*\n"
    "** This code was automatically generated by mkinit - do not edit.\n"
    "**\n"
    "** Grade: %s\n"
    "** Input files:\n"
    "**\n"
    ;

// NOTE: _DEFAULT_SOURCE is defined in order to suppress a warning about
// _BSD_SOURCE being deprecated in glibc 2.20. We keep the definition of
// the deprecated macro about for compatibility with older versions of glibc.
static const char header2[] =
    "*/\n"
    "#ifndef _DEFAULT_SOURCE\n"
    "  #define _DEFAULT_SOURCE\n"
    "#endif\n"
    "#ifndef _BSD_SOURCE\n"
    "  #define _BSD_SOURCE\n"
    "#endif\n"
    "#include <stddef.h>\n"
    "#ifdef MR_PROFILE_SBRK\n"
    "#include <unistd.h>\n"
    "#endif\n"
    "#include \"mercury_init.h\"\n"
    "#include \"mercury_grade.h\"\n"
    "#ifdef MR_WIN32\n"
    "  #include \"mercury_windows.h\"\n"
    "#endif\n"
    "\n"
    "#define MR_TRACE_ENABLED %d\n"
    "#if MR_TRACE_ENABLED\n"
    "  #define MR_MAY_NEED_INITIALIZATION\n"
    "#endif\n"
    "\n"
    "// Work around a bug in the Solaris 2.X (X<=4) linker;\n"
    "// on these machines, init_gc must be statically linked.\n"
    "\n"
    "#ifdef MR_CONSERVATIVE_GC\n"
    "static void init_gc(void)\n"
    "{\n"
    "    GC_INIT();\n"
    "}\n"
    "#endif\n"
    ;

static const char mercury_funcs1[] =
    "\n"
    "#ifdef MR_HIGHLEVEL_CODE\n"
    "  extern void MR_CALL %s(void);\n"
    "#else\n"
    "  MR_declare_entry(%s);\n"
    "#endif\n"
    "\n"
    "#if defined(MR_USE_DLLS)\n"
    "  #if !defined(libmer_DEFINE_DLL)\n"
    "        #define libmer_impure_ptr \\\n"
    "        (*__imp_libmer_impure_ptr)\n"
    "    extern void *libmer_impure_ptr;\n"
    "  #endif\n"
    "  #if !defined(libmercury_DEFINE_DLL)\n"
    "        #define libmercury_impure_ptr \\\n"
    "        (*__imp_libmercury_impure_ptr)\n"
    "    extern void *libmercury_impure_ptr;\n"
    "  #endif\n"
    "#endif\n"
    "\n"
    "void\n"
    "mercury_init(int argc, char **argv, void *stackbottom)\n"
    "{\n"
    "\n"
    "#if defined(MR_CONSERVATIVE_GC)\n"
    "    // Explicitly register the bottom of the stack, so that the\n"
    "    // GC knows where it starts.\n"
    "    //\n"
    "    // Boehm GC 7.4.2 deprecates the use of GC_stackbottom and\n"
    "    // prohibits the use of the alternative GC_register_my_thread()\n"
    "    // for the primordial thread. Therefore we do not attempt to\n"
    "    // register the bottom of the C stack but let the collector\n"
    "    // determine an appropriate value itself.\n"
    "    #if defined(MR_HGC)\n"
    "      MR_hgc_set_stack_bot(stackbottom);\n"
    "    #endif\n"
    "#endif\n"
    "\n"
    "// If we are using DLLs on gnu-win32, then we need to take\n"
    "// special steps to initialize _impure_ptr for the DLLs.\n"
    "#if defined(MR_USE_DLLS)\n"
    "  #if !defined(libmer_DEFINE_DLL)\n"
    "    libmer_impure_ptr = _impure_ptr;\n"
    "  #endif\n"
    "  #if !defined(libmercury_DEFINE_DLL)\n"
    "    libmercury_impure_ptr = _impure_ptr;\n"
    "  #endif\n"
    "#endif\n"
    "\n";

static const char mercury_funcs2[] =
    "    MR_address_of_mercury_init_io = mercury_init_io;\n"
    "    MR_address_of_init_modules = init_modules;\n"
    "    MR_address_of_init_modules_type_tables = init_modules_type_tables;\n"
    "    MR_address_of_init_modules_debugger = init_modules_debugger;\n"
    "#ifdef MR_RECORD_TERM_SIZES\n"
    "    MR_address_of_init_modules_complexity =\n"
    "        init_modules_complexity_procs;\n"
    "#endif\n"
    "#ifdef MR_DEEP_PROFILING\n"
    "    MR_address_of_write_out_proc_statics =\n"
    "        write_out_proc_statics;\n"
    "#endif\n"
    "#ifdef MR_THREADSCOPE\n"
    "    MR_address_of_init_modules_threadscope_string_table =\n"
    "        init_modules_threadscope_string_table;\n"
    "#endif\n"
    "    MR_address_of_init_modules_required = init_modules_required;\n"
    "    MR_address_of_final_modules_required = final_modules_required;\n"
    "#ifdef MR_RECORD_TERM_SIZES\n"
    "    MR_complexity_procs = MR_complexity_proc_table;\n"
    "    MR_num_complexity_procs = %d;\n"
    "#endif\n"
    "    MR_type_ctor_info_for_univ = ML_type_ctor_info_for_univ;\n"
    "    MR_type_info_for_type_info = (MR_TypeCtorInfo)\n"
    "        &ML_type_info_for_type_info;\n"
    "    MR_type_info_for_pseudo_type_info = (MR_TypeCtorInfo)\n"
    "        &ML_type_info_for_pseudo_type_info;\n"
    "    MR_type_info_for_list_of_univ = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_univ;\n"
    "    MR_type_info_for_list_of_int = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_int;\n"
    "    MR_type_info_for_list_of_char = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_char;\n"
    "    MR_type_info_for_list_of_string = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_string;\n"
    "    MR_type_info_for_list_of_type_info = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_type_info;\n"
    "    MR_type_info_for_list_of_pseudo_type_info = (MR_TypeInfo)\n"
    "        &ML_type_info_for_list_of_pseudo_type_info;\n"
    "#ifdef MR_CONSERVATIVE_GC\n"
    "    MR_address_of_init_gc = init_gc;\n"
    "#endif\n"
    "    MR_library_initializer = ML_std_library_init;\n"
    "    MR_library_finalizer = ML_std_library_finalize;\n"
    "    MR_io_stdin_stream = ML_io_stdin_stream;\n"
    "    MR_io_stdout_stream = ML_io_stdout_stream;\n"
    "    MR_io_stderr_stream = ML_io_stderr_stream;\n"
    "    MR_io_print_to_stream = ML_io_print_to_stream;\n"
    "#if MR_TRACE_ENABLED\n"
    "    MR_exec_trace_func_ptr = MR_trace_real;\n"
    "    MR_register_module_layout = MR_register_module_layout_real;\n"
    "    MR_address_of_trace_getline = MR_trace_getline;\n"
    "    MR_address_of_trace_get_command = MR_trace_get_command;\n"
    "    MR_address_of_trace_browse_all_on_level =\n"
    "        MR_trace_browse_all_on_level;\n"
    "    MR_address_of_trace_interrupt_handler =\n"
    "        MR_trace_interrupt_handler;\n"
    "  #ifdef MR_USE_EXTERNAL_DEBUGGER\n"
    "    MR_address_of_trace_init_external = MR_trace_init_external;\n"
    "    MR_address_of_trace_final_external = MR_trace_final_external;\n"
    "  #endif\n"
    "#else\n"
    "    MR_exec_trace_func_ptr = MR_trace_fake;\n"
    "    MR_register_module_layout = NULL;\n"
    "    MR_address_of_trace_getline = NULL;\n"
    "    MR_address_of_trace_get_command = NULL;\n"
    "    MR_address_of_trace_browse_all_on_level = NULL;\n"
    "    MR_address_of_trace_interrupt_handler = NULL;\n"
    "  #ifdef MR_USE_EXTERNAL_DEBUGGER\n"
    "    MR_address_of_trace_init_external = NULL;\n"
    "    MR_address_of_trace_final_external = NULL;\n"
    "  #endif\n"
    "#endif\n"
    "#if defined(MR_USE_GCC_NONLOCAL_GOTOS) && !defined(MR_USE_ASM_LABELS)\n"
    "    MR_do_init_modules();\n"
    "#endif\n"
    "#ifdef MR_HIGHLEVEL_CODE\n"
    "    MR_program_entry_point = %s;\n"
    "#else\n"
    "    MR_program_entry_point = MR_ENTRY(%s);\n"
    "#endif\n"
    ;

static const char mercury_funcs3[] =
    "\n"
    "    mercury_runtime_init(argc, argv);\n"
    "\n"
    ;

static const char mercury_funcs4[] =
    "    return;\n"
    "}\n"
    "\n"
    ;

static const char mercury_call_main_func[] =
    "void\n"
    "mercury_call_main(void)\n"
    "{\n"
    "    mercury_runtime_main();\n"
    "}\n"
    "\n"
    ;

static const char mercury_terminate_func[] =
    "int\n"
    "mercury_terminate(void)\n"
    "{\n"
    "    return mercury_runtime_terminate();\n"
    "}\n"
    "\n"
    ;

static const char mercury_main_func[] =
    "int\n"
    "mercury_main(int argc, char **argv)\n"
    "{\n"
    "#ifdef MR_PROFILE_SBRK\n"
    "    void* old_break;\n"
    "    void* new_break;\n"
    "#endif\n"
    "    int result;\n"
    "#ifdef MR_PROFILE_SBRK\n"
    "    old_break = sbrk(0);\n"
    "#endif\n"
        // Note that the address we use for the stack base needs to be
        // word-aligned. That is why we give dummy the type `void *'
        // rather than e.g. `char'.
    "    void *dummy;\n"
    "    mercury_init(argc, argv, &dummy);\n"
    "    mercury_call_main();\n"
    "    result = mercury_terminate();\n"
    "#ifdef MR_PROFILE_SBRK\n"
    "    new_break = sbrk(0);\n"
    "    printf(\"sbrk delta: %d\\n\",\n"
    "        (MR_Integer) new_break - (MR_Integer) old_break);\n"
    "#endif\n"
    "    return result;\n"
    "}\n"
    "\n"
    // Convert wide-character representation of the command line
    // arguments to UTF-8.
    "#if defined(MR_MSVC)\n"
    "static char **\n"
    "mercury_msvc_args(int argc, wchar_t **wargv)\n"
    "{\n"
    "    char    **argv;\n"
    "    int     i;\n"
    "\n"
    "    argv = MR_NEW_ARRAY(char *, argc + 1);\n"
    "    for (i = 0; i < argc; i++) {\n"
    "        int bytes = WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,\n"
    "            NULL, 0, NULL, NULL);\n"
    "        argv[i] = MR_malloc(bytes);\n"
    "        WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,\n"
    "            argv[i], bytes, NULL, NULL);\n"
    "    }\n"
    "    argv[i] = NULL;\n"
    "    return argv;\n"
    "}\n"
    "#elif defined(MR_MINGW)\n"
    "extern int __wgetmainargs(int *, wchar_t ***, wchar_t ***, int, int *);\n"
    "\n"
    "static void\n"
    "mercury_win32_args(int *argc_ret, char ***argv_ret)\n"
    "{\n"
    "    int     argc;\n"
    "    char    **argv;\n"
    "    wchar_t **wargv;\n"
    "    wchar_t **wenv;\n"
    "    int     start_info = 0;\n"
    "    int     i;\n"
    "\n"
    "    if (__wgetmainargs(&argc, &wargv, &wenv, 0, &start_info) != 0) {\n"
    "        return;\n"
    "    }\n"
    "    argv = MR_NEW_ARRAY(char *, argc + 1);\n"
    "    for (i = 0; i < argc; i++) {\n"
    "        int bytes = WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,\n"
    "            NULL, 0, NULL, NULL);\n"
    "        argv[i] = MR_malloc(bytes);\n"
    "        WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,\n"
    "            argv[i], bytes, NULL, NULL);\n"
    "    }\n"
    "    argv[i] = NULL;\n"
    "    *argc_ret = argc;\n"
    "    *argv_ret = argv;\n"
    "}\n"
    "#endif\n"
    "\n"
    ;

static const char mercury_grade_check_func[] =
    "// Ensure that everything gets compiled in the same grade.\n"
    "const char *mercury_init_grade_check(void)\n"
    "{\n"
    "    return &MR_GRADE_VAR;\n"
    "}\n"
    "\n"
    ;

static const char main_func[] =
    "#if defined(MR_MSVC)\n"
    "\n"
    "int\n"
    "wmain(int argc, wchar_t **wargv)\n"
    "{\n"
    "    int    result;\n"
    "    char   **argv;\n"
    "\n"
    "    argv = mercury_msvc_args(argc, wargv);\n"
    "    result = mercury_main(argc, argv);\n"
    "    return result;\n"
    "}\n"
    "#else\n"
    "\n"
    "int\n"
    "main(int argc, char **argv)\n"
    "{\n"
    "#ifdef MR_MINGW\n"
    "    mercury_win32_args(&argc, &argv);\n"
    "#endif\n"
    "    return mercury_main(argc, argv);\n"
    "}\n"
    "#endif\n"
    ;

// Function prototypes.
static  void    parse_options(int argc, char *argv[]);
static  void    usage(void);
static  void    output_complexity_proc(const char *procname);
static  void    output_complexity_experiment_table(const char *filename);
static  void    output_headers(void);
static  int     output_sub_init_functions(Purpose purpose,
                    const char **func_names, int num_func_names);
static  void    output_main_init_function(Purpose purpose, int num_bunches);
static  void    output_main(void);
static  int     output_lib_init_file(void);
static  int     output_init_program(void);
static  void    process_file(const char *filename);
static  void    process_init_file(const char *filename);
static  void    output_init_function(const char *func_name,
                    int *num_bunches_ptr, int *num_calls_in_cur_bunch_ptr,
                    Purpose purpose);

/////////////////////////////////////////////////////////////////////////////

#ifdef  CHECK_GET_LINE
FILE    *check_fp;
#endif

int
main(int argc, char **argv)
{
    int exit_status;

    MR_progname = argv[0];

    parse_options(argc, argv);

#ifdef  CHECK_GET_LINE
    check_fp = fopen(".check_get_line", "w");
    // If the open fails, we won't write to the file.
#endif

    set_output_file(output_file_name);

    switch (output_task) {
        case TASK_OUTPUT_LIB_INIT:
            // Output a .init file.
            exit_status = output_lib_init_file();
            break;

        case TASK_OUTPUT_STANDALONE_INIT:
        case TASK_OUTPUT_INIT_PROG:
            // Output a _init.c file or a standalone initialisation interface.
            exit_status = output_init_program();
            break;

        default:
            fprintf(stderr, "mkinit: unknown task\n");
            exit(EXIT_FAILURE);
    }

    return exit_status;
}

/////////////////////////////////////////////////////////////////////////////

// Output the initialisation file for a Mercury library, the .init file.

static int
output_lib_init_file(void)
{
    int filenum;
    int i;

    for (filenum = 0; filenum < num_files; filenum++) {
            process_file(files[filenum]);
    }

    for (i = 0; i < std_module_next; i++) {
        printf("INIT %s%s\n", std_modules[i], module_suffix[PURPOSE_INIT]);
    }

    for (i = 0; i < req_init_module_next; i++) {
        printf("REQUIRED_INIT %s\n", req_init_modules[i]);
    }

    for (i = 0; i < req_final_module_next; i++) {
        printf("REQUIRED_FINAL %s\n", req_final_modules[i]);
    }

    for (i = 0; i < mercury_env_var_next; i++) {
        printf("ENVVAR %s\n", mercury_env_vars[i]);
    }

    if (num_errors > 0) {
        fprintf(stderr, "%s: error while creating .init file.\n", MR_progname);
        return EXIT_FAILURE;
    } else {
        return EXIT_SUCCESS;
    }

}

/////////////////////////////////////////////////////////////////////////////

// Output the initialisation program for a Mercury executable, the *_init.c
// file.

static int
output_init_program(void)
{
    int filenum;
    int num_bunches;
    int i;

    do_path_search(files, num_files);
    output_headers();

    for (filenum = 0; filenum < num_files; filenum++) {
        process_file(files[filenum]);
    }

    printf("\n");
    for (i = 0; i < mercury_env_var_next; i++) {
        printf("MR_Word %s%s = 0;\n", envvar_prefix, mercury_env_vars[i]);
    }

    if (need_initialization_code) {
        printf("#define MR_MAY_NEED_INITIALIZATION\n\n");
    }

    std_and_special_modules = MR_NEW_ARRAY(const char *,
        std_module_next + special_module_next);

    for (i = 0; i < std_module_next; i++) {
        std_and_special_modules[i] = std_modules[i];
    }

    for (i = 0; i < special_module_next; i++) {
        std_and_special_modules[std_module_next + i] = special_modules[i];
    }

    num_bunches = output_sub_init_functions(PURPOSE_INIT,
        std_and_special_modules, std_module_next + special_module_next);
    output_main_init_function(PURPOSE_INIT, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_TYPE_TABLE,
        std_and_special_modules, std_module_next + special_module_next);
    output_main_init_function(PURPOSE_TYPE_TABLE, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_DEBUGGER,
        std_modules, std_module_next);
    output_main_init_function(PURPOSE_DEBUGGER, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_COMPLEXITY,
        std_modules, std_module_next);
    output_main_init_function(PURPOSE_COMPLEXITY, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_PROC_STATIC,
        std_and_special_modules, std_module_next + special_module_next);
    output_main_init_function(PURPOSE_PROC_STATIC, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_THREADSCOPE_STRING_TABLE,
        std_modules, std_module_next);
    output_main_init_function(PURPOSE_THREADSCOPE_STRING_TABLE, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_REQ_INIT,
        req_init_modules, req_init_module_next);
    output_main_init_function(PURPOSE_REQ_INIT, num_bunches);

    num_bunches = output_sub_init_functions(PURPOSE_REQ_FINAL,
        req_final_modules, req_final_module_next);
    output_main_init_function(PURPOSE_REQ_FINAL, num_bunches);

    output_main();

    if (num_errors > 0) {
        fputs("// Force syntax error, since there were errors\n", stdout);
        fputs("// in the generation of this file.", stdout);
        fputs("#error \"You need to remake this file\"\n", stdout);
        if (output_file_name != NULL) {
            (void) fclose(stdout);
            (void) remove(output_file_name);
        }
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////////////////////////

static void
parse_options(int argc, char *argv[])
{
    int         c;
    String_List *tmp_slist;
    int         seen_f_option = 0;

    while ((c = getopt(argc, argv, "A:c:f:g:iI:l:o:r:tw:xX:ks")) != EOF) {
        switch (c) {
        case 'A':
            // Add the argument to the end of the list of always executed
            // initialization functions.
            if (optarg[0] != '\0') {
                tmp_slist = (String_List *)
                    checked_malloc(sizeof(String_List));
                tmp_slist->next = NULL;
                tmp_slist->data = (char *) checked_malloc(strlen(optarg) + 1);
                strcpy(tmp_slist->data, optarg);
                *always_exec_funcs_tail = tmp_slist;
                always_exec_funcs_tail = &tmp_slist->next;
            }
            break;

        case 'c':
            if (sscanf(optarg, "%d", &maxcalls) != 1) {
                usage();
            }

            break;

        case 'f':
            process_file_list_file(optarg);
            seen_f_option = 1;
            break;

        case 'g':
            grade = optarg;
            break;

        case 'i':
            need_initialization_code = MR_TRUE;
            break;

        case 'I':
            add_init_file_dir(optarg);
            break;

        case 'l':
            output_main_func = MR_FALSE;
            break;

        case 'o':
            if (strcmp(optarg, "-") == 0) {
                output_file_name = NULL; // output to stdout
            } else {
                output_file_name = optarg;
            }
            break;

        case 'r':
            // Add the argument to the end of the list of runtime flags.
            if (optarg[0] != '\0') {
                tmp_slist = (String_List *)
                    checked_malloc(sizeof(String_List));
                tmp_slist->next = NULL;
                tmp_slist->data = (char *) checked_malloc(strlen(optarg) + 1);
                strcpy(tmp_slist->data, optarg);
                *runtime_flags_tail = tmp_slist;
                runtime_flags_tail = &tmp_slist->next;
            }
            break;

        case 't':
            need_tracing = MR_TRUE;
            need_initialization_code = MR_TRUE;
            break;

        case 'w':
            hl_entry_point = entry_point = optarg;
            break;

        case 'x':
            // We always assume this option.
            break;

        case 'X':
            experimental_complexity = optarg;
            break;

        case 'k':
            output_task = TASK_OUTPUT_LIB_INIT;
            break;

        case 's':
            output_task = TASK_OUTPUT_STANDALONE_INIT;
            output_main_func = MR_FALSE; // -s implies -l
            break;

        default:
            usage();
        }
    }

    if (seen_f_option) {
        // -f could be made compatible if we copied the filenames
        // from argv into files.
        if ((argc - optind) > 0) {
            fprintf(stderr,
                "%s: -f incompatible with filenames on the command line\n",
                MR_progname);
            exit(EXIT_FAILURE);
        }
    } else {
        num_files = argc - optind;
        files = argv + optind;
    }

    if (num_files <= 0) {
        usage();
    }
}

static void
usage(void)
{
    fputs("Usage: mkinit [options] files...\n", stderr);
    fputs("Options:\n", stderr);
    fputs("  -c maxcalls:\tset the max size of an init function\n", stderr);
    fputs("  -f filename:\tprocess the files one per line in filename\n", stderr);
    fputs("  -g grade:\tset the grade of the executable\n", stderr);
    fputs("  -i:\t\tenable initialization code\n", stderr);
    fputs("  -l:\t\tdo not output main function\n", stderr);
    fputs("  -o file:\toutput to the named file\n", stderr);
    fputs("  -r word:\tadd word to the flags for the runtime\n", stderr);
    fputs("  -t:\t\tenable execution tracing\n", stderr);
    fputs("  -w entry:\tset the entry point to the given label\n", stderr);
    fputs("  -I dir:\tadd dir to the search path for init files\n", stderr);
    fputs("  -k:\t\tgenerate the .init for a library\n", stderr);
    fputs("  -s:\t\tgenerate a standalone runtime interface\n", stderr);
    fputs("  -m:\t\t(error)\n", stderr);
    exit(EXIT_FAILURE);
}

/////////////////////////////////////////////////////////////////////////////

#define MAX_PROCNAME_LEN    1024

static void
output_complexity_proc(const char *procname)
{
    printf("{ \"%s\", -1, -1, NULL,\n", procname);
    printf("MR_COMPLEXITY_IS_INACTIVE, NULL, 0, NULL, NULL },\n");
}

static void
output_complexity_experiment_table(const char *filename)
{
    const char  *procname;
    FILE        *fp;

    fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "%s: error opening file `%s': %s\n",
            MR_progname, filename, strerror(errno));
        num_errors++;
        return;
    }

    printf("\nMR_ComplexityProc MR_complexity_proc_table[] = {\n");

    num_experimental_complexity_procs = 0;
    while ((procname = read_line(filename, fp, MAX_PROCNAME_LEN)) != NULL) {
        num_experimental_complexity_procs++;
        output_complexity_proc(procname);
    }

    printf("};\n");
}

static void
output_headers(void)
{
    int filenum;

    printf(header1, grade);

    for (filenum = 0; filenum < num_files; filenum++) {
        fputs("** ", stdout);
        fputs(files[filenum], stdout);
        putc('\n', stdout);
    }

    printf(header2, need_tracing);

}

static int
output_sub_init_functions(Purpose purpose, const char **func_names,
    int num_func_names)
{
    int funcnum;
    int num_bunches;
    int num_calls_in_cur_bunch;

    fputs("\n", stdout);
    if (bunch_function_guard[purpose] != NULL) {
        fputs(bunch_function_guard[purpose], stdout);
        fputs("\n", stdout);
    }

    printf("static void %s_0(%s)\n",
        main_func_name[purpose], main_func_arg_defn[purpose]);
    fputs("{\n", stdout);

    num_bunches = 0;
    num_calls_in_cur_bunch = 0;
    for (funcnum = 0; funcnum < num_func_names; funcnum++) {
        output_init_function(func_names[funcnum], &num_bunches,
            &num_calls_in_cur_bunch, purpose);
    }

    fputs("}\n", stdout);
    if (bunch_function_guard[purpose] != NULL) {
        fputs("\n#endif\n", stdout);
    }

    return num_bunches;
}

static void
output_main_init_function(Purpose purpose, int num_bunches)
{
    int i;

    fputs("\n", stdout);
    if (main_func_guard[purpose] != NULL) {
        fputs(main_func_guard[purpose], stdout);
        fputs("\n", stdout);
    }

    printf("\nstatic void %s(%s)\n",
        main_func_name[purpose], main_func_arg_defn[purpose]);
    fputs("{\n", stdout);

    if (main_func_body_guard[purpose] != NULL) {
        fputs(main_func_body_guard[purpose], stdout);
    }

    for (i = 0; i <= num_bunches; i++) {
        printf("\t%s_%d(%s);\n",
            main_func_name[purpose], i, main_func_arg[purpose]);
    }

    if (main_func_body_guard[purpose] != NULL) {
        fputs("#endif\n", stdout);
    }

    fputs("}\n", stdout);

    if (main_func_guard[purpose] != NULL) {
        fputs("\n#endif\n", stdout);
    }
}

static void
output_main(void)
{
    String_List *list;
    char        *options_str;
    int         i;

    if (experimental_complexity != NULL) {
        output_complexity_experiment_table(experimental_complexity);
    } else {
        printf("\nMR_ComplexityProc MR_complexity_proc_table[] = {\n");
        output_complexity_proc("dummy_proc/0-0");
        printf("};\n");
    }

    // If we are building a standalone interface then we set the entry
    // point as MR_dummy_main. This is defined in runtime/mercury_wrapper.c
    // and aborts execution if called. In standalone mode, we are not
    // expecting Mercury to be called through the standard entry point.
    if (output_task == TASK_OUTPUT_STANDALONE_INIT) {
        hl_entry_point = entry_point = "MR_dummy_main";
    }

    printf(mercury_funcs1, hl_entry_point, entry_point);
    printf(mercury_funcs2, num_experimental_complexity_procs,
        hl_entry_point, entry_point);

    printf("    MR_runtime_flags = \"");
    for (list = runtime_flags; list != NULL; list = list->next) {
        for (options_str = list->data; *options_str != '\0'; options_str++) {
            if (*options_str == '\n') {
                putchar('\\');
                putchar('n');
            } else if (*options_str == '\t') {
                putchar('\\');
                putchar('t');
            } else if (*options_str == '"' ||
                    *options_str == '\\') {
                putchar('\\');
                putchar(*options_str);
            } else {
                putchar(*options_str);
            }
        }
        putchar(' ');
    }
    printf("\";\n");

    printf("\n");
    for (i = 0; i < mercury_env_var_next; i++) {
        printf("    if (getenv(\"%s\") != NULL) {\n", mercury_env_vars[i]);
        printf("        %s%s = 1;\n", envvar_prefix, mercury_env_vars[i]);
        printf("    } else {\n");
        printf("        %s%s = 0;\n", envvar_prefix, mercury_env_vars[i]);
        printf("    }\n");
    }

    fputs(mercury_funcs3, stdout);

    for (list = always_exec_funcs; list != NULL; list = list->next) {
        printf("    %s();\n", list->data);
    }

    fputs(mercury_funcs4, stdout);

    if (output_task == TASK_OUTPUT_INIT_PROG) {
        fputs(mercury_call_main_func, stdout);
    }

    fputs(mercury_terminate_func, stdout);

    if (output_task == TASK_OUTPUT_INIT_PROG) {
        fputs(mercury_main_func, stdout);
    }

    fputs(mercury_grade_check_func, stdout);

    if (output_main_func) {
        fputs(main_func, stdout);
    }
}

/////////////////////////////////////////////////////////////////////////////

static void
process_file(const char *filename)
{
    size_t len;

    len = strlen(filename);
    if (len >= 2 && strcmp(filename + len - 2, ".c") == 0) {
        process_init_file(filename);
    } else if (len >= 5 && strcmp(filename + len - 5, ".init") == 0) {
        process_init_file(filename);
    } else {
        fprintf(stderr,
            "%s: filename `%s' must end in `.c' or `.init'\n",
            MR_progname, filename);
        num_errors++;
    }
}

static void
process_init_file(const char *filename)
{
    // The strings that are supposed to be followed by other information
    // (INIT, REQUIRED_INIT, and REQUIRED_FINAL) should end with
    // the space that separates the keyword from the following data.
    // The string that is not supposed to be following by other information
    // (ENDINIT) should not have a following space, since llds_out.m and
    // mlds_to_c.m do not add that space.

    const char * const  init_str = "INIT ";
    const char * const  reqinit_str = "REQUIRED_INIT ";
    const char * const  reqfinal_str = "REQUIRED_FINAL ";
    const char * const  envvar_str = "ENVVAR ";
    const char * const  endinit_str = "ENDINIT";
    const size_t        init_strlen = strlen(init_str);
    const size_t        reqinit_strlen = strlen(reqinit_str);
    const size_t        reqfinal_strlen = strlen(reqfinal_str);
    const size_t        envvar_strlen = strlen(envvar_str);
    const size_t        endinit_strlen = strlen(endinit_str);
    char                line[MAXLINE];
    FILE                *cfile;

    cfile = fopen(filename, "r");
    if (cfile == NULL) {
        fprintf(stderr, "%s: error opening file `%s': %s\n",
            MR_progname, filename, strerror(errno));
        num_errors++;
        return;
    }

    while (get_line(cfile, line, MAXLINE) > 0) {
        if (strncmp(line, init_str, init_strlen) == 0) {
            char    *func_name;
            size_t  func_name_len;
            int     j;

            for (j = init_strlen; MR_isalnumunder(line[j]); j++) {
                // VOID
            }
            line[j] = '\0';

            func_name = line + init_strlen;
            func_name_len = strlen(func_name);
            if (MR_strneq(&func_name[func_name_len - 4], "init", 4)) {
                func_name[func_name_len - 4] = '\0';
                MR_ensure_room_for_next(std_module, const char *,
                    MR_INIT_STD_MODULE_SIZE);
                std_modules[std_module_next] = checked_strdup(func_name);
                std_module_next++;
            } else {
                MR_ensure_room_for_next(special_module, const char *,
                    MR_INIT_SPECIAL_MODULE_SIZE);
                special_modules[special_module_next] =
                    checked_strdupcat(func_name, "_");
                special_module_next++;
            }
        } else if (strncmp(line, reqinit_str, reqinit_strlen) == 0) {
            char    *func_name;
            int     j;

            for (j = reqinit_strlen; MR_isalnumunder(line[j]); j++) {
                // VOID
            }
            line[j] = '\0';

            func_name = line + reqinit_strlen;
            MR_ensure_room_for_next(req_init_module, const char *,
                MR_INIT_REQ_MODULE_SIZE);
            req_init_modules[req_init_module_next] = checked_strdup(func_name);
            req_init_module_next++;
        } else if (strncmp(line, reqfinal_str, reqfinal_strlen) == 0) {
            char    *func_name;
            int     j;

            for (j = reqfinal_strlen; MR_isalnumunder(line[j]); j++) {
                // VOID
            }
            line[j] = '\0';

            func_name = line + reqfinal_strlen;
            MR_ensure_room_for_next(req_final_module, const char *,
                MR_FINAL_REQ_MODULE_SIZE);
            req_final_modules[req_final_module_next] =
                checked_strdup(func_name);
            req_final_module_next++;
        } else if (strncmp(line, envvar_str, envvar_strlen) == 0) {
            char    *envvar_name;
            int     i;
            int     j;
            MR_bool found;

            // Check that all characters in the name of the environment
            // variable are acceptable as components of a C variable name.
            // Note that the variable name doesn't have to start with a letter
            // because the variable name has a prefix.
            for (j = envvar_strlen; MR_isalnumunder(line[j]); j++) {
                // VOID
            }

            if (line[j] != '\n') {
                printf("mkinit: error: bad environment variable name %s\n",
                    line);
            }

            line[j] = '\0';     // Overwrite the newline.

            envvar_name = line + envvar_strlen;

            // Since the number of distinct environment variables used by
            // a program is likely to be in the single digits, linear search
            // should be efficient enough.
            found = MR_FALSE;
            for (i = 0; i < mercury_env_var_next; i++) {
                if (strcmp(envvar_name, mercury_env_vars[i]) == 0) {
                    found = MR_TRUE;
                    break;
                }
            }

            if (!found) {
                MR_ensure_room_for_next(mercury_env_var, const char *,
                    MR_ENV_VAR_LIST_SIZE);
                mercury_env_vars[mercury_env_var_next] =
                    checked_strdup(envvar_name);
                mercury_env_var_next++;
            }
        } else if (strncmp(line, endinit_str, endinit_strlen) == 0) {
            break;
        }
    }

    fclose(cfile);
}

// We could in theory put all calls to e.g. <module>_init_type_tables()
// functions in a single C function in the <mainmodule>_init.c file we
// generate. However, doing so turns out to be a bad idea: it leads to large
// compilation times for the <mainmodule>_init.c files. Instead, we divide
// the calls into bunches containing at most max_calls calls, with each bunch
// contained in its own function. *num_calls_in_cur_bunch_ptr says how many
// calls the current bunch already has; *num_bunches_ptr gives the number
// of the current bunch.

static void
output_init_function(const char *func_name, int *num_bunches_ptr,
    int *num_calls_in_cur_bunch_ptr, Purpose purpose)
{
    if (*num_calls_in_cur_bunch_ptr >= maxcalls) {
        printf("}\n\n");

        (*num_bunches_ptr)++;
        *num_calls_in_cur_bunch_ptr = 0;
        printf("static void %s_%d(%s)\n",
            main_func_name[purpose], *num_bunches_ptr,
            main_func_arg_defn[purpose]);
        printf("{\n");
    }

    (*num_calls_in_cur_bunch_ptr)++;

    printf("\t{ extern void %s%s(%s);\n",
        func_name, module_suffix[purpose], main_func_arg_decl[purpose]);
    printf("\t  %s%s(%s); }\n",
        func_name, module_suffix[purpose], main_func_arg[purpose]);
}

/////////////////////////////////////////////////////////////////////////////
