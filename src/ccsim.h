#ifndef CHANCE_CCSIM_H
#define CHANCE_CCSIM_H

#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct
    {
        int opt_level;
        int aggressive;
    } CcsimOptions;

    typedef struct
    {
        size_t passes;
        size_t rewritten_load_locals;
        size_t const_folds;
        size_t collapsed_hidden_calls;
        size_t vm_collapsed_functions;
        size_t simulation_barriers;
    } CcsimStats;

    void ccsim_optimize_lines(char **lines, size_t line_count,
                              const CcsimOptions *options,
                              CcsimStats *stats);

    void ccsim_collapse_hidden_calls(char **function_lines, size_t function_line_count,
                                     char **module_lines, size_t module_line_count,
                                     const CcsimOptions *options,
                                     CcsimStats *stats);

    int ccsim_vm_collapse_hidden_function(char **function_lines, size_t function_line_count,
                                          const char *function_name,
                                          char **module_lines, size_t module_line_count,
                                          const CcsimOptions *options,
                                          CcsimStats *stats);

#ifdef __cplusplus
}
#endif

#endif
