#ifndef CHANCE_FRONTEND_H
#define CHANCE_FRONTEND_H

#include "ast.h"

#include <stdio.h>

typedef struct ChanceFrontend ChanceFrontend;
typedef struct FrontendUnit FrontendUnit;

typedef struct
{
  const char *input_path;
  const char *ccb_output_path;
  int opt_level;
} ChanceFrontendEmitRequest;

typedef struct
{
  const char *input_path;
  const char *raw_source;
  int raw_source_len;
  SourceBuffer source;
  char **include_dirs;
  int include_dir_count;
} ChanceFrontendLoadRequest;

const ChanceFrontend *chance_frontend_default(void);
const ChanceFrontend *chance_frontend_for_input_path(const char *input_path);
const ChanceFrontend *chance_frontend_find(const char *name);
void chance_frontend_list(FILE *out);

const char *chance_frontend_name(const ChanceFrontend *frontend);

FrontendUnit *chance_frontend_load_unit(const ChanceFrontend *frontend,
                                        const ChanceFrontendLoadRequest *request,
                                        int keep_semantic_state);
void chance_frontend_unit_discard_semantic_state(FrontendUnit *unit);

Node *chance_frontend_unit_ast(FrontendUnit *unit);
const Node *chance_frontend_unit_ast_const(const FrontendUnit *unit);
SymTable *chance_frontend_unit_symtab(FrontendUnit *unit);
int chance_frontend_unit_check(FrontendUnit *unit);

void chance_frontend_unit_register_foreign_symbols(FrontendUnit *target,
                                                   const FrontendUnit *foreign);
const Symbol *chance_frontend_unit_externs(FrontendUnit *unit, int *count);
Symbol *chance_frontend_unit_copy_imported_function_symbols(const FrontendUnit *unit,
                                                            int *out_count);
Symbol *chance_frontend_unit_copy_imported_global_symbols(const FrontendUnit *unit,
                                                          int *out_count);
void chance_frontend_unit_track_imported_function(FrontendUnit *unit,
                                                  const char *name,
                                                  const char *module_full,
                                                  const Symbol *symbol);

int chance_frontend_unit_supports_direct_ccb_emit(const FrontendUnit *unit);
int chance_frontend_unit_emit_ccb(FrontendUnit *unit,
                                  const ChanceFrontendEmitRequest *request);

void chance_frontend_unit_destroy(FrontendUnit *unit);

#endif
