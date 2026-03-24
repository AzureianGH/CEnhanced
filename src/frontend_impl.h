#ifndef CHANCE_FRONTEND_IMPL_H
#define CHANCE_FRONTEND_IMPL_H

#include "frontend.h"

typedef struct ChanceFrontendOps
{
  FrontendUnit *(*load_unit)(const ChanceFrontend *frontend,
                             const ChanceFrontendLoadRequest *request,
                             int keep_semantic_state);
  void (*discard_semantic_state)(FrontendUnit *unit);
  Node *(*unit_ast)(FrontendUnit *unit);
  const Node *(*unit_ast_const)(const FrontendUnit *unit);
  SymTable *(*unit_symtab)(FrontendUnit *unit);
  int (*unit_check)(FrontendUnit *unit);
  void (*register_foreign_symbols)(FrontendUnit *target,
                                   const FrontendUnit *foreign);
  const Symbol *(*unit_externs)(FrontendUnit *unit, int *count);
  Symbol *(*copy_imported_function_symbols)(const FrontendUnit *unit,
                                            int *out_count);
  Symbol *(*copy_imported_global_symbols)(const FrontendUnit *unit,
                                          int *out_count);
  void (*track_imported_function)(FrontendUnit *unit, const char *name,
                                  const char *module_full,
                                  const Symbol *symbol);
  int (*supports_direct_ccb_emit)(const FrontendUnit *unit);
  int (*emit_ccb)(FrontendUnit *unit,
                  const ChanceFrontendEmitRequest *request);
  void (*destroy_unit)(FrontendUnit *unit);
} ChanceFrontendOps;

struct ChanceFrontend
{
  const char *name;
  const char *description;
  int (*supports_input_path)(const char *input_path);
  const ChanceFrontendOps *ops;
};

struct FrontendUnit
{
  const ChanceFrontend *frontend;
  void *impl;
};

const ChanceFrontend *chance_frontend_ce(void);
const ChanceFrontend *chance_frontend_cinder(void);

#endif
