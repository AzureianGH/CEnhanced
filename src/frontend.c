#include "frontend_impl.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static int equals_icase(const char *a, const char *b)
{
  if (!a || !b)
    return 0;
  while (*a && *b)
  {
    unsigned char ca = (unsigned char)*a;
    unsigned char cb = (unsigned char)*b;
    if (tolower(ca) != tolower(cb))
      return 0;
    ++a;
    ++b;
  }
  return *a == '\0' && *b == '\0';
}

static const ChanceFrontend *registry_entry(int i)
{
  switch (i)
  {
  case 0:
    return chance_frontend_ce();
  case 1:
    return chance_frontend_cinder();
  default:
    return NULL;
  }
}

const ChanceFrontend *chance_frontend_default(void)
{
  return registry_entry(0);
}

const ChanceFrontend *chance_frontend_for_input_path(const char *input_path)
{
  for (int i = 0;; ++i)
  {
    const ChanceFrontend *frontend = registry_entry(i);
    if (!frontend)
      break;
    if (frontend->supports_input_path && frontend->supports_input_path(input_path))
      return frontend;
  }
  return NULL;
}

const ChanceFrontend *chance_frontend_find(const char *name)
{
  if (!name || !*name)
    return NULL;
  for (int i = 0;; ++i)
  {
    const ChanceFrontend *frontend = registry_entry(i);
    if (!frontend)
      break;
    if (equals_icase(frontend->name, name))
      return frontend;
  }
  return NULL;
}

void chance_frontend_list(FILE *out)
{
  if (!out)
    out = stdout;
  for (int i = 0;; ++i)
  {
    const ChanceFrontend *frontend = registry_entry(i);
    if (!frontend)
      break;
    fprintf(out, "%s", frontend->name ? frontend->name : "unknown");
    if (frontend->description && frontend->description[0])
      fprintf(out, " - %s", frontend->description);
    fputc('\n', out);
  }
}

const char *chance_frontend_name(const ChanceFrontend *frontend)
{
  return frontend ? frontend->name : NULL;
}

FrontendUnit *chance_frontend_load_unit(const ChanceFrontend *frontend,
                                        const ChanceFrontendLoadRequest *request,
                                        int keep_semantic_state)
{
  if (!frontend || !frontend->ops || !frontend->ops->load_unit)
    return NULL;
  return frontend->ops->load_unit(frontend, request, keep_semantic_state);
}

void chance_frontend_unit_discard_semantic_state(FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->discard_semantic_state)
    return;
  unit->frontend->ops->discard_semantic_state(unit);
}

Node *chance_frontend_unit_ast(FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->unit_ast)
    return NULL;
  return unit->frontend->ops->unit_ast(unit);
}

const Node *chance_frontend_unit_ast_const(const FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->unit_ast_const)
    return NULL;
  return unit->frontend->ops->unit_ast_const(unit);
}

SymTable *chance_frontend_unit_symtab(FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->unit_symtab)
    return NULL;
  return unit->frontend->ops->unit_symtab(unit);
}

int chance_frontend_unit_check(FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->unit_check)
    return 1;
  return unit->frontend->ops->unit_check(unit);
}

void chance_frontend_unit_register_foreign_symbols(FrontendUnit *target,
                                                   const FrontendUnit *foreign)
{
  if (!target || !target->frontend || !target->frontend->ops ||
      !target->frontend->ops->register_foreign_symbols)
    return;
  target->frontend->ops->register_foreign_symbols(target, foreign);
}

const Symbol *chance_frontend_unit_externs(FrontendUnit *unit, int *count)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->unit_externs)
  {
    if (count)
      *count = 0;
    return NULL;
  }
  return unit->frontend->ops->unit_externs(unit, count);
}

Symbol *chance_frontend_unit_copy_imported_function_symbols(const FrontendUnit *unit,
                                                            int *out_count)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->copy_imported_function_symbols)
  {
    if (out_count)
      *out_count = 0;
    return NULL;
  }
  return unit->frontend->ops->copy_imported_function_symbols(unit, out_count);
}

Symbol *chance_frontend_unit_copy_imported_global_symbols(const FrontendUnit *unit,
                                                          int *out_count)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->copy_imported_global_symbols)
  {
    if (out_count)
      *out_count = 0;
    return NULL;
  }
  return unit->frontend->ops->copy_imported_global_symbols(unit, out_count);
}

void chance_frontend_unit_track_imported_function(FrontendUnit *unit,
                                                  const char *name,
                                                  const char *module_full,
                                                  const Symbol *symbol)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->track_imported_function)
    return;
  unit->frontend->ops->track_imported_function(unit, name, module_full, symbol);
}

int chance_frontend_unit_supports_direct_ccb_emit(const FrontendUnit *unit)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->supports_direct_ccb_emit)
    return 0;
  return unit->frontend->ops->supports_direct_ccb_emit(unit);
}

int chance_frontend_unit_emit_ccb(FrontendUnit *unit,
                                  const ChanceFrontendEmitRequest *request)
{
  if (!unit || !unit->frontend || !unit->frontend->ops ||
      !unit->frontend->ops->emit_ccb)
    return 1;
  return unit->frontend->ops->emit_ccb(unit, request);
}

void chance_frontend_unit_destroy(FrontendUnit *unit)
{
  if (!unit)
    return;
  if (unit->frontend && unit->frontend->ops && unit->frontend->ops->destroy_unit)
  {
    unit->frontend->ops->destroy_unit(unit);
    return;
  }
  free(unit);
}
