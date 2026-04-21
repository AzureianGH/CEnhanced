#include "frontend_impl.h"

#include "includes.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  Parser *parser;
  SemaContext *sema;
  Node *unit;
} CEFrontendUnit;

static int ends_with_icase(const char *s, const char *suf)
{
  if (!s || !suf)
    return 0;
  size_t ls = strlen(s);
  size_t lu = strlen(suf);
  if (lu > ls)
    return 0;
  const char *p = s + (ls - lu);
  for (size_t i = 0; i < lu; ++i)
  {
    unsigned char a = (unsigned char)p[i];
    unsigned char b = (unsigned char)suf[i];
    if (tolower(a) != tolower(b))
      return 0;
  }
  return 1;
}

static int ce_supports_input_path(const char *input_path)
{
  return input_path &&
         (ends_with_icase(input_path, ".ce") ||
          ends_with_icase(input_path, ".ceproj"));
}

static CEFrontendUnit *ce_unit_from_frontend_unit(FrontendUnit *unit)
{
  if (!unit || !unit->impl || unit->frontend != chance_frontend_ce())
    return NULL;
  return (CEFrontendUnit *)unit->impl;
}

static const CEFrontendUnit *ce_unit_from_frontend_unit_const(const FrontendUnit *unit)
{
  if (!unit || !unit->impl || unit->frontend != chance_frontend_ce())
    return NULL;
  return (const CEFrontendUnit *)unit->impl;
}

static void ce_discard_semantic_state(FrontendUnit *unit)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce)
    return;
  if (ce->sema)
  {
    sema_destroy(ce->sema);
    ce->sema = NULL;
  }
  if (ce->parser)
  {
    parser_destroy(ce->parser);
    ce->parser = NULL;
  }
}

static FrontendUnit *ce_load_unit(const ChanceFrontend *frontend,
                                  const ChanceFrontendLoadRequest *request,
                                  int keep_semantic_state)
{
  (void)frontend;
  if (!request)
    return NULL;

  FrontendUnit *unit = (FrontendUnit *)calloc(1, sizeof(FrontendUnit));
  CEFrontendUnit *ce = (CEFrontendUnit *)calloc(1, sizeof(CEFrontendUnit));
  if (!unit || !ce)
  {
    free(unit);
    free(ce);
    return NULL;
  }

  ce->parser = parser_create(request->source);
  ce->sema = sema_create();
  if (!ce->parser || !ce->sema)
  {
    if (ce->sema)
      sema_destroy(ce->sema);
    if (ce->parser)
      parser_destroy(ce->parser);
    free(ce);
    free(unit);
    return NULL;
  }

  chance_process_includes_and_scan(request->input_path,
                                   request->raw_source,
                                   request->raw_source_len,
                                   request->include_dirs,
                                   request->include_dir_count,
                                   ce->sema->syms);

  ce->unit = parse_unit(ce->parser);
  parser_export_externs(ce->parser, ce->sema->syms);

  unit->frontend = chance_frontend_ce();
  unit->impl = ce;

  if (!keep_semantic_state)
    ce_discard_semantic_state(unit);

  return unit;
}

static Node *ce_unit_ast(FrontendUnit *unit)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  return ce ? ce->unit : NULL;
}

static const Node *ce_unit_ast_const(const FrontendUnit *unit)
{
  const CEFrontendUnit *ce = ce_unit_from_frontend_unit_const(unit);
  return ce ? ce->unit : NULL;
}

static SymTable *ce_unit_symtab(FrontendUnit *unit)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce || !ce->sema)
    return NULL;
  return ce->sema->syms;
}

static int ce_unit_check(FrontendUnit *unit)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce || !ce->sema || !ce->unit)
    return 1;
  return sema_check_unit(ce->sema, ce->unit);
}

static void ce_register_foreign_symbols(FrontendUnit *target,
                                        const FrontendUnit *foreign)
{
  CEFrontendUnit *dst = ce_unit_from_frontend_unit(target);
  const CEFrontendUnit *src = ce_unit_from_frontend_unit_const(foreign);
  if (!dst || !dst->sema || !dst->unit || !src || !src->unit)
    return;
  sema_register_foreign_unit_symbols(dst->sema, dst->unit, src->unit);
}

static const Symbol *ce_unit_externs(FrontendUnit *unit, int *count)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce || !ce->parser)
  {
    if (count)
      *count = 0;
    return NULL;
  }
  return parser_get_externs(ce->parser, count);
}

static Symbol *ce_copy_imported_function_symbols(const FrontendUnit *unit,
                                                 int *out_count)
{
  const CEFrontendUnit *ce = ce_unit_from_frontend_unit_const(unit);
  if (!ce || !ce->sema)
  {
    if (out_count)
      *out_count = 0;
    return NULL;
  }
  return sema_copy_imported_function_symbols(ce->sema, out_count);
}

static Symbol *ce_copy_imported_global_symbols(const FrontendUnit *unit,
                                               int *out_count)
{
  const CEFrontendUnit *ce = ce_unit_from_frontend_unit_const(unit);
  if (!ce || !ce->sema)
  {
    if (out_count)
      *out_count = 0;
    return NULL;
  }
  return sema_copy_imported_global_symbols(ce->sema, out_count);
}

static void ce_track_imported_function(FrontendUnit *unit,
                                       const char *name,
                                       const char *module_full,
                                       const Symbol *symbol)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce || !ce->sema || !name || !module_full || !symbol)
    return;
  sema_track_imported_function(ce->sema, name, module_full, symbol);
}

static void ce_destroy_unit(FrontendUnit *unit)
{
  CEFrontendUnit *ce = ce_unit_from_frontend_unit(unit);
  if (!ce)
  {
    free(unit);
    return;
  }
  if (ce->sema)
    sema_destroy(ce->sema);
  if (ce->unit)
    ast_free(ce->unit);
  if (ce->parser)
    parser_destroy(ce->parser);
  free(ce);
  free(unit);
}

static const ChanceFrontendOps ce_frontend_ops = {
    .load_unit = ce_load_unit,
    .discard_semantic_state = ce_discard_semantic_state,
    .unit_ast = ce_unit_ast,
    .unit_ast_const = ce_unit_ast_const,
    .unit_symtab = ce_unit_symtab,
    .unit_check = ce_unit_check,
    .register_foreign_symbols = ce_register_foreign_symbols,
    .unit_externs = ce_unit_externs,
    .copy_imported_function_symbols = ce_copy_imported_function_symbols,
    .copy_imported_global_symbols = ce_copy_imported_global_symbols,
    .track_imported_function = ce_track_imported_function,
    .supports_direct_ccb_emit = NULL,
    .emit_ccb = NULL,
    .destroy_unit = ce_destroy_unit,
};

static const ChanceFrontend ce_frontend = {
    .name = "chance",
    .description = "CHance language frontend (.ce)",
    .supports_input_path = ce_supports_input_path,
    .ops = &ce_frontend_ops,
};

const ChanceFrontend *chance_frontend_ce(void)
{
  return &ce_frontend;
}
