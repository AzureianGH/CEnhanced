#ifndef CHANCE_MANGLE_H
#define CHANCE_MANGLE_H

#include "ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    void describe_type(const Type *t, char *buf, size_t bufsz);
    char *append_param_signature(const char *base_name, const Node *fn);
    char *module_backend_name(const char *module_full, const char *fn_name, const Node *fn);

#ifdef __cplusplus
}
#endif

#endif
