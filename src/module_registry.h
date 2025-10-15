#ifndef CHANCE_MODULE_REGISTRY_H
#define CHANCE_MODULE_REGISTRY_H

#include "ast.h"

void module_registry_reset(void);

void module_registry_register_struct(const char *module_full, Type *type);
void module_registry_register_enum(const char *module_full, const char *enum_name, Type *type);
void module_registry_register_enum_value(const char *module_full, const char *enum_name, const char *value_name, int value);

Type *module_registry_lookup_struct(const char *module_full, const char *type_name);
Type *module_registry_lookup_enum(const char *module_full, const char *enum_name);
int module_registry_lookup_enum_value(const char *module_full, const char *enum_name, const char *value_name, int *out_value);

Type *module_registry_canonical_type(Type *ty);
const char *module_registry_find_struct_module(const Type *type);

#endif
