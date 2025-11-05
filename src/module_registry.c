#include "module_registry.h"
#include <stdlib.h>
#include <string.h>

typedef struct
{
    char *module_full;
    char *name;
    Type *type;
} StructEntry;

typedef struct
{
    char *module_full;
    char *name;
    Type *type;
} EnumEntry;

typedef struct
{
    char *module_full;
    char *enum_name;
    char *value_name;
    int value;
} EnumValueEntry;

static StructEntry *struct_entries = NULL;
static int struct_count = 0;
static int struct_cap = 0;

static EnumEntry *enum_entries = NULL;
static int enum_count = 0;
static int enum_cap = 0;

static EnumValueEntry *enum_value_entries = NULL;
static int enum_value_count = 0;
static int enum_value_cap = 0;

static char *dup_string(const char *s)
{
    return s ? xstrdup(s) : NULL;
}

static void free_struct_entries(void)
{
    for (int i = 0; i < struct_count; ++i)
    {
        free(struct_entries[i].module_full);
        free(struct_entries[i].name);
    }
    free(struct_entries);
    struct_entries = NULL;
    struct_count = 0;
    struct_cap = 0;
}

static void free_enum_entries(void)
{
    for (int i = 0; i < enum_count; ++i)
    {
        free(enum_entries[i].module_full);
        free(enum_entries[i].name);
    }
    free(enum_entries);
    enum_entries = NULL;
    enum_count = 0;
    enum_cap = 0;
}

static void free_enum_value_entries(void)
{
    for (int i = 0; i < enum_value_count; ++i)
    {
        free(enum_value_entries[i].module_full);
        free(enum_value_entries[i].enum_name);
        free(enum_value_entries[i].value_name);
    }
    free(enum_value_entries);
    enum_value_entries = NULL;
    enum_value_count = 0;
    enum_value_cap = 0;
}

void module_registry_reset(void)
{
    free_struct_entries();
    free_enum_entries();
    free_enum_value_entries();
}

static int match_strings(const char *a, const char *b)
{
    if (!a || !b)
        return 0;
    return strcmp(a, b) == 0;
}

void module_registry_register_struct(const char *module_full, Type *type)
{
    if (!module_full || !type)
        return;
    for (int i = 0; i < struct_count; ++i)
    {
        if (match_strings(struct_entries[i].module_full, module_full) &&
            match_strings(struct_entries[i].name, type->struct_name))
        {
            struct_entries[i].type = type;
            return;
        }
    }
    if (struct_count == struct_cap)
    {
        struct_cap = struct_cap ? struct_cap * 2 : 8;
        struct_entries = (StructEntry *)realloc(struct_entries, sizeof(StructEntry) * (size_t)struct_cap);
    }
    struct_entries[struct_count].module_full = dup_string(module_full);
    struct_entries[struct_count].name = dup_string(type->struct_name);
    struct_entries[struct_count].type = type;
    struct_count++;
}

void module_registry_register_enum(const char *module_full, const char *enum_name, Type *type)
{
    if (!module_full || !enum_name || !type)
        return;
    for (int i = 0; i < enum_count; ++i)
    {
        if (match_strings(enum_entries[i].module_full, module_full) &&
            match_strings(enum_entries[i].name, enum_name))
        {
            enum_entries[i].type = type;
            return;
        }
    }
    if (enum_count == enum_cap)
    {
        enum_cap = enum_cap ? enum_cap * 2 : 8;
        enum_entries = (EnumEntry *)realloc(enum_entries, sizeof(EnumEntry) * (size_t)enum_cap);
    }
    enum_entries[enum_count].module_full = dup_string(module_full);
    enum_entries[enum_count].name = dup_string(enum_name);
    enum_entries[enum_count].type = type;
    enum_count++;
}

void module_registry_register_enum_value(const char *module_full, const char *enum_name, const char *value_name, int value)
{
    if (!module_full || !enum_name || !value_name)
        return;
    for (int i = 0; i < enum_value_count; ++i)
    {
        if (match_strings(enum_value_entries[i].module_full, module_full) &&
            match_strings(enum_value_entries[i].enum_name, enum_name) &&
            match_strings(enum_value_entries[i].value_name, value_name))
        {
            enum_value_entries[i].value = value;
            return;
        }
    }
    if (enum_value_count == enum_value_cap)
    {
        enum_value_cap = enum_value_cap ? enum_value_cap * 2 : 8;
        enum_value_entries = (EnumValueEntry *)realloc(enum_value_entries, sizeof(EnumValueEntry) * (size_t)enum_value_cap);
    }
    enum_value_entries[enum_value_count].module_full = dup_string(module_full);
    enum_value_entries[enum_value_count].enum_name = dup_string(enum_name);
    enum_value_entries[enum_value_count].value_name = dup_string(value_name);
    enum_value_entries[enum_value_count].value = value;
    enum_value_count++;
}

Type *module_registry_lookup_struct(const char *module_full, const char *type_name)
{
    if (!module_full || !type_name)
        return NULL;
    for (int i = 0; i < struct_count; ++i)
    {
        if (match_strings(struct_entries[i].module_full, module_full) &&
            match_strings(struct_entries[i].name, type_name))
        {
            return struct_entries[i].type;
        }
    }
    return NULL;
}

Type *module_registry_lookup_enum(const char *module_full, const char *enum_name)
{
    if (!module_full || !enum_name)
        return NULL;
    for (int i = 0; i < enum_count; ++i)
    {
        if (match_strings(enum_entries[i].module_full, module_full) &&
            match_strings(enum_entries[i].name, enum_name))
        {
            return enum_entries[i].type;
        }
    }
    return NULL;
}

int module_registry_lookup_enum_value(const char *module_full, const char *enum_name, const char *value_name, int *out_value)
{
    if (!module_full || !enum_name || !value_name)
        return 0;
    for (int i = 0; i < enum_value_count; ++i)
    {
        if (match_strings(enum_value_entries[i].module_full, module_full) &&
            match_strings(enum_value_entries[i].enum_name, enum_name) &&
            match_strings(enum_value_entries[i].value_name, value_name))
        {
            if (out_value)
                *out_value = enum_value_entries[i].value;
            return 1;
        }
    }
    return 0;
}

Type *module_registry_canonical_type(Type *ty)
{
    if (!ty)
        return NULL;
    while (ty && ty->kind == TY_IMPORT)
    {
        if (ty->import_resolved)
        {
            ty = ty->import_resolved;
            continue;
        }
        if (ty->import_module && ty->import_type_name)
        {
            Type *resolved = module_registry_lookup_struct(ty->import_module, ty->import_type_name);
            if (!resolved)
                resolved = module_registry_lookup_enum(ty->import_module, ty->import_type_name);
            ty->import_resolved = resolved;
            if (resolved)
            {
                ty = resolved;
                continue;
            }
        }
        break;
    }
    return ty;
}

const char *module_registry_find_struct_module(const Type *type)
{
    if (!type)
        return NULL;
    for (int i = 0; i < struct_count; ++i)
    {
        if (struct_entries[i].type == type)
            return struct_entries[i].module_full;
    }
    return NULL;
}

int module_registry_struct_entry_count(void)
{
    return struct_count;
}

const char *module_registry_struct_entry_module(int index)
{
    if (index < 0 || index >= struct_count)
        return NULL;
    return struct_entries[index].module_full;
}

const char *module_registry_struct_entry_name(int index)
{
    if (index < 0 || index >= struct_count)
        return NULL;
    return struct_entries[index].name;
}

Type *module_registry_struct_entry_type(int index)
{
    if (index < 0 || index >= struct_count)
        return NULL;
    return struct_entries[index].type;
}

int module_registry_enum_entry_count(void)
{
    return enum_count;
}

const char *module_registry_enum_entry_module(int index)
{
    if (index < 0 || index >= enum_count)
        return NULL;
    return enum_entries[index].module_full;
}

const char *module_registry_enum_entry_name(int index)
{
    if (index < 0 || index >= enum_count)
        return NULL;
    return enum_entries[index].name;
}

Type *module_registry_enum_entry_type(int index)
{
    if (index < 0 || index >= enum_count)
        return NULL;
    return enum_entries[index].type;
}

int module_registry_enum_value_entry_count(void)
{
    return enum_value_count;
}

const char *module_registry_enum_value_entry_module(int index)
{
    if (index < 0 || index >= enum_value_count)
        return NULL;
    return enum_value_entries[index].module_full;
}

const char *module_registry_enum_value_entry_enum(int index)
{
    if (index < 0 || index >= enum_value_count)
        return NULL;
    return enum_value_entries[index].enum_name;
}

const char *module_registry_enum_value_entry_name(int index)
{
    if (index < 0 || index >= enum_value_count)
        return NULL;
    return enum_value_entries[index].value_name;
}

int module_registry_enum_value_entry_value(int index)
{
    if (index < 0 || index >= enum_value_count)
        return 0;
    return enum_value_entries[index].value;
}
