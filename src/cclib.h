#ifndef CHANCE_CCLIB_H
#define CHANCE_CCLIB_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct CclibFunction
    {
        char *name;         /* logical function name */
        char *backend_name; /* backend-mangled name */
        char *return_type;  /* type spec string */
        char **param_types; /* array of type spec strings */
        uint32_t param_count;
        uint8_t is_varargs;
        uint8_t is_noreturn;
        uint8_t is_exposed;
    } CclibFunction;

    typedef struct CclibStruct
    {
        char *name;              /* struct name */
        char **field_names;      /* array of field names */
        char **field_types;      /* parallel array of type specs */
        uint32_t *field_offsets; /* byte offsets */
        uint32_t field_count;
        uint32_t size_bytes;
        uint8_t is_exposed;
    } CclibStruct;

    typedef struct CclibEnumValue
    {
        char *name;
        int32_t value;
    } CclibEnumValue;

    typedef struct CclibEnum
    {
        char *name;
        CclibEnumValue *values;
        uint32_t value_count;
        uint8_t is_exposed;
    } CclibEnum;

    typedef struct CclibGlobal
    {
        char *name;
        char *type_spec;
        uint8_t is_const;
    } CclibGlobal;

    typedef struct CclibModule
    {
        char *module_name;
        CclibFunction *functions;
        uint32_t function_count;
        CclibStruct *structs;
        uint32_t struct_count;
        CclibEnum *enums;
        uint32_t enum_count;
        CclibGlobal *globals;
        uint32_t global_count;
        uint8_t *ccbin_data;
        uint32_t ccbin_size;
    } CclibModule;

    typedef struct CclibFile
    {
        CclibModule *modules;
        uint32_t module_count;
        uint32_t format_version;
    } CclibFile;

    int cclib_write(const char *path, const CclibFile *lib);
    int cclib_read(const char *path, CclibFile *out_lib);
    void cclib_free(CclibFile *lib);

#ifdef __cplusplus
}
#endif

#endif /* CHANCE_CCLIB_H */
