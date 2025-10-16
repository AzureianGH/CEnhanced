#include "cclib.h"

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CCLIB_MAGIC "CCLIB"
#define CCLIB_VERSION 1u

static int write_u8(FILE *out, uint8_t value)
{
    return fputc(value, out) != EOF;
}

static int write_u32(FILE *out, uint32_t value)
{
    uint8_t buf[4];
    buf[0] = (uint8_t)(value & 0xFFu);
    buf[1] = (uint8_t)((value >> 8) & 0xFFu);
    buf[2] = (uint8_t)((value >> 16) & 0xFFu);
    buf[3] = (uint8_t)((value >> 24) & 0xFFu);
    return fwrite(buf, 1, sizeof(buf), out) == sizeof(buf);
}

static int write_bytes(FILE *out, const uint8_t *data, uint32_t size)
{
    if (size == 0)
        return 1;
    return data && fwrite(data, 1, size, out) == size;
}

static int write_string(FILE *out, const char *str)
{
    if (!str)
    {
        if (!write_u8(out, 0))
            return 0;
        return 1;
    }

    size_t len = strlen(str);
    if (len > UINT32_MAX)
        return 0;
    if (!write_u8(out, 1))
        return 0;
    if (!write_u32(out, (uint32_t)len))
        return 0;
    return fwrite(str, 1, len, out) == len;
}

static int read_u8(FILE *in, uint8_t *value)
{
    int ch = fgetc(in);
    if (ch == EOF)
        return 0;
    *value = (uint8_t)ch;
    return 1;
}

static int read_u32(FILE *in, uint32_t *value)
{
    uint8_t buf[4];
    if (fread(buf, 1, sizeof(buf), in) != sizeof(buf))
        return 0;
    *value = (uint32_t)buf[0] |
             ((uint32_t)buf[1] << 8) |
             ((uint32_t)buf[2] << 16) |
             ((uint32_t)buf[3] << 24);
    return 1;
}

static int read_bytes(FILE *in, uint8_t **data, uint32_t size)
{
    if (size == 0)
    {
        *data = NULL;
        return 1;
    }
    uint8_t *buf = (uint8_t *)malloc(size);
    if (!buf)
        return 0;
    if (fread(buf, 1, size, in) != size)
    {
        free(buf);
        return 0;
    }
    *data = buf;
    return 1;
}

static int read_string(FILE *in, char **out)
{
    uint8_t present = 0;
    if (!read_u8(in, &present))
        return 0;
    if (!present)
    {
        *out = NULL;
        return 1;
    }
    uint32_t len = 0;
    if (!read_u32(in, &len))
        return 0;
    char *buf = (char *)malloc((size_t)len + 1);
    if (!buf)
        return 0;
    if (len > 0 && fread(buf, 1, len, in) != len)
    {
        free(buf);
        return 0;
    }
    buf[len] = '\0';
    *out = buf;
    return 1;
}

static void free_string_array(char **arr, uint32_t count)
{
    if (!arr)
        return;
    for (uint32_t i = 0; i < count; ++i)
        free(arr[i]);
    free(arr);
}

static void free_enum_values(CclibEnumValue *values, uint32_t count)
{
    if (!values)
        return;
    for (uint32_t i = 0; i < count; ++i)
        free(values[i].name);
    free(values);
}

static int write_function(FILE *out, const CclibFunction *fn)
{
    if (!write_string(out, fn ? fn->name : NULL))
        return 0;
    if (!write_string(out, fn ? fn->backend_name : NULL))
        return 0;
    if (!write_string(out, fn ? fn->return_type : NULL))
        return 0;
    uint32_t param_count = fn ? fn->param_count : 0;
    if (!write_u32(out, param_count))
        return 0;
    for (uint32_t i = 0; i < param_count; ++i)
    {
        if (!write_string(out, fn->param_types ? fn->param_types[i] : NULL))
            return 0;
    }
    if (!write_u8(out, fn ? fn->is_varargs : 0))
        return 0;
    if (!write_u8(out, fn ? fn->is_noreturn : 0))
        return 0;
    if (!write_u8(out, fn ? fn->is_exposed : 0))
        return 0;
    return 1;
}

static int write_struct(FILE *out, const CclibStruct *st)
{
    if (!write_string(out, st ? st->name : NULL))
        return 0;
    if (!write_u32(out, st ? st->field_count : 0))
        return 0;
    uint32_t count = st ? st->field_count : 0;
    for (uint32_t i = 0; i < count; ++i)
    {
        if (!write_string(out, st->field_names ? st->field_names[i] : NULL))
            return 0;
        if (!write_string(out, st->field_types ? st->field_types[i] : NULL))
            return 0;
        uint32_t offset = st && st->field_offsets ? st->field_offsets[i] : 0;
        if (!write_u32(out, offset))
            return 0;
    }
    if (!write_u32(out, st ? st->size_bytes : 0))
        return 0;
    if (!write_u8(out, st ? st->is_exposed : 0))
        return 0;
    return 1;
}

static int write_enum(FILE *out, const CclibEnum *en)
{
    if (!write_string(out, en ? en->name : NULL))
        return 0;
    if (!write_u32(out, en ? en->value_count : 0))
        return 0;
    uint32_t count = en ? en->value_count : 0;
    for (uint32_t i = 0; i < count; ++i)
    {
        if (!write_string(out, en->values ? en->values[i].name : NULL))
            return 0;
        int32_t value = en && en->values ? en->values[i].value : 0;
        if (!write_u32(out, (uint32_t)value))
            return 0;
    }
    if (!write_u8(out, en ? en->is_exposed : 0))
        return 0;
    return 1;
}

static int write_global(FILE *out, const CclibGlobal *gl)
{
    if (!write_string(out, gl ? gl->name : NULL))
        return 0;
    if (!write_string(out, gl ? gl->type_spec : NULL))
        return 0;
    if (!write_u8(out, gl ? gl->is_const : 0))
        return 0;
    return 1;
}

int cclib_write(const char *path, const CclibFile *lib)
{
    if (!path || !lib)
        return EINVAL;

    FILE *out = fopen(path, "wb");
    if (!out)
        return errno ? errno : EIO;

    int ok = 1;

    if (ok)
        ok = fwrite(CCLIB_MAGIC, 1, strlen(CCLIB_MAGIC), out) == strlen(CCLIB_MAGIC);
    if (ok)
        ok = write_u32(out, CCLIB_VERSION);
    uint32_t module_count = lib->module_count;
    if (ok)
        ok = write_u32(out, module_count);

    for (uint32_t mi = 0; ok && mi < module_count; ++mi)
    {
        const CclibModule *mod = &lib->modules[mi];
        if (!write_string(out, mod ? mod->module_name : NULL))
        {
            ok = 0;
            break;
        }
        if (!write_u32(out, mod ? mod->function_count : 0))
        {
            ok = 0;
            break;
        }
        for (uint32_t fi = 0; ok && mod && fi < mod->function_count; ++fi)
            ok = write_function(out, &mod->functions[fi]);

        if (!ok)
            break;

        if (!write_u32(out, mod ? mod->struct_count : 0))
        {
            ok = 0;
            break;
        }
        for (uint32_t si = 0; ok && mod && si < mod->struct_count; ++si)
            ok = write_struct(out, &mod->structs[si]);

        if (!ok)
            break;

        if (!write_u32(out, mod ? mod->enum_count : 0))
        {
            ok = 0;
            break;
        }
        for (uint32_t ei = 0; ok && mod && ei < mod->enum_count; ++ei)
            ok = write_enum(out, &mod->enums[ei]);

        if (!ok)
            break;

        if (!write_u32(out, mod ? mod->global_count : 0))
        {
            ok = 0;
            break;
        }
        for (uint32_t gi = 0; ok && mod && gi < mod->global_count; ++gi)
            ok = write_global(out, &mod->globals[gi]);

        if (!ok)
            break;

        if (!write_u32(out, mod ? mod->ccbin_size : 0))
        {
            ok = 0;
            break;
        }
        if (!write_bytes(out, mod ? mod->ccbin_data : NULL, mod ? mod->ccbin_size : 0))
        {
            ok = 0;
            break;
        }
    }

    if (!ok)
    {
        fclose(out);
        remove(path);
        return EIO;
    }

    if (fclose(out) != 0)
    {
        remove(path);
        return errno ? errno : EIO;
    }

    return 0;
}

static int read_function(FILE *in, CclibFunction *fn)
{
    memset(fn, 0, sizeof(*fn));
    if (!read_string(in, &fn->name))
        return 0;
    if (!read_string(in, &fn->backend_name))
        return 0;
    if (!read_string(in, &fn->return_type))
        return 0;
    uint32_t param_count = 0;
    if (!read_u32(in, &param_count))
        return 0;
    if (param_count > 0)
    {
        fn->param_types = (char **)calloc(param_count, sizeof(char *));
        if (!fn->param_types)
            return 0;
        for (uint32_t i = 0; i < param_count; ++i)
        {
            if (!read_string(in, &fn->param_types[i]))
                return 0;
        }
    }
    fn->param_count = param_count;
    if (!read_u8(in, &fn->is_varargs))
        return 0;
    if (!read_u8(in, &fn->is_noreturn))
        return 0;
    if (!read_u8(in, &fn->is_exposed))
        return 0;
    return 1;
}

static int read_struct(FILE *in, CclibStruct *st)
{
    memset(st, 0, sizeof(*st));
    if (!read_string(in, &st->name))
        return 0;
    uint32_t field_count = 0;
    if (!read_u32(in, &field_count))
        return 0;
    if (field_count > 0)
    {
        st->field_names = (char **)calloc(field_count, sizeof(char *));
        st->field_types = (char **)calloc(field_count, sizeof(char *));
        st->field_offsets = (uint32_t *)calloc(field_count, sizeof(uint32_t));
        if (!st->field_names || !st->field_types || !st->field_offsets)
            return 0;
        for (uint32_t i = 0; i < field_count; ++i)
        {
            if (!read_string(in, &st->field_names[i]))
                return 0;
            if (!read_string(in, &st->field_types[i]))
                return 0;
            if (!read_u32(in, &st->field_offsets[i]))
                return 0;
        }
    }
    st->field_count = field_count;
    if (!read_u32(in, &st->size_bytes))
        return 0;
    if (!read_u8(in, &st->is_exposed))
        return 0;
    return 1;
}

static int read_enum(FILE *in, CclibEnum *en)
{
    memset(en, 0, sizeof(*en));
    if (!read_string(in, &en->name))
        return 0;
    uint32_t count = 0;
    if (!read_u32(in, &count))
        return 0;
    if (count > 0)
    {
        en->values = (CclibEnumValue *)calloc(count, sizeof(CclibEnumValue));
        if (!en->values)
            return 0;
        for (uint32_t i = 0; i < count; ++i)
        {
            if (!read_string(in, &en->values[i].name))
                return 0;
            uint32_t raw = 0;
            if (!read_u32(in, &raw))
                return 0;
            en->values[i].value = (int32_t)raw;
        }
    }
    en->value_count = count;
    if (!read_u8(in, &en->is_exposed))
        return 0;
    return 1;
}

static int read_global(FILE *in, CclibGlobal *gl)
{
    memset(gl, 0, sizeof(*gl));
    if (!read_string(in, &gl->name))
        return 0;
    if (!read_string(in, &gl->type_spec))
        return 0;
    if (!read_u8(in, &gl->is_const))
        return 0;
    return 1;
}

int cclib_read(const char *path, CclibFile *out_lib)
{
    if (!path || !out_lib)
        return EINVAL;

    FILE *in = fopen(path, "rb");
    if (!in)
        return errno ? errno : EIO;

    memset(out_lib, 0, sizeof(*out_lib));

    char magic[5] = {0};
    if (fread(magic, 1, 5, in) != 5 || memcmp(magic, CCLIB_MAGIC, 5) != 0)
    {
        fclose(in);
        return EINVAL;
    }

    uint32_t version = 0;
    if (!read_u32(in, &version))
    {
        fclose(in);
        return EIO;
    }
    out_lib->format_version = version;

    uint32_t module_count = 0;
    if (!read_u32(in, &module_count))
    {
        fclose(in);
        return EIO;
    }

    out_lib->modules = (CclibModule *)calloc(module_count, sizeof(CclibModule));
    if (!out_lib->modules && module_count > 0)
    {
        fclose(in);
        return ENOMEM;
    }
    out_lib->module_count = module_count;

    for (uint32_t mi = 0; mi < module_count; ++mi)
    {
        CclibModule *mod = &out_lib->modules[mi];
        if (!read_string(in, &mod->module_name))
        {
            fclose(in);
            return EIO;
        }
        uint32_t function_count = 0;
        if (!read_u32(in, &function_count))
        {
            fclose(in);
            return EIO;
        }
        if (function_count > 0)
        {
            mod->functions = (CclibFunction *)calloc(function_count, sizeof(CclibFunction));
            if (!mod->functions)
            {
                fclose(in);
                return ENOMEM;
            }
            for (uint32_t fi = 0; fi < function_count; ++fi)
            {
                if (!read_function(in, &mod->functions[fi]))
                {
                    fclose(in);
                    return EIO;
                }
            }
        }
        mod->function_count = function_count;

        uint32_t struct_count = 0;
        if (!read_u32(in, &struct_count))
        {
            fclose(in);
            return EIO;
        }
        if (struct_count > 0)
        {
            mod->structs = (CclibStruct *)calloc(struct_count, sizeof(CclibStruct));
            if (!mod->structs)
            {
                fclose(in);
                return ENOMEM;
            }
            for (uint32_t si = 0; si < struct_count; ++si)
            {
                if (!read_struct(in, &mod->structs[si]))
                {
                    fclose(in);
                    return EIO;
                }
            }
        }
        mod->struct_count = struct_count;

        uint32_t enum_count = 0;
        if (!read_u32(in, &enum_count))
        {
            fclose(in);
            return EIO;
        }
        if (enum_count > 0)
        {
            mod->enums = (CclibEnum *)calloc(enum_count, sizeof(CclibEnum));
            if (!mod->enums)
            {
                fclose(in);
                return ENOMEM;
            }
            for (uint32_t ei = 0; ei < enum_count; ++ei)
            {
                if (!read_enum(in, &mod->enums[ei]))
                {
                    fclose(in);
                    return EIO;
                }
            }
        }
        mod->enum_count = enum_count;

        uint32_t global_count = 0;
        if (!read_u32(in, &global_count))
        {
            fclose(in);
            return EIO;
        }
        if (global_count > 0)
        {
            mod->globals = (CclibGlobal *)calloc(global_count, sizeof(CclibGlobal));
            if (!mod->globals)
            {
                fclose(in);
                return ENOMEM;
            }
            for (uint32_t gi = 0; gi < global_count; ++gi)
            {
                if (!read_global(in, &mod->globals[gi]))
                {
                    fclose(in);
                    return EIO;
                }
            }
        }
        mod->global_count = global_count;

        uint32_t ccbin_size = 0;
        if (!read_u32(in, &ccbin_size))
        {
            fclose(in);
            return EIO;
        }
        if (!read_bytes(in, &mod->ccbin_data, ccbin_size))
        {
            fclose(in);
            return EIO;
        }
        mod->ccbin_size = ccbin_size;
    }

    fclose(in);
    return 0;
}

void cclib_free(CclibFile *lib)
{
    if (!lib || !lib->modules)
        return;

    for (uint32_t mi = 0; mi < lib->module_count; ++mi)
    {
        CclibModule *mod = &lib->modules[mi];
        free(mod->module_name);

        if (mod->functions)
        {
            for (uint32_t fi = 0; fi < mod->function_count; ++fi)
            {
                CclibFunction *fn = &mod->functions[fi];
                free(fn->name);
                free(fn->backend_name);
                free(fn->return_type);
                if (fn->param_types)
                    free_string_array(fn->param_types, fn->param_count);
            }
            free(mod->functions);
        }

        if (mod->structs)
        {
            for (uint32_t si = 0; si < mod->struct_count; ++si)
            {
                CclibStruct *st = &mod->structs[si];
                free(st->name);
                if (st->field_names)
                    free_string_array(st->field_names, st->field_count);
                if (st->field_types)
                    free_string_array(st->field_types, st->field_count);
                free(st->field_offsets);
            }
            free(mod->structs);
        }

        if (mod->enums)
        {
            for (uint32_t ei = 0; ei < mod->enum_count; ++ei)
            {
                CclibEnum *en = &mod->enums[ei];
                free(en->name);
                free_enum_values(en->values, en->value_count);
            }
            free(mod->enums);
        }

        if (mod->globals)
        {
            for (uint32_t gi = 0; gi < mod->global_count; ++gi)
            {
                free(mod->globals[gi].name);
                free(mod->globals[gi].type_spec);
            }
            free(mod->globals);
        }

        free(mod->ccbin_data);
    }

    free(lib->modules);
    lib->modules = NULL;
    lib->module_count = 0;
}
