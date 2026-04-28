#ifndef CHANCE_VERSION_H
#define CHANCE_VERSION_H

#include "ast.h"

#define CHANCE_STRINGIFY_IMPL(value) #value
#define CHANCE_STRINGIFY(value) CHANCE_STRINGIFY_IMPL(value)

#define CHANCEC_VERSION_MAJOR 4
#define CHANCEC_VERSION_MINOR 0
#define CHANCEC_VERSION_PATCH 0

#define CHANCEC_VERSION_COMPACT                                        \
    ((CHANCEC_VERSION_MAJOR * 10000) + (CHANCEC_VERSION_MINOR * 100) + \
     CHANCEC_VERSION_PATCH)

#define CHANCEC_VERSION_STRING              \
    CHANCE_STRINGIFY(CHANCEC_VERSION_MAJOR) \
    "." CHANCE_STRINGIFY(CHANCEC_VERSION_MINOR) "." CHANCE_STRINGIFY(CHANCEC_VERSION_PATCH)

#define CHANCEC_DEFAULT_STANDARD CHANCE_STD_H28

static inline const char *chance_standard_name(int standard)
{
    switch (standard)
    {
    case CHANCE_STD_H26:
        return "H26";
    case CHANCE_STD_H27:
        return "H27";
    case CHANCE_STD_H28:
        return "H28";
    default:
        return "unknown";
    }
}

static inline int chance_standard_value(int standard)
{
    switch (standard)
    {
    case CHANCE_STD_H26:
        return CHANCE_STD_H26;
    case CHANCE_STD_H27:
        return CHANCE_STD_H27;
    case CHANCE_STD_H28:
        return CHANCE_STD_H28;
    default:
        return CHANCEC_DEFAULT_STANDARD;
    }
}

#endif