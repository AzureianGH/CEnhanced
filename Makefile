CC ?= cc
AR ?= ar
RANLIB ?= ranlib

BUILD_DIR ?= build
OBJ_DIR := $(BUILD_DIR)/obj
TARGET := $(BUILD_DIR)/chancec
CORE_LIB := $(BUILD_DIR)/libchance_core.a

CPPFLAGS += -Isrc -I../ChanceCode/include
CFLAGS ?= -O2 -std=gnu23
LDFLAGS ?=
LDLIBS ?=

ifneq ($(strip $(CHANCE_DEFAULT_RUNTIME)),)
CPPFLAGS += -DDEFAULT_RUNTIME=\"$(CHANCE_DEFAULT_RUNTIME)\"
endif

ifneq ($(strip $(CHANCE_DEFAULT_STDLIB)),)
CPPFLAGS += -DDEFAULT_STDLIB=\"$(CHANCE_DEFAULT_STDLIB)\"
endif

CORE_SOURCES := \
	src/lexer.c \
	src/parser.c \
	src/sema.c \
	src/mangle.c \
	src/includes.c \
	src/preproc.c \
	src/frontend.c \
	src/frontend_ce.c \
	src/frontend_cinder.c \
	src/frontend_c.c \
	src/codegen_ccb.c \
	src/ccsim.c \
	src/module_registry.c \
	src/cclib.c \
	src/driver_cli.c \
	src/driver_overrides.c \
	src/driver_paths.c \
	src/driver_project.c \
	src/driver_options.c \
	src/driver_link.c \
	src/driver_runtime.c \
	src/driver_verbose.c \
	src/driver_toolchain.c \
	src/driver_validate.c \
	src/util.c

CORE_OBJECTS := $(CORE_SOURCES:src/%.c=$(OBJ_DIR)/src/%.o)
MAIN_OBJECT := $(OBJ_DIR)/src/main.o
DEPFILES := $(CORE_OBJECTS:.o=.d) $(MAIN_OBJECT:.o=.d)

.PHONY: all clean chancec chance_core

all: $(TARGET)

chancec: $(TARGET)

chance_core: $(CORE_LIB)

$(TARGET): $(CORE_LIB) $(MAIN_OBJECT)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(MAIN_OBJECT) $(CORE_LIB) $(LDFLAGS) $(LDLIBS) -o $@

$(CORE_LIB): $(CORE_OBJECTS)
	@mkdir -p $(dir $@)
	$(AR) rcs $@ $^
	$(RANLIB) $@

$(OBJ_DIR)/src/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)

-include $(DEPFILES)