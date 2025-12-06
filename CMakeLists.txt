cmake_minimum_required(VERSION 3.20)

project(CHance LANGUAGES C)

# Options
option(CHANCE_BUILD_TESTS "Build tests via CTest" ON)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Avoid Ninja: do not set a generator here; users can configure with VS or NMake.

add_library(chance_core
    src/lexer.c
    src/parser.c
    src/sema.c
    src/mangle.c
    src/includes.c
    src/preproc.c
    src/codegen_ccb.c
    src/module_registry.c
    src/cclib.c
    src/util.c
)

target_include_directories(chance_core PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../ChanceCode/include
)

add_executable(chancec
    src/main.c
)

target_link_libraries(chancec PRIVATE chance_core)

include(GNUInstallDirs)
install(TARGETS chancec RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if(CHANCE_BUILD_TESTS)
    include(CTest)
    add_subdirectory(tests)
endif()
