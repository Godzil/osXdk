###
# osXdk: Compiler
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (m6502-oric-none-cc)

##########################
# Configurations variables
##########################
#set (CMAKE_C_FLAGS -Wall)
#set (CMAKE_CXX_FLAGS -Wall)

set (CMAKE_CXX_FLAGS_DEBUG -g)
set (CMAKE_CXX_FLAGS_MINSIZEREL -Os)
set (CMAKE_CXX_FLAGS_RELEASE -O2)
set (CMAKE_CXX_FLAGS_RELWITHDEBINFO -O2)

include_directories(includes)

add_executable(m6502-oric-none-cc sources/dag.c sources/decl.c sources/error.c
               sources/gen.c sources/input.c sources/main.c sources/profio.c
               sources/stmt.c sources/sym.c sources/types.c sources/debug.c 
               sources/enode.c sources/expr.c sources/init.c sources/lex.c
               sources/output.c sources/simp.c sources/string.c sources/tree.c)

