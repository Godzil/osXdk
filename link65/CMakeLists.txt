###
# osXdk: Compiler
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (m6502-oric-none-ld)

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

include_directories(../common/includes)

add_executable(m6502-oric-none-ld sources/Link65.cpp)

target_link_libraries(m6502-oric-none-ld osxdk_common)

