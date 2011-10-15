###
# osXdk - MemMap: Make a memory map (in HTML) of the given executable
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (oric-memmap)

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

add_executable(oric-memmap sources/memmap.cpp)

target_link_libraries(oric-memmap osxdk_common)
