###
# osXdk: Compiler
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (m6502-oric-none-as)

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

add_executable(m6502-oric-none-as sources/xa.cpp sources/xaa.cpp
               sources/xal.cpp sources/xam.cpp sources/xao.cpp
               sources/xap.cpp sources/xar.cpp sources/xar2.cpp
               sources/xat.cpp sources/xau.cpp)

target_link_libraries(m6502-oric-none-as osxdk_common)