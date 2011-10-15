###
# osXdk - bin2txt: Binary file to Text file
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (oric-bin2txt)

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

add_executable(oric-bin2txt sources/bin2txt.cpp)

target_link_libraries(oric-bin2txt osxdk_common)