###
# osXdk: Compiler
#
# CMakeFile Author: Manoel Trapier <godzil@godzil.net>

cmake_minimum_required (VERSION 2.6)
project (m6502-oric-osXdk)

##########################
# Configurations variables
##########################
add_subdirectory(compiler)
add_subdirectory(common)
add_subdirectory(link65)
#add_directory(opt65)
#add_directory(xa)
