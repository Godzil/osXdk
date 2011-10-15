###
# osXdk: The Crossplatform Oric Software Development Kit
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
add_subdirectory(opt65)
add_subdirectory(xa)


#tools (with oric- prefix)
add_subdirectory(bas2tap)
add_subdirectory(bin2txt)
add_subdirectory(MemMap)