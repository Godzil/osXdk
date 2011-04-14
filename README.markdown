## osXdk

The Crossplatform Oric SDK (the X stand for Cross and uniX/posiX)

The Original OSDK can be found [here](http://osdk.defence-force.org/)

### General information

The osXdk is a complete cross-development system allowing you to create software for the Oric range of computers 1.

If you are interested in developing your programs for other 6502 based machines than the Oric, you should check cc65. You are still reading, so I assume that you are still interested in the osXdk. Good.

#### I recommand for now to try to use the original OSDK since this version still under devellopement and it could have a lots of bug due to the port.

### System requirements

In order to use the osXdk, you need a configuration that matches these minimum requirements:

Any POSIX compatible UNIX OS. Currently I use a Gentoo linux computer and a MacBookPro under 10.6 without any problems.

### Download

By downloading the osXdk, you will get a lot more than only a C cross-compiler for the Oric:

- An ANSI C Compiler (LCC)
- A powerful 6502 cross-assembler (XA)
- A BASIC converter
- A linker
- A set of library functions
- A map-file analyzer
- A file packer (FilePack)
- A picture converter (PictConv)
- Some tools to help you create virtual and real tapes and disks
- A ready to use Oric emulator and debugger (Euphoric)
- A number of sample programs written in C, Assembler and BASIC, with comments.

### Support

If you have questions, suggestions, or simply want to discuss the sdXdk, you can try one of the following options:
- For bug or problem specificaly to this fork, use the GitHub utilities to report bugs, or leave me a messages here
- For other problem on Oric development write a message in [the Defence-Force forums](http://forum.defence-force.org/viewforum.php?f=3)

### License

There is currently no clear license on the original OSDK. I will contact DBug about this, and will currently put this fork under the classical MIT licence:

    OSDK Copyright (c) 2001-2010 DBug
    osXdk UNIX Port Copyright (c) 2010-2011 godzil <Manoel Trapier>

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.
