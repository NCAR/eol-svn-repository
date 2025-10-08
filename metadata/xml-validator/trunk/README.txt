XML Validator Readme
==========================

Author: Eric Dattore
Last Revision: 4/1/16

Overview:

The XML validator is a C++ wrapper around the Xerces-C XML parsing library
that is designed to validate XML files in various schemas. It was created
primarily to bulk-validate our ISO 19115-2 records. The functionality has
since been extended to support various other metadata standards provided
the validator is aware of the schema.

Build Instructions:
The XML validator depends on the Xerces-C and Boost libraries. In order to
build the program, make sure you have both libraries installed and in your
linker's library path. Currently, the only EOL server meeting these
requirements is Barolo.

The code includes a Makefile so it's as simple as running the make command.
The Makefile is set up in such a way that you can parallelize the builds as it
compiles each file separately into object code and then links them all together
with the library dependences. In order to compile in parallel, use the -j<num>
flag where <num> is the number of parallel jobs you want to run. This will
speed up compilation time.

Usage:
Once built, the tool is placed in the target directory. The executable will be
named "validator." In order to properly use it, you'll have to enter in at least two
arguments. The first of which is an argument indicating whether you're Validating
a file or directory and then the name of that file or directory. Secondly, you'll need
to indicate whether you want to auto-detect the schema from the file (only works on files, not directories)
or if you want to use one of the built-in schemas using the long flag. See example usage
below for more details.

Example usage:
    target/validator -a -f ../../102.016_v4.xml
    target/validator -d ../../ --noaa-iso
    target/validator -f ../../255.079_v1.xml --tc211-iso

Future Work:
- Get schema from XML file for directories (works for files)

Technical Details:

The XML Validator uses Boost for program options, filesystem interactions, and regular expressions
for picking out the schema from the XML file. The header files contain all the .h files that define
the interface of functions or classes, the source directory contains the .cpp files that define the
implementation of the functions or classes.

The validator.cpp file is the main driver for the program. The file creates the program options and runs
the program based on the user provided options.

The parser_error_handler.cpp file defines the parser error handler class that prints a new error message
depending on errors that happen while attempting to validate. It inherits from Xerces constructs to hook
in with the parser operation.

The helpers.cpp file defines the helper methods used to validate the XML file(s). There are methods for
setting up the parser and then doing the parse on a single file or list of files. It also includes the
method for attempting to extract the schema from the XML file supplied by the user.

Each of those source files has a corresponding header in the headers directory. The other header files no
mentioned above are builtin_schema.hpp and includes.hpp. The builtin_schema.hpp file is a static map of
schemas that the validator can handle in a key-value pair. Adding new schema is easy and just requires
recompiling the executable. This method is less than ideal and can be superseded by automatically
fetching the schema from the XML file. The includes.hpp file just contains a global list of all included
headers that are required to compile the program.
