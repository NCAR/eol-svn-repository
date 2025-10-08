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
named "validator." In order to properly use it, you'll have to enter in two
arguments. The first is the path (or link) to the schema you're wanting to
validate from. The second is the file or directory you want to validate.
The script will intelligently figure out whether you're trying to validate
all files in a directory, or just a file.

Example usage:
    target/validator http://ngdc.noaa.gov/metadata/published/xsd/schema.xsd iso_19115.xml
    target/validator http://ngdc.noaa.gov/metadata/published/xsd/schema.xsd iso_19115_folder/

