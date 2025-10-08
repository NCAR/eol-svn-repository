# examples/python include file for CMake

set -eax

data_dir=/opt/local/bufr/build/data

# use definitions from binary dir to test if installation will be correct
def_dir="/opt/local/bufr/build/share/eccodes/definitions"
ECCODES_DEFINITION_PATH="${def_dir}"
export ECCODES_DEFINITION_PATH

tools_dir=/opt/local/bufr/build/bin
examples_dir=/opt/local/bufr/build/examples/python
examples_src=/opt/local/bufr/eccodes-2.16.0-Source/examples/python

# use samples from binary dir to test if installation will be correct
samp_dir="/opt/local/bufr/build/share/eccodes/samples"
ECCODES_SAMPLES_PATH=${samp_dir}
export ECCODES_SAMPLES_PATH

PYTHONPATH=/opt/local/bufr/build/python:$PYTHONPATH
export PYTHONPATH

echo "Current directory: `pwd`"
