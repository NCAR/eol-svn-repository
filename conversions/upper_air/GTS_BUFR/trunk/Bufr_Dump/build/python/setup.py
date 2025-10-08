#!/usr/bin/env python

from distutils.core import setup, Extension
import os
import sys

import numpy
# Obtain the numpy include directory.  This logic works across numpy versions.
try:
    numpy_include = numpy.get_include()
except AttributeError:
    numpy_include = numpy.get_numpy_include()

# See ECC-644
extra_compile_args = []
cmake_c_compiler_id='GNU'
if cmake_c_compiler_id == 'PGI':
    extra_compile_args.append('-noswitcherror')

attdict = dict(sources=['/opt/local/bufr/eccodes-2.16.0-Source/python/swig_wrap_numpy.c',
                        '/opt/local/bufr/eccodes-2.16.0-Source/python/grib_interface.c'],
               swig_opts=[],
               include_dirs=['.', '/opt/local/bufr/build/python/../src',
                             '/opt/local/bufr/eccodes-2.16.0-Source/python/../src',
                             numpy_include],
               library_dirs=['/opt/local/bufr/build/lib'],
               runtime_library_dirs=[],
               libraries=['eccodes'],
               extra_compile_args=extra_compile_args,
               extra_objects=[])

shared_libs='ON'
if shared_libs == 'OFF':

    add_attribute = lambda **args: [list.append(attdict[key], value)
                                    for key, value in args.items()]

    if 1:
        jasper_dir = '/usr'
        if jasper_dir and jasper_dir != 'system':
            add_attribute(library_dirs=os.path.join(jasper_dir, 'lib'),
                          runtime_library_dirs=os.path.join(jasper_dir, 'lib'))
        add_attribute(libraries='jasper')

    if 0:
        openjpeg_lib_dir = ''
        openjpeg_libname = ''
        if openjpeg_lib_dir:
            add_attribute(library_dirs=openjpeg_lib_dir,
                          runtime_library_dirs=openjpeg_lib_dir)
        add_attribute(libraries=openjpeg_libname)

    # assumes png is supplied by system paths -- may not be true
    if 0:
        add_attribute(libraries='png')

    if 0:
        add_attribute(libraries='eccodes_memfs')

    if 0:
        aec_dir = ''
        if aec_dir and aec_dir != 'system':
            add_attribute(library_dirs=os.path.join(aec_dir, 'lib'),
                          runtime_library_dirs=os.path.join(aec_dir, 'lib'))
        add_attribute(libraries='aec')


setup(name='eccodes',
      version='2.16.0',
      author='ECMWF',
      author_email='Software.Support@ecmwf.int',
      description="""Python interface for ecCodes""",
      license='Apache License, Version 2.0',
      url='https://confluence.ecmwf.int/display/ECC/ecCodes+Home',
      download_url='https://confluence.ecmwf.int/display/ECC/Releases',
      ext_modules=[Extension('gribapi._gribapi_swig', **attdict)],
      packages=['eccodes', 'eccodes.high_level', 'gribapi'])
