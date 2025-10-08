
file(READ /opt/local/bufr/build/eccodes.pc.tmp _content)

string(REPLACE "/opt/local/bufr/build/lib" "\${libdir}" _content "${_content}")
if(NOT "lib" STREQUAL "lib")
  string(REPLACE "/opt/local/bufr/build/lib" "\${libdir}" _content "${_content}")
endif()
string(REPLACE "/opt/local/bufr/lib" "\${libdir}" _content "${_content}")

string(REGEX REPLACE "%SHORTEN:lib" "%SHORTEN:" _content "${_content}")
string(REGEX REPLACE "\\.(a|so|dylib|dll|lib)(\\.[0-9]+)*%" "%" _content "${_content}")
string(REGEX REPLACE "%SHORTEN:([^%]+)%" "\\1" _content "${_content}")

file(WRITE /opt/local/bufr/build/lib/pkgconfig/eccodes.pc "${_content}")
