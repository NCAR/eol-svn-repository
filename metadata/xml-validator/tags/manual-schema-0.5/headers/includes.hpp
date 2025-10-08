#ifndef _INCLUDES_HPP_
#define _INCLUDES_HPP_

#include <iostream>
#include <string>
#include <chrono>
#include <cstdlib>
#include <boost/filesystem.hpp>
#include <boost/range/iterator_range_core.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/sax/ErrorHandler.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/validators/common/Grammar.hpp>

XERCES_CPP_NAMESPACE_USE

static std::string const COLOR_FATAL = "\x1b[45m";
static std::string const COLOR_ERROR = "\x1b[41m";
static std::string const COLOR_WARNING = "\x1b[43m";
static std::string const COLOR_SUCCESS = "\x1b[42m";
static std::string const COLOR_INFO = "\x1b[44m";
static std::string const COLOR_RESET = "\x1b[0m";

using hr_clock = std::chrono::high_resolution_clock;
using bpath = boost::filesystem::path;


#endif
