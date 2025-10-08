#include "../headers/parser_error_handler.hpp"

void ParserErrorHandler::reportParseException(SAXParseException const& ex, std::string type)
{
    char* msg = XMLString::transcode(ex.getMessage());
    std::string color;

    if (type == "Warning") {
        color = COLOR_WARNING;
    } else if (type == "Error") {
        color = COLOR_ERROR;
    } else if (type == "Fatal Error") {
        color = COLOR_FATAL;
    }

    std::cerr << color << type <<  ": at line " << ex.getLineNumber() << " column "
        << ex.getColumnNumber() <<  " " << msg << COLOR_RESET << std::endl;
    XMLString::release(&msg);
}

void ParserErrorHandler::warning(SAXParseException const& ex)
{
    reportParseException(ex, "Warning");
}

void ParserErrorHandler::error(SAXParseException const& ex)
{
    reportParseException(ex, "Error");
}

void ParserErrorHandler::fatalError(SAXParseException const &ex)
{
    reportParseException(ex, "Fatal Error");
}

void ParserErrorHandler::resetErrors() {}

