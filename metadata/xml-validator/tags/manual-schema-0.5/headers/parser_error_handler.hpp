#ifndef _PARSER_ERROR_HANDLER_HPP_
#define _PARSER_ERROR_HANDLER_HPP_

#include "includes.hpp"

class ParserErrorHandler : public ErrorHandler
{
    private:
        void reportParseException(const SAXParseException& ex, std::string type);

    public:
        void warning(const SAXParseException& ex);
        void error(const SAXParseException& ex);
        void fatalError(const SAXParseException& ex);
        void resetErrors();
};

#endif

