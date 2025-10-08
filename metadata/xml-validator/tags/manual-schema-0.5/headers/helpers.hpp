#include "includes.hpp"
#include "../headers/parser_error_handler.hpp"

void validateFile(std::string const&, std::string const&);
void validateDirectory(std::string const&, std::string const&);
void doValidation(std::vector<std::string> const&, XercesDOMParser&);
void doValidation(std::string const&, XercesDOMParser&);
