#include "includes.hpp"
#include "../headers/parser_error_handler.hpp"

void validateFile(std::pair<std::string, std::string> const&);
void validateDirectory(std::string const&, std::string const&, int);
void doValidationThreadSafe(std::vector<std::pair<std::string, std::string>> const&, XercesDOMParser&, std::mutex&);
void doValidation(std::pair<std::string, std::string> const&, XercesDOMParser&);
