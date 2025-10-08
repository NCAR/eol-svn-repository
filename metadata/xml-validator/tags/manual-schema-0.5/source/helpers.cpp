#include "../headers/helpers.hpp"

void validateFile(std::string const& schemaFilePath, std::string const& xmlFilePath)
{
    XercesDOMParser domParser;
#ifdef DEBUG
    auto start = hr_clock::now();
#endif
    if (domParser.loadGrammar(schemaFilePath.c_str(), Grammar::SchemaGrammarType, true) == NULL)
    {
        std::cerr << COLOR_FATAL << "Fatal error: Couldn't load schema"
        << COLOR_RESET << std::endl;
        std::exit(1);
    }
#ifdef DEBUG
    auto end = hr_clock::now();
    std::cout << COLOR_INFO << "Info: time to download schema "
    << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
    << " milliseconds." << COLOR_RESET << std::endl;
#endif

    ParserErrorHandler parserErrorHandler;

    domParser.setErrorHandler(&parserErrorHandler);
    domParser.setValidationScheme(XercesDOMParser::Val_Auto);
    domParser.setDoNamespaces(true);
    domParser.setDoSchema(true);
    domParser.setValidationConstraintFatal(true);
    domParser.setExitOnFirstFatalError(false);

    doValidation(xmlFilePath, domParser);
}

void validateDirectory(std::string const& schemaFilePath, std::string const& xmlDirectoryPath)
{
    XercesDOMParser domParser;
#ifdef DEBUG
    auto start = hr_clock::now();
#endif
    if (domParser.loadGrammar(schemaFilePath.c_str(), Grammar::SchemaGrammarType, true) == NULL)
    {
        std::cerr << COLOR_FATAL << "Fatal error: Couldn't load schema"
        << COLOR_RESET << std::endl;
        std::exit(1);
    }
#ifdef DEBUG
    auto end = hr_clock::now();
    std::cout << COLOR_INFO << "Info: time to download schema "
    << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
    << " milliseconds." << COLOR_RESET << std::endl;
#endif

    ParserErrorHandler parserErrorHandler;

    domParser.setErrorHandler(&parserErrorHandler);
    domParser.setValidationScheme(XercesDOMParser::Val_Auto);
    domParser.setDoNamespaces(true);
    domParser.setDoSchema(true);
    domParser.setValidationConstraintFatal(true);
    domParser.setExitOnFirstFatalError(false);

    bpath directory(xmlDirectoryPath);
    std::vector<std::string> xmlFiles;

    for (auto const& file : boost::make_iterator_range(boost::filesystem::recursive_directory_iterator{directory},{})) {
        if (file.path().string().find(".xml") != std::string::npos) {
            xmlFiles.emplace_back(file.path().string());
        }
    }

    doValidation(xmlFiles, domParser);
}

void doValidation(std::vector<std::string> const& files, XercesDOMParser& parser)
{
    for (auto const& file : files) {
#ifdef DEBUG
        auto start = hr_clock::now();
#endif
        std::cout << COLOR_INFO << "Info: Validating: " << file << COLOR_RESET
        << std::endl;
        parser.parse(file.c_str());
#ifdef DEBUG
        auto end = hr_clock::now();
        std::cout << COLOR_INFO << "Info: time to validate XML "
        << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
        << " milliseconds." << COLOR_RESET << std::endl;
#endif
        if (parser.getErrorCount() == 0) {
            std::cout << COLOR_SUCCESS << "XML file validated against the schema successfully"
            << COLOR_RESET << std::endl;
        } else {
            std::cout << COLOR_ERROR << "XML file doesn't conform to the schema. See above errors"
            << COLOR_RESET << std::endl;
        }
    }
}

void doValidation(std::string const& file, XercesDOMParser& parser)
{
#ifdef DEBUG
    auto start = hr_clock::now();
#endif
    std::cout << COLOR_INFO << "Info: Validating " << file
    << COLOR_RESET << std::endl;
    parser.parse(file.c_str());
#ifdef DEBUG
    auto end = hr_clock::now();
    std::cout << COLOR_INFO << "Info: time to validate XML "
    << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
    << " milliseconds." << COLOR_RESET << std::endl;
#endif
    if (parser.getErrorCount() == 0) {
        std::cout << COLOR_SUCCESS << "XML file validated against the schema successfully"
        << COLOR_RESET << std::endl;
    } else {
        std::cout << COLOR_ERROR << "XML file doesn't conform to the schema. See above errors"
        << COLOR_RESET << std::endl;
    }
}
