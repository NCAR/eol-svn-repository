#include "../headers/helpers.hpp"

void validateFile(std::pair<std::string, std::string> const& xmlFile)
{
    XercesDOMParser domParser;
    ParserErrorHandler parserErrorHandler;

    domParser.setErrorHandler(&parserErrorHandler);
    domParser.setValidationScheme(XercesDOMParser::Val_Auto);
    domParser.setDoNamespaces(true);
    domParser.setDoSchema(true);
    domParser.setValidationConstraintFatal(true);
    domParser.setExitOnFirstFatalError(false);

    doValidation(xmlFile, domParser);
}

void validateDirectory(std::string const& xmlDirectoryPath, std::string const& extension, int jobs)
{
    bpath directory(xmlDirectoryPath);
    std::vector<std::pair<std::string, std::string>> xmlFiles;

    std::vector<std::vector<std::pair<std::string, std::string>>> dividedVectors(jobs);
    std::vector<std::thread> threads;
    std::mutex sharedMutex;

    auto validatorFunction = [] (std::vector<std::pair<std::string, std::string>> xmlFiles, std::mutex& sharedMutex) {
        XercesDOMParser domParser;
        ParserErrorHandler parserErrorHandler;

        domParser.setErrorHandler(&parserErrorHandler);
        domParser.setValidationScheme(XercesDOMParser::Val_Auto);
        domParser.setDoNamespaces(true);
        domParser.setDoSchema(true);
        domParser.setValidationConstraintFatal(true);
        domParser.setExitOnFirstFatalError(false);

        doValidationThreadSafe(xmlFiles, domParser, sharedMutex);
    };

    for (auto const& file : boost::make_iterator_range(boost::filesystem::recursive_directory_iterator{directory},{})) {
        if (file.path().string().find("." + extension) != std::string::npos) {
            xmlFiles.emplace_back(std::make_pair(file.path().string(), file.path().filename().string()));
        }
    }

    int partitionSize = std::round(xmlFiles.size() / jobs);
    for (int i = 0; i < jobs - 1; ++i) {
        dividedVectors[i] = std::move(
            std::vector<std::pair<std::string, std::string>>(
                std::make_move_iterator(xmlFiles.begin()),
                std::make_move_iterator(xmlFiles.begin() + partitionSize)
            )
        );

        xmlFiles.erase(xmlFiles.begin(), xmlFiles.begin() + partitionSize);
    }

    dividedVectors[jobs - 1] = std::move(std::vector<std::pair<std::string, std::string>>(
            std::make_move_iterator(xmlFiles.begin()),
            std::make_move_iterator(xmlFiles.end())
        )
    );
    std::cout << "Vector " << jobs - 1 << " size is: " << dividedVectors[jobs - 1].size() << std::endl;

    for (int i = 0; i < jobs; ++i) {
        threads.emplace_back(
            std::thread(
                validatorFunction,
                std::move(dividedVectors[i]),
                std::ref(sharedMutex)
            )
        );
    }

    for (auto& thread : threads) {
        thread.join();
    }
}

void doValidationThreadSafe(std::vector<std::pair<std::string, std::string>> const& files, XercesDOMParser& parser, std::mutex& lock)
{
    for (auto const& file : files) {
        lock.lock();
        std::cout << COLOR_INFO << "Validating " << file.second << "."
            << COLOR_RESET << std::endl;
        lock.unlock();
        parser.parse(file.first.c_str());

        if (parser.getErrorCount() == 0) {
            lock.lock();
            std::cout << COLOR_SUCCESS << "XML file, " << file.second
                << ", validated against the schema successfully." << COLOR_RESET << std::endl;
            lock.unlock();
        } else {
            lock.lock();
            std::cout << COLOR_ERROR << "XML file, " << file.second
                << ", doesn't conform to the schema. See above errors." << COLOR_RESET << std::endl;
            lock.unlock();
        }
    }
}

void doValidation(std::pair<std::string, std::string> const& file, XercesDOMParser& parser)
{
    std::cout << COLOR_INFO << "Validating " << file.second << "."
        << COLOR_RESET << std::endl;
    parser.parse(file.first.c_str());

    if (parser.getErrorCount() == 0) {
        std::cout << COLOR_SUCCESS << "XML file, " << file.second
            << ", validated against the schema successfully" << COLOR_RESET << std::endl;
    } else {
        std::cout << COLOR_ERROR << "XML file, " << file.second
            << ", doesn't conform to the schema. See above errors" << COLOR_RESET << std::endl;
    }
}

