#include "../headers/includes.hpp"

using hr_clock = std::chrono::high_resolution_clock;
using bpath = boost::filesystem::path;

void validateFile(std::string const&, std::string const&);
void validateDirectory(std::string const&, std::string const&);
void doValidation(std::vector<std::string> const&, XercesDOMParser&);
void doValidation(std::string const&, XercesDOMParser&);

int main(int argc, const char **argv)
{
    if (argc < 3)
    {
        std::cout << COLOR_ERROR
            << "Not enough arguments!\nUsage: validator <schema file> <xml file>|<directory>"
            << COLOR_RESET << std::endl;
        return 1;
    }


    XMLPlatformUtils::Initialize();

    boost::filesystem::path p(argv[2]);

    if (boost::filesystem::is_directory(p)) {
        validateDirectory(argv[1], argv[2]);
    } else {
        validateFile(argv[1], argv[2]);
    }

    XMLPlatformUtils::Terminate();

    return 0;
}
