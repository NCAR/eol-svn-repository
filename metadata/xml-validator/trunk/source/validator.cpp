// TODO: Start dividing work out amongst threads
#include "../headers/includes.hpp"
#include "../headers/helpers.hpp"

using bpath = boost::filesystem::path;

using namespace boost::program_options;

int main(int argc, const char **argv)
{
    try {
        options_description cmdline_options{"Usage"};

        options_description default_options{"General Options"};
        default_options.add_options()
            ("file,f", value<std::string>(), "File to validate")
            ("dir,d", value<std::string>(), "Directory to validate")
            ("help,h", "Help information");

        options_description directory_options{"Directory Options"};
        directory_options.add_options()
            ("ext,e", value<std::string>()->default_value("xml"), "The file extension to search for in the specified directory")
            ("jobs,j", value<int>()->default_value(4), "Number of concurrent jobs to validate with");

        cmdline_options.add(default_options).add(directory_options);

        variables_map vm;
        store(parse_command_line(argc, argv, cmdline_options), vm);
        notify(vm);

        std::string schemaKey = "";

        if (vm.count("help")) {
            std::cout << cmdline_options << std::endl;
        } else if (vm.count("file")) {
            boost::filesystem::path p(vm["file"].as<std::string>());
            if (!boost::filesystem::is_regular_file(p)) {
                throw error("Argument provided is not a file!");
            }

            std::pair<std::string, std::string> file =
                std::make_pair(p.string(), p.filename().string());

            XMLPlatformUtils::Initialize();
            validateFile(file);
            XMLPlatformUtils::Terminate();
        } else if (vm.count("dir")) {
            boost::filesystem::path p(vm["dir"].as<std::string>());
            if (!boost::filesystem::is_directory(p)) {
                throw error("Argument provided is not a directory!");
            }

            std::string extension = vm["ext"].as<std::string>();
            int jobs = vm["jobs"].as<int>();

            XMLPlatformUtils::Initialize();
            validateDirectory(p.string(), extension, jobs);
            XMLPlatformUtils::Terminate();
        } else {
            std::cout << cmdline_options << std::endl;
        }
    } catch (const error &ex) {
        std::cerr << ex.what() << std::endl;
        return 1;
    }

    return 0;
}
