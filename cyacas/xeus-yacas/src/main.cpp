#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>

#include <boost/dll/runtime_symbol_info.hpp>

#include <xeus/xkernel.hpp>
#include <xeus/xkernel_configuration.hpp>

#include <xeus-zmq/xserver_shell_main.hpp>
#include <xeus-zmq/xserver_zmq.hpp>
#include <xeus-zmq/xzmq_context.hpp>

#include "xeus-yacas/xeus-yacas.hpp"

namespace fs = std::filesystem;

void print_help(const std::string& program_name)
{
    std::cout << "Usage: " << program_name << " [connection_file]\n";
}

int install(const std::string& prefix)
{
    fs::create_directories(prefix + "/jupyter/kernels/yacas");
    std::ofstream kernel_json_file{prefix + "/jupyter/kernels/yacas/kernel.json"};

    if (kernel_json_file.is_open()) {
        const nl::json kernel_json{{"display_name", "yacas"},
                                   {"argv",
                                    {boost::dll::program_location().c_str(),
                                     "-f",
                                     "{connection_file}"}},
                                   {"language", "yacas"}};

        kernel_json_file << kernel_json.dump(4) << std::endl;
        return 0;
    } else {
        std::cerr << "Error: could not write kernel.json\n";
        return 1;
    }
}

void run_kernel(const std::string& file_name)
{
    const xeus::xconfiguration config{xeus::load_configuration(file_name)};

    const std::string scripts_path =
        (boost::dll::program_location().parent_path().parent_path() /
         "share/yacas/scripts/")
            .string();

    std::unique_ptr<xeus::xcontext> context{xeus::make_zmq_context()};

    std::unique_ptr<YacasInterpreter> interpreter{
        std::make_unique<YacasInterpreter>(scripts_path)};

    xeus::xkernel kernel{config,
                         xeus::get_user_name(),
                         std::move(context),
                         std::move(interpreter),
                         xeus::make_xserver_shell_main};

    kernel.start();
}

int main(int argc, char* argv[])
{
    if (argc > 1 &&
        (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")) {
        print_help(argv[0]);
        return 0;
    }

    if (argc > 1 && std::string(argv[1]) == "install") {
        std::string prefix{"/usr/share"};

        if (argc > 2) {
            if (std::string(argv[2]) == "--prefix") {
                if (argc > 3) {
                    prefix = argv[3];
                } else {
                    std::cerr << "Error: --prefix requires an argument\n";
                    return 1;
                }
            } else if (std::string(argv[2]) == "--user") {
                prefix = std::string(std::getenv("HOME")) + "/.local/share";
            } else {
                std::cerr << "Error: unknown argument " << argv[2] << "\n";
                return 1;
            }
        }

        return install(prefix);
    }

    const std::string file_name{(argc == 1) ? "connection.json" : argv[2]};

    run_kernel(file_name);
}
