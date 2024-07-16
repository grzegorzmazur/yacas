#ifndef XEUS_YACAS_HPP
#define XEUS_YACAS_HPP

#include <memory>
#include <sstream>
#include <string>

#include <xeus/xinterpreter.hpp>

#include "yacas/yacas.h"

class YacasInterpreter : public xeus::xinterpreter {
public:
    YacasInterpreter(const std::string& scripts_path);
    virtual ~YacasInterpreter() = default;

private:
    void configure_impl() override;

    nl::json execute_request_impl(int execution_counter,
                                  const std::string& code,
                                  bool silent,
                                  bool store_history,
                                  nl::json user_expressions,
                                  bool allow_stdin) override;

    nl::json complete_request_impl(const std::string& code,
                                   int cursor_pos) override;

    nl::json inspect_request_impl(const std::string& code,
                                  int cursor_pos,
                                  int detail_level) override;

    nl::json is_complete_request_impl(const std::string& code) override;

    nl::json kernel_info_request_impl() override;

    void shutdown_request_impl() override;

    std::string m_scripts_path;
    std::unique_ptr<std::ostringstream> m_side_effects;
    std::unique_ptr<CYacas> m_yacas;
};

#endif