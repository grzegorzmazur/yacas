#include "xeus-yacas/xeus-yacas.hpp"

#include <regex>
#include <set>

#include <xeus/xhelper.hpp>
#include <xtl/xbase64.hpp>

YacasInterpreter::YacasInterpreter(const std::string& scripts_path) :
    m_scripts_path(scripts_path)
{
}

void YacasInterpreter::configure_impl()
{
    m_side_effects = std::make_unique<std::ostringstream>();
    m_yacas = std::make_unique<CYacas>(*m_side_effects.get());

    m_yacas->Evaluate(std::string("DefaultDirectory(\"") + m_scripts_path +
                      std::string("\");"));
    m_yacas->Evaluate("Load(\"yacasinit.ys\");");
    m_yacas->Evaluate("Plot2D'outputs();");
    m_yacas->Evaluate("UnProtect(Plot2D'outputs);");
    m_yacas->Evaluate("Plot2D'outputs() := {{\"default\", \"png\"}, {\"png\", "
                      "\"Plot2D'png\"}}");
    m_yacas->Evaluate("Protect(Plot2D'outputs);");
    m_yacas->Evaluate("Plot3DS'outputs();");
    m_yacas->Evaluate("UnProtect(Plot3DS'outputs);");
    m_yacas->Evaluate("Plot3DS'outputs() := {{\"default\", \"png\"}, {\"png\", "
                      "\"Plot3DS'png\"}}");
    m_yacas->Evaluate("Protect(Plot3DS'outputs);");
}

nl::json YacasInterpreter::execute_request_impl(int execution_counter,
                                                const std::string& code,
                                                bool silent,
                                                bool store_history,
                                                nl::json user_expressions,
                                                bool allow_stdin)
{
    m_side_effects->str("");
    m_side_effects->clear();

    const std::string code_block{'[' + code + (code.back() != ';' ? ";" : "") +
                                 ']'};
    m_yacas->Evaluate(code_block);

    if (m_yacas->IsError()) {
        publish_execution_error("Error", m_yacas->Error(), {});
        // publish_execution_error() does not print the error message to stderr,
        // so we do it ourselves
        publish_stream("stderr", m_yacas->Error());
        const nl::json error{{"status", "error"},
                             {"ename", "Error"},
                             {"evalue", m_yacas->Error()},
                             {"traceback", nl::json::array()}};
        return error;
    } else {
        std::string text_result = m_yacas->Result();
        if (!text_result.empty() && text_result.back() == ';')
            text_result.pop_back();

        nl::json pub_data{{"text/plain", text_result}};

        const std::string side_effects{m_side_effects->str()};

        m_side_effects->clear();
        m_side_effects->str("");

        if (!side_effects.empty())
            publish_stream("stdout", side_effects);

        std::regex rx("File\\(\"([^\"]+)\", *\"([^\"]+)\"\\)",
                      std::regex_constants::ECMAScript);
        std::smatch m;
        if (std::regex_match(text_result, m, rx)) {
            std::ifstream f(m[1], std::ios_base::in | std::ios_base::binary);
            const std::string img{
                std::istreambuf_iterator<char>(f),
                std::istreambuf_iterator<char>()};
            pub_data[m[2]] = xtl::base64encode(img);
        } else {
            m_yacas->Evaluate(std::string("TeXForm(Hold(") + text_result +
                              "));");
            std::string tex_result = m_yacas->Result();
            tex_result = tex_result.substr(1, tex_result.size() - 3);
            pub_data["text/latex"] = tex_result;
        }

        publish_execution_result(
            execution_counter, std::move(pub_data), nl::json::object());

        const nl::json result{{"status", "ok"},
                              {"execution_count", execution_counter},
                              {"payload", nl::json::array()},
                              {"user_expressions", nl::json::object()}};
        return result;
    }
}

nl::json YacasInterpreter::complete_request_impl(const std::string& code,
                                                 int cursor)
{
    int start = cursor;
    while (start > 0 && std::isalpha(code[start - 1]))
        start -= 1;
    const std::string prefix = code.substr(start, cursor - start);

    std::set<std::string> matches;

    for (auto op : m_yacas->getDefEnv().getEnv().PreFix())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    for (auto op : m_yacas->getDefEnv().getEnv().InFix())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    for (auto op : m_yacas->getDefEnv().getEnv().PostFix())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    for (auto op : m_yacas->getDefEnv().getEnv().Bodied())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    for (auto op : m_yacas->getDefEnv().getEnv().CoreCommands())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    for (auto op : m_yacas->getDefEnv().getEnv().UserFunctions())
        if (op.first->compare(0, prefix.length(), prefix) == 0)
            matches.insert(*op.first);

    return xeus::create_complete_reply(matches, start, cursor);
}

nl::json YacasInterpreter::inspect_request_impl(const std::string& code,
                                                int cursor_pos,
                                                int detail_level)
{
    return xeus::create_inspect_reply();
}

nl::json YacasInterpreter::is_complete_request_impl(const std::string& code)
{
    return xeus::create_is_complete_reply("unknown");
}

nl::json YacasInterpreter::kernel_info_request_impl()
{
    const nl::json language_info{{"file_extension", ".ys"},
                                 {"mimetype", "text/x-yacas"},
                                 {"name", "yacas"},
                                 {"version", "24.07"},
                                 {"codemirror_mode", {{"name", "yacas"}}}};

    const nl::json help_links{
        {{"text", "Yacas Homepage"}, {"url", "http://www.yacas.org"}},
        {{"text", "Yacas Documentation"},
         {"url", "http://yacas.readthedocs.org"}}};

    const nl::json result{{"implementation", "yacas_kernel"},
                          {"implementation_version", "24.07"},
                          {"language_info", language_info},
                          {"banner", "yacas_kernel"}, // YACAS_VERSION},
                          {"help_links", help_links}};

    return result;
}

void YacasInterpreter::shutdown_request_impl()
{
    m_yacas.reset();
    m_side_effects.reset();
}