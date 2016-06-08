/*
 * This file is part of yacas_kernel.
 * Yacas_kernel is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Yacas_kernel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas_kernel.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 
 * File:   yacas_kernel.cpp
 * Author: mazur
 *
 * Created on November 6, 2015, 3:10 PM
 */

#include "yacas_kernel.hpp"
#include "yacas/yacas_version.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <set>

namespace {
    std::string now()
    {
        using namespace boost::posix_time;

        return to_iso_extended_string(microsec_clock::local_time());
    }
}

YacasKernel::YacasKernel(const std::string& scripts_path, const Json::Value& config):
    _session(config["key"].asString()),
    _hb_socket(_ctx, zmqpp::socket_type::reply),
    _iopub_socket(_ctx, zmqpp::socket_type::publish),
    _control_socket(_ctx, zmqpp::socket_type::router),
    _stdin_socket(_ctx, zmqpp::socket_type::router),
    _shell_socket(_ctx, zmqpp::socket_type::router),
    _engine_socket(_ctx, zmqpp::socket_type::pair),
    _execution_count(1),
    _engine(scripts_path, _ctx, "inproc://engine"),
    _tex_output(true),
    _yacas(_side_effects),
    _shutdown(false)
{
    const std::string transport = config["transport"].asString();
    const std::string ip = config["ip"].asString();

    _hb_socket.bind(transport + "://" + ip + ":" + config["hb_port"].asString());
    _iopub_socket.bind(transport + "://" + ip + ":" + config["iopub_port"].asString());
    _control_socket.bind(transport + "://" + ip + ":" + config["control_port"].asString());
    _stdin_socket.bind(transport + "://" + ip + ":" + config["stdin_port"].asString());
    _shell_socket.bind(transport + "://" + ip + ":" + config["shell_port"].asString());
    _engine_socket.bind("inproc://engine");
    
    _yacas.Evaluate(std::string("DefaultDirectory(\"") + scripts_path + std::string("\");"));
    _yacas.Evaluate("Load(\"yacasinit.ys\");");
}

void YacasKernel::run()
{
    zmqpp::poller poller;
    
    poller.add(_hb_socket);
    poller.add(_control_socket);
    poller.add(_stdin_socket);
    poller.add(_shell_socket);
    poller.add(_iopub_socket);
    poller.add(_engine_socket);
    
    for (;;) {
        poller.poll();
        
        if (poller.has_input(_hb_socket)) {
            zmqpp::message msg;
            _hb_socket.receive(msg);
            _hb_socket.send(msg);
        }

        if (poller.has_input(_shell_socket)) {
            zmqpp::message msg;
            _shell_socket.receive(msg);
            _handle_shell(std::make_shared<Request>(_session, msg));
        }

        if (poller.has_input(_control_socket)) {
            zmqpp::message msg;
            _control_socket.receive(msg);
            Request request(_session, msg);
            
            if (request.header()["msg_type"].asString() == "shutdown_request")
                _shutdown = true;
        }

        if (_shutdown)
            return;


        if (poller.has_input(_stdin_socket)) {
            zmqpp::message msg;
            _stdin_socket.receive(msg);
        }
        
        if (poller.has_input(_engine_socket)) {
            zmqpp::message msg;
            _engine_socket.receive(msg);
            _handle_engine(msg);
        }
    }
}

YacasKernel::Session::Session(const std::string& key):
    _auth(key),
    _uuid(_uuid_gen())
{
}


YacasKernel::Request::Request(const Session& session, const zmqpp::message& msg):
    _session(session)
{
    std::string header_buf;
    msg.get(header_buf, 3);
    std::string parent_header_buf;
    msg.get(parent_header_buf, 4);
    std::string metadata_buf;
    msg.get(metadata_buf, 5);
    std::string content_buf;
    msg.get(content_buf, 6);

    HMAC_SHA256 auth(_session.auth());
    
    auth.update(header_buf);
    auth.update(parent_header_buf);
    auth.update(metadata_buf);
    auth.update(content_buf);

    std::string signature_buf;
    msg.get(signature_buf, 2);
 
    if (auth.hexdigest() != signature_buf)
        throw std::runtime_error("invalid signature");

    msg.get(_identities_buf, 0);
    
    Json::Reader reader;

    reader.parse(header_buf, _header);
    reader.parse(content_buf, _content);
    reader.parse(metadata_buf, _metadata);
}

void YacasKernel::Request::reply(zmqpp::socket& socket, const std::string& msg_type, const Json::Value& content) const
{
    Json::Value header;
    header["username"] = "kernel";
    header["version"] = "5.0";
    header["session"] = boost::uuids::to_string(_session.uuid());
    header["date"]  = now();
    header["msg_id"] = boost::uuids::to_string(_session.generate_msg_uuid());
    header["msg_type"] = msg_type;

    Json::StreamWriterBuilder builder;

    const std::string content_buf = Json::writeString(builder, content);
    // FIXME:
    const std::string metadata_buf = "{}";
    const std::string header_buf = Json::writeString(builder, header);
    const std::string parent_header_buf = Json::writeString(builder, _header);
    
    HMAC_SHA256 auth(_session.auth());
    
    auth.update(header_buf);
    auth.update(parent_header_buf);
    auth.update(metadata_buf);
    auth.update(content_buf);
    
    zmqpp::message msg;
    msg.add(_identities_buf);
    msg.add("<IDS|MSG>");
    msg.add(auth.hexdigest());
    msg.add(header_buf);
    msg.add(parent_header_buf);
    msg.add(metadata_buf);
    msg.add(content_buf);
    
    socket.send(msg);
}

void YacasKernel::_handle_shell(const std::shared_ptr<Request>& request)
{
    const std::string msg_type = request->header()["msg_type"].asString();
    
    if (msg_type == "kernel_info_request") {
        Json::Value language_info;
        language_info["name"] = "yacas";
        language_info["version"] = "1.3.6";
        language_info["mimetype"] = "text/x-yacas";
        language_info["file_extension"] = ".ys";

        Json::Value homepage;
        homepage["text"] = "Yacas Homepage";
        homepage["url"] = "http://www.yacas.org";

        Json::Value docs;
        docs["text"] = "Yacas Documentation";
        docs["url"] = "http://yacas.readthedocs.org";
        
        Json::Value help_links;
        help_links.append(homepage);
        help_links.append(docs);
        
        Json::Value reply_content;
        reply_content["protocol_version"] = "5.1";
        reply_content["implementation"] = "yacas_kernel";
        reply_content["implementation_version"] = "0.1";
        reply_content["language_info"] = language_info;
        reply_content["banner"] = "yacas_kernel " YACAS_VERSION;
        reply_content["help_links"] = help_links;

        request->reply(_shell_socket, "kernel_info_reply", reply_content);
    } else if (msg_type == "execute_request") {

        _execute_requests.insert(std::make_pair(_execution_count, std::move(request)));
        _engine.submit(_execution_count, request->content()["code"].asString());
        
        _execution_count += 1;
    } else if (msg_type == "complete_request") {
        std::string code = request->content()["code"].asString();
        int cursor = request->content()["cursor_pos"].asInt();
        int start = cursor;
        while (start > 0 && std::isalpha(code[start - 1]))
            start -= 1;
        const std::string prefix = code.substr(start, cursor - start);
        
        std::set<std::string> matches;
        
        for (auto op: _yacas.getDefEnv().getEnv().PreFix())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);
 
        for (auto op: _yacas.getDefEnv().getEnv().InFix())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);
        
        for (auto op: _yacas.getDefEnv().getEnv().PostFix())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);
        
        for (auto op: _yacas.getDefEnv().getEnv().Bodied())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);
        
        for (auto op: _yacas.getDefEnv().getEnv().CoreCommands())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);

        for (auto op: _yacas.getDefEnv().getEnv().UserFunctions())
            if (op.first->compare(0, prefix.length(), prefix) == 0)
                matches.insert(*op.first);

        Json::Value reply_content_matches;
        for (const std::string& match: matches)
            reply_content_matches.append(match);

        
        Json::Value reply_content;
        reply_content["status"] = "ok";
        reply_content["cursor_start"] = start;
        reply_content["cursor_end"] = cursor;
        reply_content["matches"] = reply_content_matches;
        
        request->reply(_shell_socket, "complete_reply", reply_content);

    } else if (msg_type == "shutdown_request") {

        _shutdown = true;

        Json::Value reply_content;
        reply_content["status"] = "ok";

        request->reply(_shell_socket, "shutdown_reply", reply_content);
    }
}

void YacasKernel::_handle_engine(const zmqpp::message& msg)
{
    std::string msg_type;
    msg.get(msg_type, 0);
    
    std::string content_buf;
    msg.get(content_buf, 1);

    Json::Value content;
    Json::Reader().parse(content_buf, content);

    std::shared_ptr<YacasKernel::Request> request = _execute_requests[content["id"].asUInt64()];
    
    bool condemned = false;
    
    if (msg_type == "calculate") {
        Json::Value status_content;
        status_content["execution_state"] = "busy";

        request->reply(_iopub_socket, "status", status_content);

        Json::Value execute_input_content;
        execute_input_content["execution_count"] = request->content()["id"];
        execute_input_content["code"] = request->content()["expr"];

        request->reply(_iopub_socket, "execute_input", execute_input_content);
    } else if (msg_type == "result") {
        
        if (content.isMember("side_effects")) {
            Json::Value stream_content;
            stream_content["name"] = "stdout";
            stream_content["text"] = content["side_effects"];

            request->reply(_iopub_socket, "stream", stream_content);
        }

        if (content.isMember("error")) {
            Json::Value reply_content;
            reply_content["status"] =  "error";
            reply_content["execution_count"] = content["id"];
            reply_content["ename"] = Json::Value();
            reply_content["evalue"] = Json::Value();
            reply_content["traceback"].append(content["error"]);

            request->reply(_shell_socket, "execute_reply", reply_content);

            Json::Value error_content;
            error_content["execution_count"] = content["id"];
            error_content["ename"] = Json::Value();
            error_content["evalue"] = Json::Value();
            error_content["traceback"].append(content["error"]);
            
            request->reply(_iopub_socket, "error", error_content);
        } else {
            std::string text_result = content["result"].asString();
            if (text_result.back() == ';')
                text_result.pop_back();
            
            Json::Value content_data;
            content_data["text/plain"] = text_result;

            if (_tex_output) {
                _side_effects.clear();
                _side_effects.str("");

                _yacas.Evaluate(std::string("TeXForm(Hold(") + text_result + "));");

                std::string tex_result = _yacas.Result();
                tex_result = tex_result.substr(1, tex_result.size() - 3);

                content_data["text/latex"] = tex_result;
            }
            
            Json::Value reply_content;
            reply_content["status"] =  "ok";
            reply_content["execution_count"] = content["id"];
            reply_content["data"] = content_data;

            request->reply(_shell_socket, "execute_result", reply_content);

            Json::Value result_content;
            result_content["execution_count"] = content["id"];
            result_content["data"] = content_data;
            result_content["metadata"] = "{}";
            
            request->reply(_iopub_socket, "execute_result", result_content);
            
            condemned = true;
        }

        Json::Value status_content;
        status_content["execution_state"] = "idle";

        request->reply(_iopub_socket, "status", status_content);
        
        if (condemned)
            _execute_requests.erase(content["id"].asUInt64());
    }
}
