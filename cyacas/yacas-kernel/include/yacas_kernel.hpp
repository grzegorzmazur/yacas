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
 * File:   yacas_kernel.hpp
 * Author: mazur
 *
 * Created on November 6, 2015, 3:10 PM
 */

#ifndef YACAS_KERNEL_HPP
#define YACAS_KERNEL_HPP

#include "hmac_sha256.hpp"
#include "yacas_engine.hpp"

#include <boost/uuid/random_generator.hpp>
#include <jsoncpp/json/json.h>
#include <zmqpp/zmqpp.hpp>

#include <map>
#include <sstream>

class YacasKernel: NonCopyable {
public:
    YacasKernel(const std::string& scripts_path, const Json::Value&);

    void run();
    
private:
    class Session: NonCopyable {
    public:
        Session(const std::string& key);
        
        const HMAC_SHA256& auth() const { return _auth; };
        const boost::uuids::uuid& uuid() const { return _uuid; };

        boost::uuids::uuid generate_msg_uuid() const { return _uuid_gen(); }
    
    private:
        HMAC_SHA256 _auth;

        mutable boost::uuids::random_generator _uuid_gen;
        boost::uuids::uuid _uuid;
    };
    
    class Request: NonCopyable {
    public:
        Request(const Session& session, const zmqpp::message& msg);

        const Json::Value& header() const { return _header; }
        const Json::Value& content() const { return _content; }
        
        void reply(zmqpp::socket&, const std::string& type, const Json::Value& content) const;
        
    private:
        const Session& _session;
        Json::Value _header;
        Json::Value _content;
        Json::Value _metadata;
        std::string _identities_buf;
    };
    
    void _handle_shell(const std::shared_ptr<Request>& request);
    void _handle_engine(const zmqpp::message& msg);
    
    Session _session;
    
    zmqpp::context _ctx;
    
    zmqpp::socket _hb_socket;
    zmqpp::socket _iopub_socket;
    zmqpp::socket _control_socket;
    zmqpp::socket _stdin_socket;
    zmqpp::socket _shell_socket;
    
    zmqpp::socket _engine_socket;
    
    unsigned long _execution_count;
    
    YacasEngine _engine;
    
    bool _tex_output;
    std::stringstream _side_effects;
    CYacas _yacas;

    std::map<unsigned long, std::shared_ptr<Request> > _execute_requests;
    
    bool _shutdown;
};

#endif
