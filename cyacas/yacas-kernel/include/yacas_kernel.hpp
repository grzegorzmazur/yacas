/*
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Yacas is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas. If not, see <http://www.gnu.org/licenses/>.
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
#include <json/json.h>
#include <zmq.hpp>
#include <zmq_addon.hpp>
#include <map>
#include <sstream>

class YacasKernel : NonCopyable {
public:
    YacasKernel(const std::string& scripts_path, const Json::Value&);

    void run();

private:
    class Session : NonCopyable {
    public:
        explicit Session(const std::string& key);

        const HMAC_SHA256& auth() const { return _auth; };
        const boost::uuids::uuid& uuid() const { return _uuid; };

        boost::uuids::uuid generate_msg_uuid() const { return _uuid_gen(); }

    private:
        HMAC_SHA256 _auth;

        mutable boost::uuids::random_generator _uuid_gen;
        boost::uuids::uuid _uuid;
    };

    class Request : NonCopyable {
    public:
        Request(const Session& session, const zmq::multipart_t& msg);

        const Json::Value& header() const { return _header; }
        const Json::Value& content() const { return _content; }

        void reply(zmq::socket_t&,
                   const std::string& type,
                   const Json::Value& content) const;

    private:
        const Session& _session;
        Json::Value _header;
        Json::Value _content;
        Json::Value _metadata;
        std::string _identities_buf;
    };

    void _handle_shell(const std::shared_ptr<Request>& request);
    void _handle_engine(const zmq::multipart_t& msg);

    Session _session;

    zmq::context_t _ctx;

    zmq::socket_t _hb_socket;
    zmq::socket_t _iopub_socket;
    zmq::socket_t _control_socket;
    zmq::socket_t _stdin_socket;
    zmq::socket_t _shell_socket;

    zmq::socket_t _engine_socket;

    unsigned long _execution_count;

    YacasEngine _engine;

    bool _tex_output;
    std::stringstream _side_effects;
    CYacas _yacas;

    std::map<unsigned long, std::shared_ptr<Request>> _execute_requests;

    bool _shutdown;
};

#endif
