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
 * File:   yacas_engine.hpp
 * Author: mazur
 *
 * Created on November 7, 2015, 12:52 PM
 */

#ifndef YACAS_ENGINE_HPP
#define YACAS_ENGINE_HPP

#include "yacas/yacas.h"

#include <zmqpp/zmqpp.hpp>

#include <atomic>
#include <condition_variable>
#include <deque>
#include <mutex>
#include <string>
#include <sstream>
#include <thread>

class YacasEngine {
public:
    YacasEngine(
            const std::string& scripts_path,
            const zmqpp::context& ctx,
            const std::string& endpoint = "inproc://engine");
    
    ~YacasEngine();
    
    void submit(unsigned long id, const std::string& expr);
    
private:
    void _worker();
    
    std::ostringstream _side_effects;
    CYacas _yacas;

    struct TaskInfo {
        unsigned long id;
        std::string expr;
    };
    
    std::deque<TaskInfo> _tasks;
    
    std::mutex _mtx;
    std::condition_variable _cv;

    std::thread* _worker_thread;
    
    zmqpp::socket _socket;
    
    std::atomic<bool> _shutdown;
};


#endif

