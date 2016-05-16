#include "yacasserver.h"

#include <QDebug>
#include <QMutexLocker>

YacasServer::YacasServer(const QString& scripts_path, QObject *parent) :
    QObject(parent)
{
    _requests.shutdown = false;

    _engine = new YacasEngine(scripts_path, _requests);
    _engine->moveToThread(&_engine_thread);
    _engine_thread.start();
    connect(&_engine_thread, SIGNAL(finished()), _engine, SLOT(deleteLater()));
    connect(this, SIGNAL(start_processing()), _engine, SLOT(on_start_processing()));
    connect(_engine, SIGNAL(busy(bool)), this, SLOT(on_engine_busy(bool)));
    
    emit start_processing();
}

YacasServer::~YacasServer()
{
    _requests.shutdown = true;
    _requests.cnd.wakeAll();

    _engine_thread.quit();
    _engine_thread.wait();
}

void YacasServer::submit(YacasRequest* request)
{
    QMutexLocker lock(&_requests.mtx);
    _requests.waiting.enqueue(request);
    _requests.cnd.wakeAll();
}

void YacasServer::cancel()
{
    _engine->cancel();
}

void YacasServer::on_engine_busy(bool b)
{
    busy(b);
}