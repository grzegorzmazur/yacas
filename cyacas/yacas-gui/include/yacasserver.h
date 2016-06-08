#ifndef YACASSERVER_H
#define YACASSERVER_H

#include <QObject>
#include <QThread>

#include "yacasengine.h"

class YacasServer: public QObject
{
    Q_OBJECT
public:
    explicit YacasServer(const QString& scripts_path, QObject* parent = 0);
    ~YacasServer();

    void submit(YacasRequest*);
    void cancel();

signals:
    void start_processing();
    void interrupt();
    void busy(bool);

public slots:
    void on_engine_busy(bool);
    
private:
    YacasRequestQueue _requests;
    YacasEngine* _engine;
    QThread _engine_thread;
};

#endif
