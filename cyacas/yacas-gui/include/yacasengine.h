#ifndef YACASENGINE_H
#define YACASENGINE_H

#include <QObject>

#include <QMutex>
#include <QQueue>
#include <QWaitCondition>

#include <sstream>

#include "yacasrequest.h"

#include "yacas/yacas.h"

struct YacasRequestQueue {
    QMutex mtx;
    QWaitCondition cnd;
    QQueue<YacasRequest*> waiting;
    bool shutdown;
};

class YacasEngine: public QObject
{
    Q_OBJECT
public:
    explicit YacasEngine(const QString& scripts_path, YacasRequestQueue& requests, QObject* = 0);
    ~YacasEngine();

    void cancel();
    
public slots:
    void on_start_processing();

signals:
    void busy(bool);
    
private:
    YacasRequestQueue& _requests;
    CYacas* _yacas;
    std::ostringstream _side_effects;
    unsigned _idx;
};

#endif // YACASENGINE_H
