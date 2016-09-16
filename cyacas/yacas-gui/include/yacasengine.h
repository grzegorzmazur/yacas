#ifndef YACASENGINE_H
#define YACASENGINE_H

#include <QtCore/QObject>

#include <QtCore/QMutex>
#include <QtCore/QQueue>
#include <QtCore/QWaitCondition>
#include <QtCore/QStringList>

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
    
    QStringList symbols() const;
    
public slots:
    void on_start_processing();

signals:
    void busy(bool);
    
private:
    void _update_symbols();
    
    YacasRequestQueue& _requests;
    CYacas* _yacas;
    std::ostringstream _side_effects;
    unsigned _idx;
    
    QStringList _symbols;
    mutable QMutex _symbols_mtx;
};

#endif // YACASENGINE_H
