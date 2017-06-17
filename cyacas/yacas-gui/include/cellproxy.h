#ifndef CELLPROXY_H
#define	CELLPROXY_H

#include <QtCore/QObject>
#include <QtWebEngineWidgets/QWebEnginePage>

#include "yacasrequest.h"
#include "yacasserver.h"

class CellProxy: public QObject {
    Q_OBJECT
public:
    CellProxy(QWebEnginePage* page, int idx, QString expr, YacasServer& yacas_server, CYacas& yacas2tex, QObject* = 0);
    ~CellProxy();
    
public slots:
    void on_request_state_changed(YacasRequest::State);
    
private:
    QWebEnginePage* _page;
    const int _idx;
    QString _expr;
    YacasServer& _yacas_server;
    CYacas& _yacas2tex;

    YacasRequest* _request;
};

#endif	/* CELLPROXY_H */

