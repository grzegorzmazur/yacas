#ifndef YACASREQUEST_H
#define YACASREQUEST_H

#include <QObject>

class YacasRequest : public QObject
{
    Q_OBJECT
public:
    explicit YacasRequest(QString expr, QObject *parent = 0);

    enum State {
        WAITING,
        BUSY,
        READY
    };

    enum ResultType {
        EXPRESSION,
        PLOT2D,
        PLOT3D,
        GRAPH,
        ERROR
    };

    State state() const;

    QString take();
    void answer(unsigned idx, ResultType type, QString result, QString side_effects);

    ResultType result_type() const;
    QString result() const;
    QString side_effects() const;

signals:

    void state_changed(YacasRequest::State);

public slots:

private:
    QString _expr;
    State _state;
    unsigned _idx;
    ResultType _result_type;
    QString _result;
    QString _side_effects;
};

Q_DECLARE_METATYPE(YacasRequest::State)

#endif // YACASREQUEST_H
