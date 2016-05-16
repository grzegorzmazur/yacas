#include "yacasrequest.h"

YacasRequest::YacasRequest(QString expr, QObject *parent):
    QObject(parent),
    _expr(expr),
    _state(WAITING)
{
}

YacasRequest::State YacasRequest::state() const
{
    return _state;
}

QString YacasRequest::take()
{
    _state = BUSY;
    emit state_changed(_state);
    return _expr;
}

void YacasRequest::answer(unsigned idx, ResultType type, QString result, QString side_effects)
{
    _idx = idx;
    _result_type = type;
    _result = result;
    _side_effects = side_effects;

    _state = READY;

    emit state_changed(_state);
}

YacasRequest::ResultType YacasRequest::result_type() const
{
    return _result_type;
}

QString YacasRequest::result() const
{
    return _result;
}

QString YacasRequest::side_effects() const
{
    return _side_effects;
}

