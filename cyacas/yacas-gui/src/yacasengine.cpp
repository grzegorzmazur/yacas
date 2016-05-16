#include "yacasengine.h"

#include <string>

#include <QtCore/QMutexLocker>
#include <QtCore/QFile>

YacasEngine::YacasEngine(const QString& scripts_path, YacasRequestQueue& requests, QObject* parent):
    QObject(parent),
    _requests(requests),
    _yacas(new CYacas(_side_effects)),
    _idx(1)
{
    if (!QFile(scripts_path + "yacasinit.ys").exists())
        throw std::runtime_error(QString("Invalid yacas scripts path: %1").arg(scripts_path).toStdString());

    _yacas->Evaluate(std::string("DefaultDirectory(\"") + scripts_path.toStdString() + std::string("\");"));
    _yacas->Evaluate("Load(\"yacasinit.ys\");");

    _yacas->Evaluate("Plot2D'outputs();");
    _yacas->Evaluate("UnProtect(Plot2D'outputs);");
    _yacas->Evaluate("Plot2D'yagy(values_IsList, _options'hash) <-- Yagy'Plot2D'Data(values, options'hash);");
    _yacas->Evaluate("Plot2D'outputs() := { {\"default\", \"yagy\"}, {\"data\", \"Plot2D'data\"}, {\"gnuplot\", \"Plot2D'gnuplot\"}, {\"java\", \"Plot2D'java\"}, {\"yagy\", \"Plot2D'yagy\"}, };");
    _yacas->Evaluate("Protect(Plot2D'outputs);");
    _yacas->Evaluate("Plot3DS'outputs();");
    _yacas->Evaluate("UnProtect(Plot3DS'outputs);");
    _yacas->Evaluate("Plot3DS'yagy(values_IsList, _options'hash) <-- Yagy'Plot3DS'Data(values, options'hash);");
    _yacas->Evaluate("Plot3DS'outputs() := { {\"default\", \"yagy\"}, {\"data\", \"Plot3DS'data\"}, {\"gnuplot\", \"Plot3DS'gnuplot\"}, {\"yagy\", \"Plot3DS'yagy\"},};");
    _yacas->Evaluate("Protect(Plot3DS'outputs);");
}

YacasEngine::~YacasEngine()
{
    delete _yacas;
}

void YacasEngine::cancel()
{
    _yacas->getDefEnv().getEnv().stop_evaluation = true;
}

void YacasEngine::on_start_processing()
{
    for (;;) {

        QMutexLocker lock(&_requests.mtx);

        if (_requests.shutdown)
            return;

        busy(false);
        
        _requests.cnd.wait(&_requests.mtx);

        if (_requests.shutdown)
            return;

        _yacas->getDefEnv().getEnv().stop_evaluation = false;
        
        busy(true);
        
        while (!_requests.waiting.empty()) {
            YacasRequest* request = _requests.waiting.dequeue();

            // Beware of low flying butterflies
            _requests.mtx.unlock();

            const QString expr = request->take();
            
            _side_effects.clear();
            _side_effects.str("");
            _yacas->Evaluate((expr + ";").toStdString());

            if (!_yacas->IsError()) {
                QString result = QString::fromStdString(_yacas->Result());
                result = result.left(result.length() - 1).trimmed();
                
                YacasRequest::ResultType result_type = YacasRequest::EXPRESSION;
                if (result.startsWith("Yagy'Plot2D'Data")) {
                    result_type = YacasRequest::PLOT2D;
                    result = result.remove("Yagy'Plot2D'Data(");
                    result.truncate(result.length() - 1);
                } else if (result.startsWith("Yagy'Plot3DS'Data")) {
                    result_type = YacasRequest::PLOT3D;
                    result = result.remove("Yagy'Plot3DS'Data(");
                    result.truncate(result.length() - 1);
                } else if (result.startsWith("Graph(")) {
                    result_type = YacasRequest::GRAPH;
                    result = result.remove("Graph(");
                    result.truncate(result.length() - 1);
                }
                request->answer(_idx++, result_type, result, QString::fromStdString(_side_effects.str()));
            } else {
                QString msg = QString::fromStdString(_yacas->Error());
                request->answer(_idx++, YacasRequest::ERROR, msg.trimmed(), QString::fromStdString(_side_effects.str()));
            }

            _requests.mtx.lock();
        }
    }
}
