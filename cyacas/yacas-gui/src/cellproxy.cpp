#include "cellproxy.h"

#include <QtCore/QVariant>
#include <QtCore/QJsonDocument>

CellProxy::CellProxy(QWebFrame* frame, int idx, QString expr, YacasServer& yacas_server, CYacas& yacas2tex, QObject* parent):
    QObject(parent),
    _frame(frame),
    _idx(idx),
    _expr(expr),
    _yacas_server(yacas_server),
    _yacas2tex(yacas2tex),
    _request(new YacasRequest(expr.trimmed().isEmpty() ? "True;" : (expr.endsWith(";") ? "[" + expr + "];" : "[" + expr + ";];")))
{
    connect(_request, SIGNAL(state_changed(YacasRequest::State)), this, SLOT(on_request_state_changed(YacasRequest::State)));
    _yacas_server.submit(_request);
}

CellProxy::~CellProxy()
{
    delete _request;
}

void CellProxy::on_request_state_changed(YacasRequest::State state)
{
    if (state == YacasRequest::READY) {
        QVariantMap evaluation_result;
        evaluation_result["idx"] = _idx;
        evaluation_result["input"] = _expr;
        
        if (!_request->side_effects().isEmpty())
            evaluation_result["side_effects"] = _request->side_effects();
        
        switch (_request->result_type()) {
            case YacasRequest::EXPRESSION: {
                const QString result = _request->result().trimmed();
                const QString texform_expr = QString("TeXForm(Hold(") + result + "));";
                _yacas2tex.Evaluate(texform_expr.toStdString());
                const QString texform_result = QString::fromStdString(_yacas2tex.Result());
                const QString tex_code =
                    texform_result.trimmed().mid(2, texform_result.length() - 5);
                evaluation_result["type"] = "Expression";
                evaluation_result["expression"] = result;
                evaluation_result["tex_code"] = tex_code;

                bool is_number = false;
                bool is_constant = false;
                bool is_vector = false;
                bool is_matrix = false;
                bool is_square_matrix = false;
                
                _yacas2tex.Evaluate((QString("IsNumber(Hold(") + result + "));").toStdString());
                if (_yacas2tex.Result() == "True;")
                    is_number = true;
                _yacas2tex.Evaluate((QString("IsConstant(Hold(") + result + "));").toStdString());
                if (_yacas2tex.Result() == "True;")
                    is_constant = true;
                 _yacas2tex.Evaluate((QString("IsVector(Hold(") + result + "));").toStdString());
                if (_yacas2tex.Result() == "True;")
                    is_vector = true;
                 _yacas2tex.Evaluate((QString("IsMatrix(Hold(") + result + "));").toStdString());
                if (_yacas2tex.Result() == "True;")
                    is_matrix = true;
                _yacas2tex.Evaluate((QString("IsSquareMatrix(Hold(") + result + "));").toStdString());
                if (_yacas2tex.Result() == "True;")
                    is_square_matrix = true;

                evaluation_result["expression_type"] = "function";

                if (is_number)
                    evaluation_result["expression_type"] = "number";
                else if (is_constant && !(is_vector || is_matrix))
                    evaluation_result["expression_type"] = "constant";
                else if (is_vector)
                    evaluation_result["expression_type"] = "vector";
                else if (is_square_matrix)
                    evaluation_result["expression_type"] = "square_matrix";
                else if (is_matrix)
                    evaluation_result["expression_type"] = "matrix";

                evaluation_result["variables"] = QStringList();
                
                _yacas2tex.Evaluate((QString("VarList(Hold(") + result + "));").toStdString());
                QString vars = QString::fromStdString(_yacas2tex.Result());
                vars = vars.remove("{");
                vars.truncate(vars.length() - 2);
                if (vars.length() != 0)
                    evaluation_result["variables"] = vars.split(",");
                
                break;
            }
            case YacasRequest::PLOT2D: {
                
                QList<QVariant> data;

                const QString result = _request->result().trimmed();
                QStringList parts = result.split("}},{{");

                QString options_string = parts.takeLast().trimmed();
                options_string.truncate(options_string.length() - 2);

                QRegExp dict_entry_rx("(\"[^\"]+\"),(.+)");
                QRegExp number_list_rx("\\{([^,\\}]+)(?:,([^,\\}]+))*\\}");
                QRegExp split_string_list_rx("\",(?=(?:[^\\\"\"]*\\\"\"[^\\\"\"]*\\\"\")*(?![^\\\"\"]*\\\"\"))\"");

                QStringList labels;

                foreach (QString os, options_string.split("},{")) {

                    dict_entry_rx.exactMatch(os);

                    if (dict_entry_rx.cap(1) == "\"xrange\"") {
                        number_list_rx.exactMatch(dict_entry_rx.cap(2));
                    }

                    if (dict_entry_rx.cap(1) == "\"yname\"") {
                        QString s = dict_entry_rx.cap(2);
                        s.remove(0, 2);
                        s.chop(2);
                        labels = s.split(split_string_list_rx);
                    }
                }

                parts = parts.replaceInStrings("{{{", "");
                parts = parts.replaceInStrings("}}}", "");

                for (int i = 0; i < parts.size(); ++i) {

                    QList<QVariant> partial_data;

                    foreach (const QString& ss, parts[i].split("},{")) {
                        QList<QVariant> p;
                        foreach (QString s, ss.split(",")) {
                            p.append(s.replace("{", "").replace("}","").toDouble());
                        }
                        partial_data.append(QVariant(p));
                    }

                    QVariantMap data_entry;
                    data_entry["label"] = labels[i];
                    data_entry["data"] = partial_data;
                    data.append(data_entry);
                }

                evaluation_result["type"] = "Plot2D";
                evaluation_result["plot2d_data"] = data;

                
                break;
            }
            case YacasRequest::PLOT3D: {
                QList<QVariant> data;

                const QString result = _request->result().trimmed();

                QStringList parts = result.split("}},{{");

                QString options_string = parts.takeLast().trimmed();
                options_string.truncate(options_string.length() - 2);

                QRegExp dict_entry_rx("(\"[^\"]+\"),(.+)");
                QRegExp number_list_rx("\\{([^,\\}]+)(?:,([^,\\}]+))*\\}");
                QRegExp split_string_list_rx("\",(?=(?:[^\\\"\"]*\\\"\"[^\\\"\"]*\\\"\")*(?![^\\\"\"]*\\\"\"))\"");

                QStringList labels;

                foreach (QString os, options_string.split("},{")) {
                    dict_entry_rx.exactMatch(os);

                    if (dict_entry_rx.cap(1) == "\"zname\"") {
                        QString s = dict_entry_rx.cap(2);
                        s.remove(0, 2);
                        s.chop(2);
                        labels = s.split(split_string_list_rx);
                    }
                }

                parts = parts.replaceInStrings("{{{", "");
                parts = parts.replaceInStrings("}}}", "");

                for (int i = 0; i < parts.size(); ++i) {

                    QList<QVariant> partial_data;

                    foreach (const QString& ss, parts[i].split("},{")) {
                        QList<QVariant> p;
                        foreach (QString s, ss.split(",")) {
                            p.append(s.replace("{", "").replace("}","").toDouble());
                        }
                        partial_data.append(QVariant(p));
                    }

                    QVariantMap data_entry;
                    data_entry["label"] = labels[i];
                    data_entry["data"] = partial_data;
                    data.append(data_entry);
                }

                evaluation_result["type"] = "Plot3D";
                evaluation_result["plot3d_data"] = data;
                
                break;
            }

            case YacasRequest::GRAPH: {
                evaluation_result["type"] = "Graph";
                const QString result = _request->result().trimmed();
                QStringList parts = result.split("},{");
                const QStringList vertices = parts[0].replace("{", "").split(",");
                QVariantList edges;
                foreach (QString es, parts[1].replace("}", "").split(",")) {
                    bool bi = true;
                    QStringList ft = es.split("<->");
                    if (ft.length() == 1) {
                        ft = es.split("->");
                        bi = false;
                    }
                    QVariantMap edge;
                    edge["from"] = vertices.indexOf(ft[0]) + 1;
                    edge["to"] = vertices.indexOf(ft[1]) + 1;
                    edge["bi"] = bi;
                    
                    edges.append(edge);
                }
                
                evaluation_result["graph_vertices"] = vertices;
                evaluation_result["graph_edges"] = edges;
                
                break;
            }

            case YacasRequest::ERROR: {
                evaluation_result["type"] = "Error";
                evaluation_result["error_message"] = _request->result().trimmed();

                break;
            }
        }
        
        _frame->evaluateJavaScript(QString("printResults(") + (QJsonDocument::fromVariant(evaluation_result)).toJson() + ");");
        
        deleteLater();
    }
}
