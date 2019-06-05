#include "mainwindow.h"
#include "cellproxy.h"
#include "preferences_dialog.h"
#include "ui_mainwindow.h"

#include <QtCore/QDebug>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QJsonArray>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QList>
#include <QtCore/QUrl>
#include <QtCore/QVariant>

#include <QtGui/QDesktopServices>

#include <QtWebEngineWidgets/QtWebEngineWidgets>

#include <QtPrintSupport/QPrintDialog>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>

#include "yacas/yacas_version.h"

MainWindow::MainWindow(Preferences& prefs, QWidget* parent) :
    QMainWindow(parent),
    _prefs(prefs),
    _ui(new Ui::MainWindow),
    _null_stream(&_null_buffer),
    _scripts_path(prefs.get_scripts_path()),
    _yacas_server(nullptr),
    _yacas2tex(new CYacas(_null_stream)),
    _has_file(false),
    _modified(false),
    _fname(QString("Untitled Notebook ") + QString::number(_cntr++))
{
    _yacas_server = new YacasServer(_scripts_path);

    connect(_yacas_server,
            &YacasServer::busy,
            this,
            &MainWindow::handle_engine_busy);

    _yacas2tex->Evaluate(((std::string("DefaultDirectory(\"") +
                           _scripts_path.toStdString() + "\");")));
    _yacas2tex->Evaluate("Load(\"yacasinit.ys\");");

    _ui->setupUi(this);

    const qreal scale =
        QGuiApplication::primaryScreen()->logicalDotsPerInch() / 96.0;

    resize(size() * scale);

    const qreal icon_size = 16 * scale;
    _ui->toolBar->setIconSize(QSize(icon_size, icon_size));

    _ui->webEngineView->setZoomFactor(scale);

    _update_title();

    loadYacasPage();
    _ui->webEngineView->setAttribute(Qt::WA_AcceptTouchEvents, false);

    _ui->webEngineView->addAction(_ui->actionInsert_Before);
    _ui->webEngineView->addAction(_ui->actionInsert_After);
    _ui->webEngineView->addAction(_ui->actionDelete_Current);
    _ui->webEngineView->addAction(_ui->actionCurrent_Symbol_Help);

    _windows.append(this);

    connect(&_prefs,
            &Preferences::changed,
            this,
            &MainWindow::handle_prefs_changed);

#ifdef __APPLE__
    // workaround for a qt bug causing toolbar to be displayed with black
    // background when QWebEngineView is used
    setUnifiedTitleAndToolBarOnMac(false);
#endif
}

MainWindow::~MainWindow()
{
    _windows.removeOne(this);

    delete _yacas_server;
    delete _yacas2tex;
    delete _ui;
}

void MainWindow::closeEvent(QCloseEvent* event)
{
    if (!_modified) {
        event->accept();
    } else {
        const QMessageBox::StandardButton reply = QMessageBox::question(
            this,
            "Save notebook?",
            "Save changes before closing?\n\nYour changes will be lost if you "
            "don't save them.",
            QMessageBox::Save | QMessageBox::Cancel | QMessageBox::Close);

        if (reply == QMessageBox::Save)
            on_action_Save_triggered();

        if (reply == QMessageBox::Cancel)
            event->ignore();
        else
            event->accept();
    }
}

void MainWindow::loadYacasPage()
{
    QDir resources_dir(_prefs.get_resources_path());
    const QUrl url =
        QUrl::fromLocalFile(resources_dir.absoluteFilePath("yacas_gui.html"));

    QWebChannel* c = new QWebChannel();
    c->registerObject("yacas", this);
    _ui->webEngineView->page()->setWebChannel(c);

    _ui->webEngineView->load(url);
}

void MainWindow::print(QPrinter* printer)
{
#if QT_VERSION_MAJOR >= 5 && QT_VERSION_MINOR >= 8
    _ui->webEngineView->page()->print(printer, [](bool) {});
#endif
}

void MainWindow::on_action_New_triggered()
{
    MainWindow* w = new MainWindow(_prefs);
    w->show();
}

void MainWindow::on_action_Open_triggered()
{
    QString fname = QFileDialog::getOpenFileName(
        this, "Open", _prefs.get_cwd(), "Yagy files (*.ygy);;All files (*)");

    if (fname.length() == 0)
        return;

    QFile f(fname);

    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Couldn't open file for loading.");
        return;
    }

    QByteArray data = f.readAll();

    loadYacasPage();

    foreach (const QJsonValue& v, QJsonDocument::fromJson(data).array())
        _ui->webEngineView->page()->runJavaScript(
            QString("calculate(\"") +
            v.toObject()["input"].toString().replace("\"", "\\\"") +
            QString("\");"));

    _fname = fname;
    _modified = false;
    _has_file = true;
    _update_title();

    _prefs.set_cwd(QFileInfo(f).canonicalPath());
}

void MainWindow::on_action_Save_triggered()
{
    if (!_has_file)
        on_action_Save_As_triggered();
    else
        _save();
}

void MainWindow::on_action_Save_As_triggered()
{
    QString fname = QFileDialog::getSaveFileName(
        this, "Save", _fname, "Yacas GUI files (*.ygy);;All files (*)");

    if (fname.length() == 0)
        return;

    if (QFileInfo(fname).suffix() == "")
        fname += ".ygy";

    _fname = fname;

    _save();
}

void MainWindow::on_action_Print_triggered()
{
    if (!_printer)
        _printer.reset(new QPrinter);

    QScopedPointer<QPrintDialog> d(new QPrintDialog(_printer.data(), this));
    d->setAttribute(Qt::WA_DeleteOnClose);
    connect(d.data(), SIGNAL(accepted(QPrinter*)), SLOT(print(QPrinter*)));
    d->show();
    d.take();
}

void MainWindow::on_action_Close_triggered()
{
    close();
}

void MainWindow::on_action_Quit_triggered()
{
    foreach (MainWindow* w, _windows)
        w->close();
}

void MainWindow::on_actionCu_t_triggered()
{
    _ui->webEngineView->triggerPageAction(QWebEnginePage::Cut);
}

void MainWindow::on_action_Copy_triggered()
{
    _ui->webEngineView->triggerPageAction(QWebEnginePage::Copy);
}

void MainWindow::on_action_Paste_triggered()
{
    _ui->webEngineView->triggerPageAction(QWebEnginePage::Paste);
}

void MainWindow::on_actionPreferences_triggered()
{
    PreferencesDialog(_prefs, this).exec();
}

void MainWindow::on_actionInsert_Before_triggered()
{
    _ui->webEngineView->page()->runJavaScript("insertBeforeCurrent();");
}

void MainWindow::on_action_Previous_triggered()
{
    _ui->webEngineView->page()->runJavaScript("previousCell();");
}

void MainWindow::on_action_Next_triggered()
{
    _ui->webEngineView->page()->runJavaScript("nextCell();");
}

void MainWindow::on_actionInsert_After_triggered()
{
    _ui->webEngineView->page()->runJavaScript("insertAfterCurrent();");
}

void MainWindow::on_actionDelete_Current_triggered()
{
    _ui->webEngineView->page()->runJavaScript("deleteCurrent();");
}

void MainWindow::on_action_Use_triggered()
{
    QString fname = QFileDialog::getOpenFileName(
        this, "Open", "", "Yacas scripts (*.ys);;All files (*)");

    if (fname.length() == 0)
        return;

    QFile f(fname);

    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Couldn't open file for loading.");
        return;
    }

    _ui->webEngineView->page()->runJavaScript(QString("calculate('Use(\"") +
                                              fname + "\")');");
}

void MainWindow::on_action_Import_triggered()
{
    QString fname = QFileDialog::getOpenFileName(
        this, "Open", "", "Yacas scripts (*.ys);;All files (*)");

    if (fname.length() == 0)
        return;

    QFile f(fname);

    if (!f.open(QIODevice::ReadOnly)) {
        qWarning("Couldn't open file for loading.");
        return;
    }

    QString data = f.readAll();

    QList<QString> l;

    QString s;
    bool in_string = false;
    int level = 0;
    for (QString::const_iterator i = data.begin(); i != data.end(); ++i) {

        if (in_string) {
            s.push_back(*i);
            if (*i == '"')
                in_string = false;
            continue;
        }

        if (*i == '"') {
            s.push_back(*i);
            in_string = true;
            continue;
        }

        if (*i == '[') {
            s.push_back(*i);
            level += 1;
            continue;
        }

        if (*i == ']') {
            s.push_back(*i);
            level -= 1;
            continue;
        }

        if (*i == '\n' && s.isEmpty())
            continue;

        if (*i == ';' && level == 0) {
            l.push_back(s);
            s.clear();
            continue;
        }

        if (*i == '\n') {
            s.push_back("\\n");
            continue;
        }

        s.push_back(*i);
    }

    foreach (const QString& s, l)
        _ui->webEngineView->page()->runJavaScript(QString("calculate('") + s +
                                                  "');");
}

void MainWindow::on_action_Export_triggered()
{
    QString fname = QFileDialog::getSaveFileName(
        this, "Open", "", "Yacas scripts (*.ys);;All files (*)");

    if (fname.length() == 0)
        return;

    _ui->webEngineView->page()->runJavaScript(
        "exportScript();", [fname](const QVariant& v) {
            QFile f(fname);

            if (!f.open(QIODevice::WriteOnly)) {
                qWarning("Couldn't open file for saving.");
                return;
            }

            for (const QString& s : v.toStringList()) {
                f.write(s.toLatin1());
                f.write("\n");
            }
        });
}

void MainWindow::on_actionEvaluate_Current_triggered()
{
    _ui->webEngineView->page()->runJavaScript(QString("evaluateCurrent()"));
}

void MainWindow::on_actionEvaluate_All_triggered()
{
    _ui->webEngineView->page()->runJavaScript(QString("evaluateAll()"));
}

void MainWindow::on_action_Stop_triggered()
{
    _yacas_server->cancel();
}

void MainWindow::on_action_Restart_triggered()
{
    const QMessageBox::StandardButton reply = QMessageBox::question(
        this, "Restart", "Restart Yacas?", QMessageBox::Yes | QMessageBox::No);

    if (reply == QMessageBox::Yes) {
        delete _yacas_server;
        _yacas_server = new YacasServer(_scripts_path);
    }
}

void MainWindow::on_actionYacas_Manual_triggered()
{
    QDesktopServices::openUrl(QUrl(
        "http://yacas.readthedocs.org/en/latest/reference_manual/index.html"));
}

void MainWindow::on_actionCurrent_Symbol_Help_triggered()
{
    _ui->webEngineView->page()->runJavaScript("contextHelp()");
}

void MainWindow::on_action_About_triggered()
{
    QString about = "Yacas GUI version %1\n"
                    "\n"
                    "Powered by Yacas version %1";

    QMessageBox::about(this, "About Yacas GUI", about.arg(YACAS_VERSION));
}

void MainWindow::handle_engine_busy(bool busy)
{
    _ui->action_Stop->setEnabled(busy);
}

void MainWindow::handle_prefs_changed()
{
    _ui->toolBar->setVisible(_prefs.get_enable_toolbar());
    _ui->webEngineView->page()->runJavaScript(
        QString("changeMathJaxScale(%1)").arg(_prefs.get_math_font_scale()));
    QString font = _prefs.get_math_font();
    if (font == "Default")
        font = "TeX";
    _ui->webEngineView->page()->runJavaScript(
        QString("changeMathJaxFont(\"%1\")").arg(font));

    if (_scripts_path != _prefs.get_scripts_path()) {
        _scripts_path = _prefs.get_scripts_path();
        delete _yacas_server;
        _yacas_server = new YacasServer(_scripts_path);
        delete _yacas2tex;
        _yacas2tex = new CYacas(_null_stream);
        _yacas2tex->Evaluate(((std::string("DefaultDirectory(\"") +
                               _scripts_path.toStdString() + "\");"))
                                 .c_str());
        _yacas2tex->Evaluate("Load(\"yacasinit.ys\");");
    }
}

void MainWindow::eval(int idx, QString expr)
{
    new CellProxy(
        _ui->webEngineView->page(), idx, expr, *_yacas_server, *_yacas2tex);

    if (!_modified) {
        _modified = true;
        _update_title();
    }
}

QStringList MainWindow::complete(QString p)
{
    QStringList hints;

    for (const QString& s : _yacas_server->symbols())
        if (s.startsWith(p))
            hints.push_back(s);

    hints.sort();

    return hints;
}

void MainWindow::help(QString s, int cp)
{
    if (s.length() == 0)
        return;

    if (cp >= s.length())
        cp = s.length() - 1;

    int b = QRegExp("[^a-zA-Z']").lastIndexIn(s, cp);
    if (b == cp && cp > 0)
        b = QRegExp("[^a-zA-Z']").lastIndexIn(s, cp - 1);

    if (b == -1)
        b = 0;

    QRegExp word_rx("[a-zA-Z']+");

    if (word_rx.indexIn(s, b) == -1)
        return;

    const QString key = word_rx.cap(0);

    QDir doc_dir(QApplication::applicationDirPath());
#ifdef __APPLE__
    doc_dir.cd("../SharedFrameworks/yacas.framework/Versions/Current/Resources/"
               "documentation/singlehtml");
#else
    doc_dir.cd("../share/yacas/documentation/singlehtml");
#endif

    const QString ref = QString("file://") + doc_dir.canonicalPath() +
                        QString("/index.html#") + key;

    QDesktopServices::openUrl(QUrl(ref));
}

void MainWindow::on_contentsChanged()
{
    if (!_modified) {
        _modified = true;
        _update_title();
    }
}

void MainWindow::on_initComplete()
{
    handle_prefs_changed();
    _modified = false;
    _update_title();
}

int MainWindow::getIsWebGLEnabled()
{
    return _prefs.get_enable_WebGL();
}

void MainWindow::_save()
{
    QFile f(_fname);

    if (!f.open(QIODevice::WriteOnly)) {
        qWarning("Couldn't open file for saving.");
        return;
    }

    _ui->webEngineView->page()->runJavaScript(
        "getAllInputs()", [&f](const QVariant& v) {
            QJsonArray j;
            foreach (const QVariant e, v.toList()) {
                QJsonObject o;
                o["input"] = e.toString();
                j.push_back(o);
            }

            QJsonDocument d(j);

            f.write(d.toJson());
        });

    _modified = false;
    _has_file = true;
    _update_title();
}

void MainWindow::_update_title()
{
    QString title = QFileInfo(_fname).baseName();

    if (_modified)
        title = "*" + title;

    setWindowTitle(title);

    _ui->action_Save->setEnabled(_modified);
}

void MainWindow::copyToClipboard(QString newText)
{
    QClipboard* clipboard = QApplication::clipboard();
    clipboard->setText(newText);
}

QList<MainWindow*> MainWindow::_windows;
unsigned MainWindow::_cntr = 1;
