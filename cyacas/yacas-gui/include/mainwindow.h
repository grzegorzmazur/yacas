#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtCore/QMap>
#include <QtCore/QVariant>

#include <QtGui/QCloseEvent>
#include <QtGui/QClipboard>

#include <QtWidgets/QMainWindow>

#include <QtPrintSupport/QPrinter>

#ifdef YAGY_ENABLE_INSPECTOR
#include <QtWebKitWidgets/QWebInspector>
#endif

#include "yacasserver.h"
#include "yacas/yacas.h"

#include "preferences.h"

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    explicit MainWindow(Preferences& prefs, QWidget* parent = 0);
    ~MainWindow();

public slots:
    void eval(int idx, QString expr);
    void help(QString, int);
    bool isWebGLEnabled();
    void copyToClipboard( QString newText );
    
    void on_contentsChanged();

protected:
    void closeEvent(QCloseEvent*);
    
    void loadYacasPage();

private slots:
    void initObjectMapping();

    void print(QPrinter*);

    void on_action_New_triggered();
    void on_action_Open_triggered();
    void on_action_Save_triggered();
    void on_action_Save_As_triggered();
    void on_action_Print_triggered();
    void on_action_Close_triggered();
    void on_action_Quit_triggered();

    void on_actionCu_t_triggered();
    void on_action_Copy_triggered();
    void on_action_Paste_triggered();
    void on_actionPreferences_triggered();
    
    void on_action_Next_triggered();
    void on_action_Previous_triggered();
    void on_actionInsert_Before_triggered();
    void on_actionInsert_After_triggered();
    void on_actionDelete_Current_triggered();
    
    void on_action_Use_triggered();
    void on_action_Import_triggered();
    void on_action_Export_triggered();

    void on_actionEvaluate_Current_triggered();
    void on_actionEvaluate_All_triggered();
    void on_action_Stop_triggered();
    void on_action_Restart_triggered();

    void on_actionYacas_Manual_triggered();
    void on_actionCurrent_Symbol_Help_triggered();
    void on_action_About_triggered();

    void handle_engine_busy(bool);
    void handle_prefs_changed();
        
private:
    void _save();
    void _update_title();
    bool isWebGLSupported();
    
    Preferences& _prefs;
    
    Ui::MainWindow* _ui;

    class NullBuffer: public std::streambuf {
    public:
        int overflow(int c) { return c; }
    };

    NullBuffer _null_buffer;
    std::ostream _null_stream;

    QString _scripts_path;
    
    YacasServer* _yacas_server;
    CYacas* _yacas2tex;

    QScopedPointer<QPrinter> _printer;

    bool _has_file;
    bool _modified;
    QString _fname;
    
    static QList<MainWindow*> _windows;
    static unsigned _cntr;
    
#ifdef YAGY_ENABLE_INSPECTOR    
    QWebInspector* _inspector;
#endif
};

#endif // MAINWINDOW_H
