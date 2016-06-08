#ifndef PREFERENCES_DIALOG_H
#define	PREFERENCES_DIALOG_H

#include <QtWidgets/QDialog>

#include "ui_preferences_dialog.h"

#include "preferences.h"

class PreferencesDialog: public QDialog, public Ui::PreferencesDialog {
    
    Q_OBJECT
    
public:
    explicit PreferencesDialog(Preferences& prefs, QWidget* parent = nullptr);

private slots:
    void on_choosePathButton_clicked();
    void on_customPathButton_toggled(bool);
    
    void accept();
    void reject();
    
private:
    Preferences& _prefs;
};

#endif
