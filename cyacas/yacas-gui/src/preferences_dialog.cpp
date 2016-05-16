#include "preferences_dialog.h"

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>

PreferencesDialog::PreferencesDialog(Preferences& prefs, QWidget* parent):
    QDialog(parent),
    _prefs(prefs)
{
    setupUi(this);
    
    enableToolbarCheckBox->setChecked(_prefs.get_enable_toolbar());
    enableWebGLCheckBox->setChecked(_prefs.get_enable_WebGL());
    mathFontScaleSpinBox->setValue(_prefs.get_math_font_scale());
    mathFontComboBox->setCurrentIndex(mathFontComboBox->findText(_prefs.get_math_font()));
    pathEdit->setText(_prefs.get_custom_scripts_path());
    defaultPathButton->setChecked(_prefs.get_scripts_path_default());
    customPathButton->setChecked(!_prefs.get_scripts_path_default());
}

void PreferencesDialog::on_choosePathButton_clicked()
{
    const QString path = QFileDialog::getExistingDirectory(this, "Scripts Directory", "");
    
    if (!path.isNull())
        pathEdit->setText(path + "/");
}

void PreferencesDialog::on_customPathButton_toggled(bool enabled)
{
    pathEdit->setEnabled(enabled);
    choosePathButton->setEnabled(enabled);
}

void PreferencesDialog::accept()
{
    if (!defaultPathButton->isChecked()) {
        if (!pathEdit->text().isEmpty() && !pathEdit->text().endsWith('/'))
            pathEdit->setText(pathEdit->text() + "/");
        
        if (!QFile(pathEdit->text() + "yacasinit.ys").exists()) {
            QMessageBox::warning(this, "YAGY Warning", "Invalid custom scripts path.");
            return;
        }
    }
    
    const QString old_path = _prefs.get_scripts_path();
    const QString new_path = defaultPathButton->isChecked() ? _prefs.get_default_scripts_path() : pathEdit->text();
    if (new_path != old_path)
        if (QMessageBox::question(this, "YAGY Preferences", "The scripts path has changed. Engine restart required. Do you want to proceed?") == QMessageBox::No)
            return;

    _prefs.set_enable_toolbar(enableToolbarCheckBox->checkState());
    _prefs.set_math_font_scale(mathFontScaleSpinBox->value());
    _prefs.set_math_font(mathFontComboBox->currentText());
    _prefs.set_custom_scripts_path(pathEdit->text());
    _prefs.set_scripts_path_default(defaultPathButton->isChecked());
    _prefs.set_enable_WebGL(enableWebGLCheckBox->checkState());
    
    QDialog::accept();
}

void PreferencesDialog::reject()
{
    QDialog::reject();
}
