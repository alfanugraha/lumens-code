# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Documents and Settings\1rszostok\.qgis2\python\plugins\GroupStats\ui_groupstats.ui'
#
# Created: Mon Nov 11 14:31:15 2013
#      by: PyQt4 UI code generator 4.8.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_GroupStatsDialog(object):
    def setupUi(self, GroupStatsDialog):
        GroupStatsDialog.setObjectName(_fromUtf8("GroupStatsDialog"))
        GroupStatsDialog.resize(800, 600)
        self.centralwidget = QtGui.QWidget(GroupStatsDialog)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.centralwidget)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        GroupStatsDialog.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(GroupStatsDialog)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 20))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        self.menuData = QtGui.QMenu(self.menubar)
        self.menuData.setObjectName(_fromUtf8("menuData"))
        self.menuView = QtGui.QMenu(self.menubar)
        self.menuView.setObjectName(_fromUtf8("menuView"))
        self.menuHelp = QtGui.QMenu(self.menubar)
        self.menuHelp.setObjectName(_fromUtf8("menuHelp"))
        self.menuFeatures = QtGui.QMenu(self.menubar)
        self.menuFeatures.setObjectName(_fromUtf8("menuFeatures"))
        GroupStatsDialog.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(GroupStatsDialog)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        GroupStatsDialog.setStatusBar(self.statusbar)
        self.panelSterowania = QtGui.QDockWidget(GroupStatsDialog)
        self.panelSterowania.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.panelSterowania.sizePolicy().hasHeightForWidth())
        self.panelSterowania.setSizePolicy(sizePolicy)
        self.panelSterowania.setMinimumSize(QtCore.QSize(292, 566))
        self.panelSterowania.setAccessibleName(_fromUtf8(""))
        self.panelSterowania.setObjectName(_fromUtf8("panelSterowania"))
        self.dockWidgetContents = QtGui.QWidget()
        self.dockWidgetContents.setObjectName(_fromUtf8("dockWidgetContents"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.dockWidgetContents)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label_6 = QtGui.QLabel(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label_6.sizePolicy().hasHeightForWidth())
        self.label_6.setSizePolicy(sizePolicy)
        self.label_6.setObjectName(_fromUtf8("label_6"))
        self.verticalLayout.addWidget(self.label_6)
        self.warstwa = QtGui.QComboBox(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.warstwa.sizePolicy().hasHeightForWidth())
        self.warstwa.setSizePolicy(sizePolicy)
        self.warstwa.setObjectName(_fromUtf8("warstwa"))
        self.verticalLayout.addWidget(self.warstwa)
        self.label = QtGui.QLabel(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label.sizePolicy().hasHeightForWidth())
        self.label.setSizePolicy(sizePolicy)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        self.listaPol = QtGui.QListView(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Minimum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.listaPol.sizePolicy().hasHeightForWidth())
        self.listaPol.setSizePolicy(sizePolicy)
        self.listaPol.setMinimumSize(QtCore.QSize(0, 0))
        self.listaPol.setDragEnabled(True)
        self.listaPol.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
        self.listaPol.setObjectName(_fromUtf8("listaPol"))
        self.verticalLayout.addWidget(self.listaPol)
        self.gridLayout_2 = QtGui.QGridLayout()
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.wartosci = QtGui.QListView(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.wartosci.sizePolicy().hasHeightForWidth())
        self.wartosci.setSizePolicy(sizePolicy)
        self.wartosci.setMinimumSize(QtCore.QSize(0, 0))
        self.wartosci.setDragEnabled(True)
        self.wartosci.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
        self.wartosci.setObjectName(_fromUtf8("wartosci"))
        self.gridLayout_2.addWidget(self.wartosci, 3, 1, 1, 1)
        self.kolumny = QtGui.QListView(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.kolumny.sizePolicy().hasHeightForWidth())
        self.kolumny.setSizePolicy(sizePolicy)
        self.kolumny.setMinimumSize(QtCore.QSize(0, 0))
        self.kolumny.setDragEnabled(True)
        self.kolumny.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
        self.kolumny.setObjectName(_fromUtf8("kolumny"))
        self.gridLayout_2.addWidget(self.kolumny, 1, 1, 1, 1)
        self.wiersze = QtGui.QListView(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.wiersze.sizePolicy().hasHeightForWidth())
        self.wiersze.setSizePolicy(sizePolicy)
        self.wiersze.setMinimumSize(QtCore.QSize(0, 0))
        self.wiersze.setDragEnabled(True)
        self.wiersze.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
        self.wiersze.setObjectName(_fromUtf8("wiersze"))
        self.gridLayout_2.addWidget(self.wiersze, 3, 0, 1, 1)
        self.label_3 = QtGui.QLabel(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label_3.sizePolicy().hasHeightForWidth())
        self.label_3.setSizePolicy(sizePolicy)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout_2.addWidget(self.label_3, 0, 1, 1, 1)
        self.label_4 = QtGui.QLabel(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label_4.sizePolicy().hasHeightForWidth())
        self.label_4.setSizePolicy(sizePolicy)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout_2.addWidget(self.label_4, 2, 0, 1, 1)
        self.filtr = QtGui.QPlainTextEdit(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.filtr.sizePolicy().hasHeightForWidth())
        self.filtr.setSizePolicy(sizePolicy)
        self.filtr.setMinimumSize(QtCore.QSize(0, 0))
        self.filtr.setDocumentTitle(_fromUtf8(""))
        self.filtr.setReadOnly(False)
        self.filtr.setPlainText(_fromUtf8(""))
        self.filtr.setTextInteractionFlags(QtCore.Qt.TextEditorInteraction)
        self.filtr.setObjectName(_fromUtf8("filtr"))
        self.gridLayout_2.addWidget(self.filtr, 1, 0, 1, 1)
        self.filtrButton = QtGui.QToolButton(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.filtrButton.sizePolicy().hasHeightForWidth())
        self.filtrButton.setSizePolicy(sizePolicy)
        self.filtrButton.setObjectName(_fromUtf8("filtrButton"))
        self.gridLayout_2.addWidget(self.filtrButton, 0, 0, 1, 1)
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setObjectName(_fromUtf8("horizontalLayout_4"))
        self.label_5 = QtGui.QLabel(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label_5.sizePolicy().hasHeightForWidth())
        self.label_5.setSizePolicy(sizePolicy)
        self.label_5.setObjectName(_fromUtf8("label_5"))
        self.horizontalLayout_4.addWidget(self.label_5)
        self.useNULL = QtGui.QCheckBox(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.useNULL.sizePolicy().hasHeightForWidth())
        self.useNULL.setSizePolicy(sizePolicy)
        self.useNULL.setChecked(False)
        self.useNULL.setObjectName(_fromUtf8("useNULL"))
        self.horizontalLayout_4.addWidget(self.useNULL)
        self.gridLayout_2.addLayout(self.horizontalLayout_4, 2, 1, 1, 1)
        self.verticalLayout.addLayout(self.gridLayout_2)
        self.verticalLayout.setStretch(3, 2)
        self.verticalLayout.setStretch(4, 1)
        self.verticalLayout_2.addLayout(self.verticalLayout)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.tylkoZaznaczone = QtGui.QCheckBox(self.dockWidgetContents)
        self.tylkoZaznaczone.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.tylkoZaznaczone.sizePolicy().hasHeightForWidth())
        self.tylkoZaznaczone.setSizePolicy(sizePolicy)
        self.tylkoZaznaczone.setObjectName(_fromUtf8("tylkoZaznaczone"))
        self.horizontalLayout_2.addWidget(self.tylkoZaznaczone)
        spacerItem = QtGui.QSpacerItem(0, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem)
        self.wyczysc = QtGui.QPushButton(self.dockWidgetContents)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.wyczysc.sizePolicy().hasHeightForWidth())
        self.wyczysc.setSizePolicy(sizePolicy)
        self.wyczysc.setObjectName(_fromUtf8("wyczysc"))
        self.horizontalLayout_2.addWidget(self.wyczysc)
        self.verticalLayout_2.addLayout(self.horizontalLayout_2)
        self.oblicz = QtGui.QPushButton(self.dockWidgetContents)
        self.oblicz.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.oblicz.sizePolicy().hasHeightForWidth())
        self.oblicz.setSizePolicy(sizePolicy)
        self.oblicz.setObjectName(_fromUtf8("oblicz"))
        self.verticalLayout_2.addWidget(self.oblicz)
        self.panelSterowania.setWidget(self.dockWidgetContents)
        GroupStatsDialog.addDockWidget(QtCore.Qt.DockWidgetArea(2), self.panelSterowania)
        self.actionKopiuj = QtGui.QAction(GroupStatsDialog)
        self.actionKopiuj.setObjectName(_fromUtf8("actionKopiuj"))
        self.actionZapiszCSV = QtGui.QAction(GroupStatsDialog)
        self.actionZapiszCSV.setEnabled(True)
        self.actionZapiszCSV.setObjectName(_fromUtf8("actionZapiszCSV"))
        self.actionPokazPanel = QtGui.QAction(GroupStatsDialog)
        self.actionPokazPanel.setObjectName(_fromUtf8("actionPokazPanel"))
        self.actionKopiujZaznaczone = QtGui.QAction(GroupStatsDialog)
        self.actionKopiujZaznaczone.setEnabled(True)
        self.actionKopiujZaznaczone.setObjectName(_fromUtf8("actionKopiujZaznaczone"))
        self.actionZapiszCSVZaznaczone = QtGui.QAction(GroupStatsDialog)
        self.actionZapiszCSVZaznaczone.setEnabled(True)
        self.actionZapiszCSVZaznaczone.setObjectName(_fromUtf8("actionZapiszCSVZaznaczone"))
        self.actionPokazNaMapie = QtGui.QAction(GroupStatsDialog)
        self.actionPokazNaMapie.setEnabled(True)
        self.actionPokazNaMapie.setObjectName(_fromUtf8("actionPokazNaMapie"))
        self.actionTutorial = QtGui.QAction(GroupStatsDialog)
        self.actionTutorial.setObjectName(_fromUtf8("actionTutorial"))
        self.actionPolaczZaznaczoneObiekty = QtGui.QAction(GroupStatsDialog)
        self.actionPolaczZaznaczoneObiekty.setEnabled(False)
        self.actionPolaczZaznaczoneObiekty.setObjectName(_fromUtf8("actionPolaczZaznaczoneObiekty"))
        self.menuData.addAction(self.actionKopiuj)
        self.menuData.addAction(self.actionKopiujZaznaczone)
        self.menuData.addAction(self.actionZapiszCSV)
        self.menuData.addAction(self.actionZapiszCSVZaznaczone)
        self.menuView.addAction(self.actionPokazPanel)
        self.menuHelp.addAction(self.actionTutorial)
        self.menuFeatures.addAction(self.actionPokazNaMapie)
        self.menuFeatures.addAction(self.actionPolaczZaznaczoneObiekty)
        self.menubar.addAction(self.menuData.menuAction())
        self.menubar.addAction(self.menuFeatures.menuAction())
        self.menubar.addAction(self.menuView.menuAction())
        self.menubar.addAction(self.menuHelp.menuAction())

        self.retranslateUi(GroupStatsDialog)
        QtCore.QMetaObject.connectSlotsByName(GroupStatsDialog)

    def retranslateUi(self, GroupStatsDialog):
        GroupStatsDialog.setWindowTitle(QtGui.QApplication.translate("GroupStatsDialog", "Group Stats", None, QtGui.QApplication.UnicodeUTF8))
        self.menuData.setTitle(QtGui.QApplication.translate("GroupStatsDialog", "Data", None, QtGui.QApplication.UnicodeUTF8))
        self.menuView.setTitle(QtGui.QApplication.translate("GroupStatsDialog", "Window", None, QtGui.QApplication.UnicodeUTF8))
        self.menuHelp.setTitle(QtGui.QApplication.translate("GroupStatsDialog", "Help", None, QtGui.QApplication.UnicodeUTF8))
        self.menuFeatures.setTitle(QtGui.QApplication.translate("GroupStatsDialog", "Features", None, QtGui.QApplication.UnicodeUTF8))
        self.panelSterowania.setWindowTitle(QtGui.QApplication.translate("GroupStatsDialog", "Control panel", None, QtGui.QApplication.UnicodeUTF8))
        self.label_6.setText(QtGui.QApplication.translate("GroupStatsDialog", "Layers", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("GroupStatsDialog", "Fields", None, QtGui.QApplication.UnicodeUTF8))
        self.listaPol.setToolTip(QtGui.QApplication.translate("GroupStatsDialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Tahoma\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Drag field and drop it into \'Columns\', \'Rows\' or \'Value\' area.</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.wartosci.setToolTip(QtGui.QApplication.translate("GroupStatsDialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Tahoma\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">Drop numerical attribute or geometry for calculations.</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">Drop function </span><a name=\"result_box\"></a>if it is not in the rows/columns area.</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.kolumny.setToolTip(QtGui.QApplication.translate("GroupStatsDialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Tahoma\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">Drop atrributes or/and functions for columns.</span></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.wiersze.setToolTip(QtGui.QApplication.translate("GroupStatsDialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Tahoma\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">Drop atrributes or/and functions for rows.</span></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("GroupStatsDialog", "Columns", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("GroupStatsDialog", "Rows", None, QtGui.QApplication.UnicodeUTF8))
        self.filtrButton.setText(QtGui.QApplication.translate("GroupStatsDialog", "Filter", None, QtGui.QApplication.UnicodeUTF8))
        self.label_5.setText(QtGui.QApplication.translate("GroupStatsDialog", "Value", None, QtGui.QApplication.UnicodeUTF8))
        self.useNULL.setToolTip(QtGui.QApplication.translate("GroupStatsDialog", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'MS Shell Dlg 2\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">If checked, records with NULL values are used to calculate (regard them as 0).</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:8pt;\"></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.useNULL.setText(QtGui.QApplication.translate("GroupStatsDialog", "use NULL values", None, QtGui.QApplication.UnicodeUTF8))
        self.tylkoZaznaczone.setText(QtGui.QApplication.translate("GroupStatsDialog", "Use only selected features", None, QtGui.QApplication.UnicodeUTF8))
        self.wyczysc.setText(QtGui.QApplication.translate("GroupStatsDialog", "Clear", None, QtGui.QApplication.UnicodeUTF8))
        self.oblicz.setText(QtGui.QApplication.translate("GroupStatsDialog", "Calculate", None, QtGui.QApplication.UnicodeUTF8))
        self.actionKopiuj.setText(QtGui.QApplication.translate("GroupStatsDialog", "Copy all to clipboard", None, QtGui.QApplication.UnicodeUTF8))
        self.actionZapiszCSV.setText(QtGui.QApplication.translate("GroupStatsDialog", "Save all to CSV file", None, QtGui.QApplication.UnicodeUTF8))
        self.actionPokazPanel.setText(QtGui.QApplication.translate("GroupStatsDialog", "Show Control panel", None, QtGui.QApplication.UnicodeUTF8))
        self.actionKopiujZaznaczone.setText(QtGui.QApplication.translate("GroupStatsDialog", "Copy selected to clipboard", None, QtGui.QApplication.UnicodeUTF8))
        self.actionZapiszCSVZaznaczone.setText(QtGui.QApplication.translate("GroupStatsDialog", "Save selected to CSV file", None, QtGui.QApplication.UnicodeUTF8))
        self.actionPokazNaMapie.setText(QtGui.QApplication.translate("GroupStatsDialog", "Show selected on map", None, QtGui.QApplication.UnicodeUTF8))
        self.actionTutorial.setText(QtGui.QApplication.translate("GroupStatsDialog", "Tutorial", None, QtGui.QApplication.UnicodeUTF8))
        self.actionPolaczZaznaczoneObiekty.setText(QtGui.QApplication.translate("GroupStatsDialog", "Join selected features", None, QtGui.QApplication.UnicodeUTF8))

import resources_rc
