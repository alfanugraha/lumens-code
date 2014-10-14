# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'OknoGlowne.ui'
#
# Created: Sun Nov 18 19:22:55 2012
#      by: PyQt4 UI code generator 4.9.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_OknoGlowne(object):
    def setupUi(self, OknoGlowne):
        OknoGlowne.setObjectName(_fromUtf8("OknoGlowne"))
        OknoGlowne.resize(800, 600)
        self.centralwidget = QtGui.QWidget(OknoGlowne)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.centralwidget)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.tableView = QtGui.QTableView(self.centralwidget)
        self.tableView.setObjectName(_fromUtf8("tableView"))
        self.horizontalLayout.addWidget(self.tableView)
        OknoGlowne.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(OknoGlowne)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 25))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        OknoGlowne.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(OknoGlowne)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        OknoGlowne.setStatusBar(self.statusbar)
        self.dockWidget = QtGui.QDockWidget(OknoGlowne)
        self.dockWidget.setObjectName(_fromUtf8("dockWidget"))
        self.dockWidgetContents = QtGui.QWidget()
        self.dockWidgetContents.setObjectName(_fromUtf8("dockWidgetContents"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.dockWidgetContents)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label = QtGui.QLabel(self.dockWidgetContents)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        self.listaPol = QtGui.QListView(self.dockWidgetContents)
        self.listaPol.setObjectName(_fromUtf8("listaPol"))
        self.verticalLayout.addWidget(self.listaPol)
        self.gridLayout_2 = QtGui.QGridLayout()
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.listView_6 = QtGui.QListView(self.dockWidgetContents)
        self.listView_6.setObjectName(_fromUtf8("listView_6"))
        self.gridLayout_2.addWidget(self.listView_6, 3, 1, 1, 1)
        self.listView_4 = QtGui.QListView(self.dockWidgetContents)
        self.listView_4.setObjectName(_fromUtf8("listView_4"))
        self.gridLayout_2.addWidget(self.listView_4, 1, 1, 1, 1)
        self.label_2 = QtGui.QLabel(self.dockWidgetContents)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout_2.addWidget(self.label_2, 0, 0, 1, 1)
        self.listView_3 = QtGui.QListView(self.dockWidgetContents)
        self.listView_3.setObjectName(_fromUtf8("listView_3"))
        self.gridLayout_2.addWidget(self.listView_3, 1, 0, 1, 1)
        self.listView_5 = QtGui.QListView(self.dockWidgetContents)
        self.listView_5.setObjectName(_fromUtf8("listView_5"))
        self.gridLayout_2.addWidget(self.listView_5, 3, 0, 1, 1)
        self.label_3 = QtGui.QLabel(self.dockWidgetContents)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout_2.addWidget(self.label_3, 0, 1, 1, 1)
        self.label_4 = QtGui.QLabel(self.dockWidgetContents)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout_2.addWidget(self.label_4, 2, 0, 1, 1)
        self.label_5 = QtGui.QLabel(self.dockWidgetContents)
        self.label_5.setObjectName(_fromUtf8("label_5"))
        self.gridLayout_2.addWidget(self.label_5, 2, 1, 1, 1)
        self.verticalLayout.addLayout(self.gridLayout_2)
        self.verticalLayout_2.addLayout(self.verticalLayout)
        self.dockWidget.setWidget(self.dockWidgetContents)
        OknoGlowne.addDockWidget(QtCore.Qt.DockWidgetArea(2), self.dockWidget)

        self.retranslateUi(OknoGlowne)
        QtCore.QMetaObject.connectSlotsByName(OknoGlowne)

    def retranslateUi(self, OknoGlowne):
        OknoGlowne.setWindowTitle(QtGui.QApplication.translate("OknoGlowne", "MainWindow", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("OknoGlowne", "Lista pól", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("OknoGlowne", "Filtr", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("OknoGlowne", "Kolumny", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("OknoGlowne", "Wiersze", None, QtGui.QApplication.UnicodeUTF8))
        self.label_5.setText(QtGui.QApplication.translate("OknoGlowne", "Wartości", None, QtGui.QApplication.UnicodeUTF8))

