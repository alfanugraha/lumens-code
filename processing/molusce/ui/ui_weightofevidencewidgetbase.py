# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ui/weightofevidencewidgetbase.ui'
#
# Created: Fri Oct 18 13:30:58 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Widget(object):
    def setupUi(self, Widget):
        Widget.setObjectName(_fromUtf8("Widget"))
        Widget.resize(684, 368)
        Widget.setWindowTitle(_fromUtf8(""))
        self.gridLayout = QtGui.QGridLayout(Widget)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label = QtGui.QLabel(Widget)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 3, 0, 1, 1)
        self.btnTrainModel = QtGui.QPushButton(Widget)
        self.btnTrainModel.setObjectName(_fromUtf8("btnTrainModel"))
        self.gridLayout.addWidget(self.btnTrainModel, 12, 0, 1, 2)
        self.pteWeightsInform = QtGui.QPlainTextEdit(Widget)
        self.pteWeightsInform.setObjectName(_fromUtf8("pteWeightsInform"))
        self.gridLayout.addWidget(self.pteWeightsInform, 11, 0, 1, 2)
        self.btnResetBins = QtGui.QPushButton(Widget)
        self.btnResetBins.setObjectName(_fromUtf8("btnResetBins"))
        self.gridLayout.addWidget(self.btnResetBins, 5, 0, 1, 1)
        self.label_4 = QtGui.QLabel(Widget)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout.addWidget(self.label_4, 6, 0, 1, 1)
        self.tblReclass = MolusceTableWidget(Widget)
        self.tblReclass.setSelectionMode(QtGui.QAbstractItemView.SingleSelection)
        self.tblReclass.setObjectName(_fromUtf8("tblReclass"))
        self.tblReclass.setColumnCount(5)
        self.tblReclass.setRowCount(0)
        item = QtGui.QTableWidgetItem()
        self.tblReclass.setHorizontalHeaderItem(0, item)
        item = QtGui.QTableWidgetItem()
        self.tblReclass.setHorizontalHeaderItem(1, item)
        item = QtGui.QTableWidgetItem()
        self.tblReclass.setHorizontalHeaderItem(2, item)
        item = QtGui.QTableWidgetItem()
        self.tblReclass.setHorizontalHeaderItem(3, item)
        item = QtGui.QTableWidgetItem()
        self.tblReclass.setHorizontalHeaderItem(4, item)
        self.tblReclass.horizontalHeader().setStretchLastSection(True)
        self.gridLayout.addWidget(self.tblReclass, 4, 0, 1, 1)

        self.retranslateUi(Widget)
        QtCore.QMetaObject.connectSlotsByName(Widget)

    def retranslateUi(self, Widget):
        self.label.setText(QtGui.QApplication.translate("Widget", "Enter either Number of intervals (min 2) or Range breaks:", None, QtGui.QApplication.UnicodeUTF8))
        self.btnTrainModel.setText(QtGui.QApplication.translate("Widget", "Train model", None, QtGui.QApplication.UnicodeUTF8))
        self.btnResetBins.setText(QtGui.QApplication.translate("Widget", "Calculate Range breaks", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("Widget", "Weights Information:", None, QtGui.QApplication.UnicodeUTF8))
        item = self.tblReclass.horizontalHeaderItem(0)
        item.setText(QtGui.QApplication.translate("Widget", "Factor", None, QtGui.QApplication.UnicodeUTF8))
        item = self.tblReclass.horizontalHeaderItem(1)
        item.setText(QtGui.QApplication.translate("Widget", "Range min", None, QtGui.QApplication.UnicodeUTF8))
        item = self.tblReclass.horizontalHeaderItem(2)
        item.setText(QtGui.QApplication.translate("Widget", "Range max", None, QtGui.QApplication.UnicodeUTF8))
        item = self.tblReclass.horizontalHeaderItem(3)
        item.setText(QtGui.QApplication.translate("Widget", "Number of intervals", None, QtGui.QApplication.UnicodeUTF8))
        item = self.tblReclass.horizontalHeaderItem(4)
        item.setText(QtGui.QApplication.translate("Widget", "Range breaks", None, QtGui.QApplication.UnicodeUTF8))

from processing.molusce.moluscetablewidget import MolusceTableWidget
