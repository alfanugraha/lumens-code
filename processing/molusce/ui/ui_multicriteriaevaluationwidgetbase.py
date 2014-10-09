# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ui/multicriteriaevaluationwidgetbase.ui'
#
# Created: Fri Oct 18 13:30:57 2013
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
        Widget.resize(688, 344)
        Widget.setWindowTitle(_fromUtf8(""))
        self.gridLayout = QtGui.QGridLayout(Widget)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.tblMatrix = MolusceTableWidget(Widget)
        self.tblMatrix.setObjectName(_fromUtf8("tblMatrix"))
        self.tblMatrix.setColumnCount(0)
        self.tblMatrix.setRowCount(0)
        self.gridLayout.addWidget(self.tblMatrix, 1, 0, 1, 2)
        self.tblWeights = MolusceTableWidget(Widget)
        self.tblWeights.setObjectName(_fromUtf8("tblWeights"))
        self.tblWeights.setColumnCount(0)
        self.tblWeights.setRowCount(0)
        self.gridLayout.addWidget(self.tblWeights, 3, 0, 1, 2)
        self.label = QtGui.QLabel(Widget)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 4, 0, 1, 1)
        self.spnInitialClass = QtGui.QSpinBox(Widget)
        self.spnInitialClass.setObjectName(_fromUtf8("spnInitialClass"))
        self.gridLayout.addWidget(self.spnInitialClass, 4, 1, 1, 1)
        self.label_2 = QtGui.QLabel(Widget)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 5, 0, 1, 1)
        self.spnFinalClass = QtGui.QSpinBox(Widget)
        self.spnFinalClass.setObjectName(_fromUtf8("spnFinalClass"))
        self.gridLayout.addWidget(self.spnFinalClass, 5, 1, 1, 1)
        self.btnTrainModel = QtGui.QPushButton(Widget)
        self.btnTrainModel.setObjectName(_fromUtf8("btnTrainModel"))
        self.gridLayout.addWidget(self.btnTrainModel, 6, 1, 1, 1)
        self.label_3 = QtGui.QLabel(Widget)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout.addWidget(self.label_3, 0, 0, 1, 1)
        self.label_4 = QtGui.QLabel(Widget)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout.addWidget(self.label_4, 2, 0, 1, 1)

        self.retranslateUi(Widget)
        QtCore.QMetaObject.connectSlotsByName(Widget)

    def retranslateUi(self, Widget):
        self.label.setText(QtGui.QApplication.translate("Widget", "From class", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Widget", "To class", None, QtGui.QApplication.UnicodeUTF8))
        self.btnTrainModel.setText(QtGui.QApplication.translate("Widget", "Train model", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("Widget", "Pairwise Comparison Matrix", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("Widget", "Weights Matrix", None, QtGui.QApplication.UnicodeUTF8))

from processing.molusce.moluscetablewidget import MolusceTableWidget
