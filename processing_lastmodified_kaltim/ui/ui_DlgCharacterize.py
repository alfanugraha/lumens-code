# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'DlgCharacterize.ui'
#
# Created: Thu Nov 14 20:33:44 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_DlgCharacterize(object):
    def setupUi(self, DlgCharacterize):
        DlgCharacterize.setObjectName(_fromUtf8("DlgCharacterize"))
        DlgCharacterize.resize(627, 488)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/images/characterize.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        DlgCharacterize.setWindowIcon(icon)
        self.gridLayout = QtGui.QGridLayout(DlgCharacterize)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.dataTable = QtGui.QTableWidget(DlgCharacterize)
        self.dataTable.setObjectName(_fromUtf8("dataTable"))
        self.dataTable.setColumnCount(0)
        self.dataTable.setRowCount(0)
        self.gridLayout.addWidget(self.dataTable, 0, 0, 1, 1)

        self.retranslateUi(DlgCharacterize)
        QtCore.QMetaObject.connectSlotsByName(DlgCharacterize)

    def retranslateUi(self, DlgCharacterize):
        DlgCharacterize.setWindowTitle(QtGui.QApplication.translate("DlgCharacterize", "Characterize", None, QtGui.QApplication.UnicodeUTF8))

