# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'DlgReconcile.ui'
#
# Created: Thu Nov 14 20:33:42 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_DlgReconcile(object):
    def setupUi(self, DlgReconcile):
        DlgReconcile.setObjectName(_fromUtf8("DlgReconcile"))
        DlgReconcile.resize(765, 582)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/images/reconcile.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        DlgReconcile.setWindowIcon(icon)
        self.gridLayout = QtGui.QGridLayout(DlgReconcile)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label = QtGui.QLabel(DlgReconcile)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.inputLayerBox = QtGui.QComboBox(DlgReconcile)
        self.inputLayerBox.setObjectName(_fromUtf8("inputLayerBox"))
        self.horizontalLayout_2.addWidget(self.inputLayerBox)
        self.inputLayerButton = QtGui.QToolButton(DlgReconcile)
        self.inputLayerButton.setMinimumSize(QtCore.QSize(81, 27))
        self.inputLayerButton.setMaximumSize(QtCore.QSize(81, 27))
        self.inputLayerButton.setObjectName(_fromUtf8("inputLayerButton"))
        self.horizontalLayout_2.addWidget(self.inputLayerButton)
        self.refreshButton = QtGui.QPushButton(DlgReconcile)
        self.refreshButton.setMinimumSize(QtCore.QSize(38, 27))
        self.refreshButton.setMaximumSize(QtCore.QSize(38, 27))
        self.refreshButton.setText(_fromUtf8(""))
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/images/iterate.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.refreshButton.setIcon(icon1)
        self.refreshButton.setObjectName(_fromUtf8("refreshButton"))
        self.horizontalLayout_2.addWidget(self.refreshButton)
        self.gridLayout.addLayout(self.horizontalLayout_2, 1, 0, 1, 1)
        self.label_4 = QtGui.QLabel(DlgReconcile)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout.addWidget(self.label_4, 2, 0, 1, 1)
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        self.dataTable = QtGui.QTableWidget(DlgReconcile)
        self.dataTable.setMinimumSize(QtCore.QSize(690, 200))
        self.dataTable.setObjectName(_fromUtf8("dataTable"))
        self.dataTable.setColumnCount(0)
        self.dataTable.setRowCount(0)
        self.horizontalLayout_3.addWidget(self.dataTable)
        self.verticalLayout_2 = QtGui.QVBoxLayout()
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.newColumnButton = QtGui.QPushButton(DlgReconcile)
        self.newColumnButton.setMinimumSize(QtCore.QSize(38, 27))
        self.newColumnButton.setMaximumSize(QtCore.QSize(38, 27))
        self.newColumnButton.setText(_fromUtf8(""))
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/images/new.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.newColumnButton.setIcon(icon2)
        self.newColumnButton.setObjectName(_fromUtf8("newColumnButton"))
        self.verticalLayout_2.addWidget(self.newColumnButton)
        self.saveTableButton = QtGui.QPushButton(DlgReconcile)
        self.saveTableButton.setMinimumSize(QtCore.QSize(38, 27))
        self.saveTableButton.setMaximumSize(QtCore.QSize(38, 27))
        self.saveTableButton.setText(_fromUtf8(""))
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/images/save.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.saveTableButton.setIcon(icon3)
        self.saveTableButton.setObjectName(_fromUtf8("saveTableButton"))
        self.verticalLayout_2.addWidget(self.saveTableButton)
        spacerItem = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem)
        self.horizontalLayout_3.addLayout(self.verticalLayout_2)
        self.gridLayout.addLayout(self.horizontalLayout_3, 3, 0, 1, 1)
        self.label_2 = QtGui.QLabel(DlgReconcile)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 4, 0, 1, 1)
        self.uniqueFieldBox = QtGui.QComboBox(DlgReconcile)
        self.uniqueFieldBox.setObjectName(_fromUtf8("uniqueFieldBox"))
        self.gridLayout.addWidget(self.uniqueFieldBox, 5, 0, 1, 1)
        self.label_3 = QtGui.QLabel(DlgReconcile)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout.addWidget(self.label_3, 6, 0, 1, 1)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.lineEdit = QtGui.QLineEdit(DlgReconcile)
        self.lineEdit.setInputMask(_fromUtf8(""))
        self.lineEdit.setText(_fromUtf8(""))
        self.lineEdit.setObjectName(_fromUtf8("lineEdit"))
        self.horizontalLayout.addWidget(self.lineEdit)
        self.outputLayerButton = QtGui.QToolButton(DlgReconcile)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.outputLayerButton.sizePolicy().hasHeightForWidth())
        self.outputLayerButton.setSizePolicy(sizePolicy)
        self.outputLayerButton.setMinimumSize(QtCore.QSize(81, 27))
        self.outputLayerButton.setMaximumSize(QtCore.QSize(81, 27))
        self.outputLayerButton.setObjectName(_fromUtf8("outputLayerButton"))
        self.horizontalLayout.addWidget(self.outputLayerButton)
        self.gridLayout.addLayout(self.horizontalLayout, 7, 0, 1, 1)
        spacerItem1 = QtGui.QSpacerItem(718, 123, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.gridLayout.addItem(spacerItem1, 8, 0, 1, 1)
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setObjectName(_fromUtf8("horizontalLayout_4"))
        spacerItem2 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem2)
        self.runButton = QtGui.QPushButton(DlgReconcile)
        self.runButton.setMinimumSize(QtCore.QSize(0, 0))
        self.runButton.setMaximumSize(QtCore.QSize(85, 27))
        self.runButton.setObjectName(_fromUtf8("runButton"))
        self.horizontalLayout_4.addWidget(self.runButton)
        self.cancelButton = QtGui.QPushButton(DlgReconcile)
        self.cancelButton.setEnabled(False)
        self.cancelButton.setMinimumSize(QtCore.QSize(0, 0))
        self.cancelButton.setMaximumSize(QtCore.QSize(85, 27))
        self.cancelButton.setObjectName(_fromUtf8("cancelButton"))
        self.horizontalLayout_4.addWidget(self.cancelButton)
        self.closeButton = QtGui.QPushButton(DlgReconcile)
        self.closeButton.setMinimumSize(QtCore.QSize(0, 0))
        self.closeButton.setMaximumSize(QtCore.QSize(85, 27))
        self.closeButton.setObjectName(_fromUtf8("closeButton"))
        self.horizontalLayout_4.addWidget(self.closeButton)
        self.gridLayout.addLayout(self.horizontalLayout_4, 9, 0, 1, 1)

        self.retranslateUi(DlgReconcile)
        QtCore.QMetaObject.connectSlotsByName(DlgReconcile)

    def retranslateUi(self, DlgReconcile):
        DlgReconcile.setWindowTitle(QtGui.QApplication.translate("DlgReconcile", "Reconcile", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("DlgReconcile", "Input Layer", None, QtGui.QApplication.UnicodeUTF8))
        self.inputLayerButton.setText(QtGui.QApplication.translate("DlgReconcile", "...", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("DlgReconcile", "Attribute Table", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("DlgReconcile", "Unique ID field", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("DlgReconcile", "Reconcile", None, QtGui.QApplication.UnicodeUTF8))
        self.lineEdit.setPlaceholderText(QtGui.QApplication.translate("DlgReconcile", "[Save to temporary file]", None, QtGui.QApplication.UnicodeUTF8))
        self.outputLayerButton.setText(QtGui.QApplication.translate("DlgReconcile", "...", None, QtGui.QApplication.UnicodeUTF8))
        self.runButton.setText(QtGui.QApplication.translate("DlgReconcile", "Run", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setText(QtGui.QApplication.translate("DlgReconcile", "Cancel", None, QtGui.QApplication.UnicodeUTF8))
        self.closeButton.setText(QtGui.QApplication.translate("DlgReconcile", "Close", None, QtGui.QApplication.UnicodeUTF8))
