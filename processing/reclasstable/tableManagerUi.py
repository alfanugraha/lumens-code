# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'tableManagerUi.ui'
#
# Created: Sun Dec 15 20:24:46 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName(_fromUtf8("Dialog"))
        Dialog.setEnabled(True)
        Dialog.resize(720, 570)
        Dialog.setFocusPolicy(QtCore.Qt.NoFocus)
        Dialog.setContextMenuPolicy(QtCore.Qt.NoContextMenu)
        Dialog.setSizeGripEnabled(True)
        self.gridlayout = QtGui.QGridLayout(Dialog)
        self.gridlayout.setObjectName(_fromUtf8("gridlayout"))
        self.tabWidget = QtGui.QTabWidget(Dialog)
        self.tabWidget.setMinimumSize(QtCore.QSize(0, 0))
        self.tabWidget.setAutoFillBackground(True)
        self.tabWidget.setTabShape(QtGui.QTabWidget.Rounded)
        self.tabWidget.setElideMode(QtCore.Qt.ElideNone)
        self.tabWidget.setUsesScrollButtons(False)
        self.tabWidget.setObjectName(_fromUtf8("tabWidget"))
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName(_fromUtf8("tab_2"))
        self.gridlayout1 = QtGui.QGridLayout(self.tab_2)
        self.gridlayout1.setObjectName(_fromUtf8("gridlayout1"))
        self.fieldsTable = QtGui.QTableWidget(self.tab_2)
        self.fieldsTable.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.fieldsTable.sizePolicy().hasHeightForWidth())
        self.fieldsTable.setSizePolicy(sizePolicy)
        self.fieldsTable.setMinimumSize(QtCore.QSize(0, 280))
        self.fieldsTable.setFocusPolicy(QtCore.Qt.WheelFocus)
        self.fieldsTable.setEditTriggers(QtGui.QAbstractItemView.AnyKeyPressed|QtGui.QAbstractItemView.DoubleClicked|QtGui.QAbstractItemView.EditKeyPressed)
        self.fieldsTable.setDragDropMode(QtGui.QAbstractItemView.NoDragDrop)
        self.fieldsTable.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
        self.fieldsTable.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.fieldsTable.setGridStyle(QtCore.Qt.DotLine)
        self.fieldsTable.setWordWrap(False)
        self.fieldsTable.setCornerButtonEnabled(False)
        self.fieldsTable.setRowCount(0)
        self.fieldsTable.setColumnCount(2)
        self.fieldsTable.setObjectName(_fromUtf8("fieldsTable"))
        item = QtGui.QTableWidgetItem()
        self.fieldsTable.setHorizontalHeaderItem(0, item)
        item = QtGui.QTableWidgetItem()
        self.fieldsTable.setHorizontalHeaderItem(1, item)
        self.gridlayout1.addWidget(self.fieldsTable, 0, 0, 1, 1)
        self.vboxlayout = QtGui.QVBoxLayout()
        self.vboxlayout.setObjectName(_fromUtf8("vboxlayout"))
        self.butUp = QtGui.QToolButton(self.tab_2)
        self.butUp.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butUp.sizePolicy().hasHeightForWidth())
        self.butUp.setSizePolicy(sizePolicy)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/crystalsvg_1uparrow.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butUp.setIcon(icon)
        self.butUp.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butUp.setObjectName(_fromUtf8("butUp"))
        self.vboxlayout.addWidget(self.butUp)
        self.butDown = QtGui.QToolButton(self.tab_2)
        self.butDown.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butDown.sizePolicy().hasHeightForWidth())
        self.butDown.setSizePolicy(sizePolicy)
        self.butDown.setMinimumSize(QtCore.QSize(120, 0))
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/crystalsvg_1downarrow.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butDown.setIcon(icon1)
        self.butDown.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butDown.setObjectName(_fromUtf8("butDown"))
        self.vboxlayout.addWidget(self.butDown)
        self.butRename = QtGui.QToolButton(self.tab_2)
        self.butRename.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butRename.sizePolicy().hasHeightForWidth())
        self.butRename.setSizePolicy(sizePolicy)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/tableManagerIcon3.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butRename.setIcon(icon2)
        self.butRename.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butRename.setObjectName(_fromUtf8("butRename"))
        self.vboxlayout.addWidget(self.butRename)
        self.butDel = QtGui.QToolButton(self.tab_2)
        self.butDel.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butDel.sizePolicy().hasHeightForWidth())
        self.butDel.setSizePolicy(sizePolicy)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/mActionDeleteAttribute.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butDel.setIcon(icon3)
        self.butDel.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butDel.setObjectName(_fromUtf8("butDel"))
        self.vboxlayout.addWidget(self.butDel)
        self.butIns = QtGui.QToolButton(self.tab_2)
        self.butIns.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butIns.sizePolicy().hasHeightForWidth())
        self.butIns.setSizePolicy(sizePolicy)
        icon4 = QtGui.QIcon()
        icon4.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/mActionNewAttribute.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butIns.setIcon(icon4)
        self.butIns.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butIns.setObjectName(_fromUtf8("butIns"))
        self.vboxlayout.addWidget(self.butIns)
        self.butClone = QtGui.QToolButton(self.tab_2)
        self.butClone.setEnabled(False)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.butClone.sizePolicy().hasHeightForWidth())
        self.butClone.setSizePolicy(sizePolicy)
        icon5 = QtGui.QIcon()
        icon5.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/mActionCopySelected.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butClone.setIcon(icon5)
        self.butClone.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.butClone.setObjectName(_fromUtf8("butClone"))
        self.vboxlayout.addWidget(self.butClone)
        spacerItem = QtGui.QSpacerItem(120, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.vboxlayout.addItem(spacerItem)
        self.gridlayout1.addLayout(self.vboxlayout, 0, 1, 1, 1)
        self.tabWidget.addTab(self.tab_2, _fromUtf8(""))
        self.tab_4 = QtGui.QWidget()
        self.tab_4.setObjectName(_fromUtf8("tab_4"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.tab_4)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.dataTable = QtGui.QTableWidget(self.tab_4)
        self.dataTable.setEnabled(True)
        self.dataTable.setSelectionMode(QtGui.QAbstractItemView.SingleSelection)
        self.dataTable.setSelectionBehavior(QtGui.QAbstractItemView.SelectColumns)
        self.dataTable.setGridStyle(QtCore.Qt.DotLine)
        self.dataTable.setWordWrap(False)
        self.dataTable.setCornerButtonEnabled(False)
        self.dataTable.setObjectName(_fromUtf8("dataTable"))
        self.dataTable.setColumnCount(0)
        self.dataTable.setRowCount(0)
        self.horizontalLayout.addWidget(self.dataTable)
        self.verticalLayout_2 = QtGui.QVBoxLayout()
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.butInsRow = QtGui.QPushButton(self.tab_4)
        self.butInsRow.setText(_fromUtf8(""))
        self.butInsRow.setIcon(icon4)
        self.butInsRow.setObjectName(_fromUtf8("butInsRow"))
        self.verticalLayout_2.addWidget(self.butInsRow)
        self.butDelRow = QtGui.QPushButton(self.tab_4)
        self.butDelRow.setEnabled(False)
        self.butDelRow.setText(_fromUtf8(""))
        self.butDelRow.setIcon(icon3)
        self.butDelRow.setObjectName(_fromUtf8("butDelRow"))
        self.verticalLayout_2.addWidget(self.butDelRow)
        spacerItem1 = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem1)
        self.horizontalLayout.addLayout(self.verticalLayout_2)
        self.tabWidget.addTab(self.tab_4, _fromUtf8(""))
        self.gridlayout.addWidget(self.tabWidget, 0, 0, 1, 1)
        self.hboxlayout = QtGui.QHBoxLayout()
        self.hboxlayout.setObjectName(_fromUtf8("hboxlayout"))
        self.progressBar = QtGui.QProgressBar(Dialog)
        self.progressBar.setEnabled(True)
        self.progressBar.setAlignment(QtCore.Qt.AlignHCenter)
        self.progressBar.setTextVisible(True)
        self.progressBar.setOrientation(QtCore.Qt.Horizontal)
        self.progressBar.setInvertedAppearance(False)
        self.progressBar.setTextDirection(QtGui.QProgressBar.TopToBottom)
        self.progressBar.setFormat(_fromUtf8(""))
        self.progressBar.setObjectName(_fromUtf8("progressBar"))
        self.hboxlayout.addWidget(self.progressBar)
        spacerItem2 = QtGui.QSpacerItem(10, 20, QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Minimum)
        self.hboxlayout.addItem(spacerItem2)
        self.butSave = QtGui.QPushButton(Dialog)
        self.butSave.setEnabled(False)
        self.butSave.setMinimumSize(QtCore.QSize(0, 32))
        palette = QtGui.QPalette()
        self.butSave.setPalette(palette)
        self.butSave.setStatusTip(_fromUtf8(""))
        self.butSave.setWhatsThis(_fromUtf8(""))
        self.butSave.setAccessibleDescription(_fromUtf8(""))
        icon6 = QtGui.QIcon()
        icon6.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/mActionFileSave.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butSave.setIcon(icon6)
        self.butSave.setObjectName(_fromUtf8("butSave"))
        self.hboxlayout.addWidget(self.butSave)
        self.butSaveAs = QtGui.QPushButton(Dialog)
        self.butSaveAs.setEnabled(False)
        self.butSaveAs.setMinimumSize(QtCore.QSize(0, 32))
        icon7 = QtGui.QIcon()
        icon7.addPixmap(QtGui.QPixmap(_fromUtf8(":/processing/reclasstable/dialog/icons/mActionFileSaveAs.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.butSaveAs.setIcon(icon7)
        self.butSaveAs.setObjectName(_fromUtf8("butSaveAs"))
        self.hboxlayout.addWidget(self.butSaveAs)
        spacerItem3 = QtGui.QSpacerItem(10, 20, QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Minimum)
        self.hboxlayout.addItem(spacerItem3)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.buttonBox.sizePolicy().hasHeightForWidth())
        self.buttonBox.setSizePolicy(sizePolicy)
        self.buttonBox.setMinimumSize(QtCore.QSize(0, 32))
        self.buttonBox.setLayoutDirection(QtCore.Qt.LeftToRight)
        self.buttonBox.setAutoFillBackground(False)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Close)
        self.buttonBox.setCenterButtons(False)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.hboxlayout.addWidget(self.buttonBox)
        self.gridlayout.addLayout(self.hboxlayout, 1, 0, 1, 1)

        self.retranslateUi(Dialog)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(Dialog)
        Dialog.setTabOrder(self.buttonBox, self.tabWidget)
        Dialog.setTabOrder(self.tabWidget, self.fieldsTable)
        Dialog.setTabOrder(self.fieldsTable, self.butUp)
        Dialog.setTabOrder(self.butUp, self.butDown)
        Dialog.setTabOrder(self.butDown, self.butRename)
        Dialog.setTabOrder(self.butRename, self.butDel)
        Dialog.setTabOrder(self.butDel, self.butIns)
        Dialog.setTabOrder(self.butIns, self.butClone)
        Dialog.setTabOrder(self.butClone, self.butSave)
        Dialog.setTabOrder(self.butSave, self.butSaveAs)
        Dialog.setTabOrder(self.butSaveAs, self.dataTable)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Reconcile", None, QtGui.QApplication.UnicodeUTF8))
        item = self.fieldsTable.horizontalHeaderItem(0)
        item.setText(QtGui.QApplication.translate("Dialog", "Name", None, QtGui.QApplication.UnicodeUTF8))
        item = self.fieldsTable.horizontalHeaderItem(1)
        item.setText(QtGui.QApplication.translate("Dialog", "Type", None, QtGui.QApplication.UnicodeUTF8))
        self.butUp.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Move selected field up</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butUp.setText(QtGui.QApplication.translate("Dialog", "Move Up", None, QtGui.QApplication.UnicodeUTF8))
        self.butDown.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Move selected field down</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butDown.setText(QtGui.QApplication.translate("Dialog", "Move Down", None, QtGui.QApplication.UnicodeUTF8))
        self.butRename.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Rename selected field</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butRename.setText(QtGui.QApplication.translate("Dialog", "Rename", None, QtGui.QApplication.UnicodeUTF8))
        self.butDel.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Remove selected field</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butDel.setText(QtGui.QApplication.translate("Dialog", "Delete", None, QtGui.QApplication.UnicodeUTF8))
        self.butIns.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Insert new field</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butIns.setText(QtGui.QApplication.translate("Dialog", "Insert", None, QtGui.QApplication.UnicodeUTF8))
        self.butClone.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Clone selected field</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.butClone.setText(QtGui.QApplication.translate("Dialog", "Clone", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), QtGui.QApplication.translate("Dialog", "Fields", None, QtGui.QApplication.UnicodeUTF8))
        self.butInsRow.setToolTip(QtGui.QApplication.translate("Dialog", "<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Sans Serif\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Insert new row</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_4), QtGui.QApplication.translate("Dialog", "Table preview", None, QtGui.QApplication.UnicodeUTF8))
        self.butSave.setToolTip(QtGui.QApplication.translate("Dialog", "Save changes (available only for Shapefiles) ", None, QtGui.QApplication.UnicodeUTF8))
        self.butSave.setText(QtGui.QApplication.translate("Dialog", "Save", None, QtGui.QApplication.UnicodeUTF8))
        self.butSaveAs.setToolTip(QtGui.QApplication.translate("Dialog", "Save changes to a new layer", None, QtGui.QApplication.UnicodeUTF8))
        self.butSaveAs.setText(QtGui.QApplication.translate("Dialog", "Save as", None, QtGui.QApplication.UnicodeUTF8))

import resources_rc

if __name__ == "__main__":
    import sys
    app = QtGui.QApplication(sys.argv)
    Dialog = QtGui.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

