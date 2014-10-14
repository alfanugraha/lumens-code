# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Documents and Settings\1rszostok\.qgis\python\plugins\GroupStats2\ui_groupstats.ui'
#
# Created: Fri Dec 21 19:37:28 2012
#      by: PyQt4 UI code generator 4.8.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_GroupStats(object):
    def setupUi(self, GroupStats):
        GroupStats.setObjectName(_fromUtf8("GroupStats"))
        GroupStats.resize(400, 300)
        self.buttonBox = QtGui.QDialogButtonBox(GroupStats)
        self.buttonBox.setGeometry(QtCore.QRect(30, 240, 341, 32))
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))

        self.retranslateUi(GroupStats)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), GroupStats.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), GroupStats.reject)
        QtCore.QMetaObject.connectSlotsByName(GroupStats)

    def retranslateUi(self, GroupStats):
        GroupStats.setWindowTitle(QtGui.QApplication.translate("GroupStats", "GroupStats", None, QtGui.QApplication.UnicodeUTF8))

