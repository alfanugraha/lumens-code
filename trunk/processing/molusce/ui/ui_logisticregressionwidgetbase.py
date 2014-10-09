# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ui/logisticregressionwidgetbase.ui'
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
        Widget.resize(569, 185)
        Widget.setWindowTitle(_fromUtf8(""))
        self.verticalLayout = QtGui.QVBoxLayout(Widget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.splitter = QtGui.QSplitter(Widget)
        self.splitter.setOrientation(QtCore.Qt.Horizontal)
        self.splitter.setObjectName(_fromUtf8("splitter"))
        self.layoutWidget = QtGui.QWidget(self.splitter)
        self.layoutWidget.setObjectName(_fromUtf8("layoutWidget"))
        self.gridLayout = QtGui.QGridLayout(self.layoutWidget)
        self.gridLayout.setMargin(0)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label = QtGui.QLabel(self.layoutWidget)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 1, 0, 1, 1)
        self.label_2 = QtGui.QLabel(self.layoutWidget)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 2, 0, 1, 1)
        self.lePseudoR = QtGui.QLineEdit(self.layoutWidget)
        self.lePseudoR.setObjectName(_fromUtf8("lePseudoR"))
        self.gridLayout.addWidget(self.lePseudoR, 2, 1, 1, 1)
        self.spnNeighbourhood = QtGui.QSpinBox(self.layoutWidget)
        self.spnNeighbourhood.setObjectName(_fromUtf8("spnNeighbourhood"))
        self.gridLayout.addWidget(self.spnNeighbourhood, 1, 1, 1, 1)
        self.btnFitModel = QtGui.QPushButton(self.layoutWidget)
        self.btnFitModel.setObjectName(_fromUtf8("btnFitModel"))
        self.gridLayout.addWidget(self.btnFitModel, 3, 0, 1, 2)
        spacerItem = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.gridLayout.addItem(spacerItem, 4, 0, 1, 2)
        self.spnMaxIterations = QtGui.QSpinBox(self.layoutWidget)
        self.spnMaxIterations.setMinimum(1)
        self.spnMaxIterations.setMaximum(10000)
        self.spnMaxIterations.setSingleStep(10)
        self.spnMaxIterations.setProperty("value", 100)
        self.spnMaxIterations.setObjectName(_fromUtf8("spnMaxIterations"))
        self.gridLayout.addWidget(self.spnMaxIterations, 0, 1, 1, 1)
        self.label_3 = QtGui.QLabel(self.layoutWidget)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout.addWidget(self.label_3, 0, 0, 1, 1)
        self.tabLRResults = QtGui.QTabWidget(self.splitter)
        self.tabLRResults.setObjectName(_fromUtf8("tabLRResults"))
        self.tab = QtGui.QWidget()
        self.tab.setObjectName(_fromUtf8("tab"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.tab)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.tblCoefficients = MolusceTableWidget(self.tab)
        self.tblCoefficients.setObjectName(_fromUtf8("tblCoefficients"))
        self.tblCoefficients.setColumnCount(0)
        self.tblCoefficients.setRowCount(0)
        self.horizontalLayout.addWidget(self.tblCoefficients)
        self.tabLRResults.addTab(self.tab, _fromUtf8(""))
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName(_fromUtf8("tab_2"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.tab_2)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.tblStdDev = MolusceTableWidget(self.tab_2)
        self.tblStdDev.setObjectName(_fromUtf8("tblStdDev"))
        self.tblStdDev.setColumnCount(0)
        self.tblStdDev.setRowCount(0)
        self.verticalLayout_2.addWidget(self.tblStdDev)
        self.tabLRResults.addTab(self.tab_2, _fromUtf8(""))
        self.tab_3 = QtGui.QWidget()
        self.tab_3.setObjectName(_fromUtf8("tab_3"))
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.tab_3)
        self.verticalLayout_3.setObjectName(_fromUtf8("verticalLayout_3"))
        self.tblPValues = MolusceTableWidget(self.tab_3)
        self.tblPValues.setObjectName(_fromUtf8("tblPValues"))
        self.tblPValues.setColumnCount(0)
        self.tblPValues.setRowCount(0)
        self.verticalLayout_3.addWidget(self.tblPValues)
        self.tabLRResults.addTab(self.tab_3, _fromUtf8(""))
        self.verticalLayout.addWidget(self.splitter)

        self.retranslateUi(Widget)
        self.tabLRResults.setCurrentIndex(2)
        QtCore.QMetaObject.connectSlotsByName(Widget)

    def retranslateUi(self, Widget):
        self.label.setText(QtGui.QApplication.translate("Widget", "Neighbourhood", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Widget", "Pseudo R-squared (count)", None, QtGui.QApplication.UnicodeUTF8))
        self.spnNeighbourhood.setSuffix(QtGui.QApplication.translate("Widget", " px", None, QtGui.QApplication.UnicodeUTF8))
        self.btnFitModel.setText(QtGui.QApplication.translate("Widget", "Fit model", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("Widget", "Maximum iterations", None, QtGui.QApplication.UnicodeUTF8))
        self.tabLRResults.setTabText(self.tabLRResults.indexOf(self.tab), QtGui.QApplication.translate("Widget", "Coefficients", None, QtGui.QApplication.UnicodeUTF8))
        self.tabLRResults.setTabText(self.tabLRResults.indexOf(self.tab_2), QtGui.QApplication.translate("Widget", "Standard deviations", None, QtGui.QApplication.UnicodeUTF8))
        self.tabLRResults.setTabText(self.tabLRResults.indexOf(self.tab_3), QtGui.QApplication.translate("Widget", "P-values", None, QtGui.QApplication.UnicodeUTF8))

from processing.molusce.moluscetablewidget import MolusceTableWidget
