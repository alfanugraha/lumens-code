import os, sys, csv
import codecs
from PyQt4 import QtGui, QtCore
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from processing.molusce.moluscetablewidget import MolusceTableWidget

class EditCrossTabulation(QtGui.QDialog):
    def __init__(self):
        QtGui.QDialog.__init__(self)
        
        self.resize(777, 467)
        self.verticalLayout = QtGui.QVBoxLayout(self)
        self.crossTabulation = MolusceTableWidget(self)
        #show empty table
        self.verticalLayout.addWidget(self.crossTabulation)
        
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.openButton = QtGui.QPushButton("Open CSV", self)
        self.horizontalLayout.addWidget(self.openButton)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.saveButton = QtGui.QPushButton("Save", self)
        self.horizontalLayout.addWidget(self.saveButton)
        self.closeButton = QtGui.QPushButton("Close", self)
        self.horizontalLayout.addWidget(self.closeButton)
        self.verticalLayout.addLayout(self.horizontalLayout)
        
        self.closeButton.clicked.connect(self.close)
        self.openButton.clicked.connect(self.handleOpen)   
        self.saveButton.clicked.connect(self.handleSave)    
        self.initShortcuts() 
        
        self.setWindowTitle('File Dialog')
        self.show()
                    
    def handleSave(self):
        savefile = QtGui.QFileDialog.getSaveFileName(
                self, 'Save File', '', 'CSV(*.csv)')
        if savefile != '':
            with open(unicode(savefile), 'wb') as stream:
                writer = csv.writer(stream)
                for row in range(self.crossTabulation.rowCount()):
                    rowdata = []
                    for column in range(self.crossTabulation.columnCount()):
                        item = self.crossTabulation.item(row, column)
                        if item is not None:
                            rowdata.append(
                                unicode(item.text()).encode('utf8'))
                        else:
                            rowdata.append('')
                    writer.writerow(rowdata)

    def handleOpen(self):
        filename = QtGui.QFileDialog.getOpenFileName(
                self, 'Open File', '', 'CSV(*.csv)')
        if filename != '':
            with open(unicode(filename), 'rb') as stream:
                self.crossTabulation.setRowCount(0)
                self.crossTabulation.setColumnCount(0)
                for rowdata in csv.reader(stream):
                    row = self.crossTabulation.rowCount()
                    self.crossTabulation.insertRow(row)
                    self.crossTabulation.setColumnCount(len(rowdata))
                    for column, data in enumerate(rowdata):
                        item = QtGui.QTableWidgetItem(data.decode('utf8'))
                        self.crossTabulation.setItem(row, column, item)
                        
    def openCSV(self):
        labels = []
        checkLabels = False
        setting = QtCore.QSettings()
        path = unicode(setting.value('/Processing/LastInputPath'))        
        filename = QtGui.QFileDialog.getOpenFileName(self, self.tr("Open File"), path, self.tr("Text CSV (*.csv)"))
        if filename != '':
            lines = codecs.open(filename, 'r', encoding='utf-8')
            line = lines.readline().strip('\n').strip('\r')
            iRow = 0
            try:
                while line != "":
                    tokens = line.split(',')
                    tokensLength = len(tokens) 
                    if checkLabels == False:
                        for i in range(1, tokensLength):
                            labels.append(tokens[i])
                        self.crossTabulation.setRowCount(tokensLength - 1)
                        self.crossTabulation.setColumnCount(tokensLength - 1)
                        self.crossTabulation.setHorizontalHeaderLabels(labels)
                        self.crossTabulation.setVerticalHeaderLabels(labels)
                        checkLabels = True
                    else:
                        for i in range(1, tokensLength):
                            item = QtGui.QTableWidgetItem(tokens[i])
                            self.crossTabulation.setItem(iRow, i-1, item)
                        iRow += 1
                    line = lines.readline().strip('\n').strip('\r')
            except Exception, e:
                raise e
        
    def saveCSV(self):
        savefile = QtGui.QFileDialog.getSaveFileName(self, self.tr("Save File"), '', self.tr("Text CSV (*.csv)"))  
        if savefile != '':
            csvOut = codecs.open(savefile, 'w', encoding='utf-8')
            writer = csv.writer(csvOut)
            rowHeader = [u'']
            rowLen = self.crossTabulation.rowCount()
            for h in range(rowLen):
                headColumn = self.crossTabulation.horizontalHeaderItem(h)
                rowHeader.append(unicode(headColumn.text()).encode('utf8'))
            writer.writerow(rowHeader)
                 
            for row in range(rowLen):
                headRow = self.crossTabulation.horizontalHeaderItem(row)
                r = unicode(headRow.text()).encode('utf8')
                rowData = [r]
                for column in range(rowLen):
                    item = self.crossTabulation.item(row, column)
                    if item is not None:
                        rowData.append(unicode(item.text()).encode('utf8'))
                    else:
                        rowData.append('')
                writer.writerow(rowData)           
                
    def initShortcuts(self): 
        self.shortcutPaste = QtGui.QShortcut(QtGui.QKeySequence(Qt.CTRL + Qt.Key_V), self)
        self.shortcutPaste.activated.connect(self.handlePaste)   
        
    def handlePaste(self):
        clipboard = QtGui.QApplication.instance().clipboard().text()
        
        rows = clipboard.split('\n')
        numRows = len(rows) - 1
        cols = rows[0].split('\t')
        numCols = len(cols)
        
        for row in xrange(numRows):
            columns = rows[row].split('\t')
            for col in xrange(numCols):
                item = QtGui.QTableWidgetItem(u"%s" % columns[col])
                self.crossTabulation.setItem(row, col, item)
