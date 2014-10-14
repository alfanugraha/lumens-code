# -*- coding: utf-8 -*-

# ***************************************************************************
#
# TableManager
#
# Copyright (C) 2008 Borys Jurgiel
#
# ***************************************************************************
# *                                                                         *
# *   This program is free software; you can redistribute it and/or modify  *
# *   it under the terms of the GNU General Public License as published by  *
# *   the Free Software Foundation; either version 2 of the License, or     *
# *   (at your option) any later version.                                   *
# *                                                                         *
# ***************************************************************************

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from processing.ui.ui_DlgCharacterize import Ui_DlgCharacterize
import sys


class Characterize(QDialog, Ui_DlgCharacterize):

  def __init__(self, iface):
    QDialog.__init__(self)
    self.iface = iface
    self.setupUi(self)
    self.layer = self.iface.activeLayer()
    self.provider = self.layer.dataProvider()
    self.fields = self.readFields( self.provider.fields() )
    self.isUnsaved = False  # No unsaved changes yet
    if self.provider.storageType() == 'ESRI Shapefile': # Is provider saveable?
      self.isSaveable = True
    else:
      self.isSaveable = False

    self.needsRedraw = True # Preview table is redrawed only on demand. This is for initial drawing.
    self.lastFilter = None
    self.selection = -1     # Don't highlight any field on startup
    self.selection_list = [] #Update: Santiago Banchero 09-06-2009

#     QObject.connect(self.tabWidget, SIGNAL('currentChanged (int)'), self.drawDataTable)
    self.setWindowTitle(self.tr('Statistic: {0}').format(self.layer.name()))
    self.restoreCfg()
    self.readData()
    self.drawDataTable()

  def readFields(self, providerFields): # Populates the self.fields dictionary with providerFields
    fieldsDict = {}
    i=0
    for field in providerFields:
        fieldsDict.update({i:field})
        i+=1
    return fieldsDict

  def readData(self): # Reads data from the 'provider' QgsDataProvider into the 'data' list [[column1] [column2] [column3]...]
    fields = self.fields
    self.data = []
    for i in range(len(fields)):
      self.data += [[]]
    steps = self.provider.featureCount()
    stepp = steps / 10
    if stepp == 0:
      stepp = 1
    progress = self.tr('Reading data ') # As a progress bar is used the main window's status bar, because the own one is not initialized yet
    n = 0
    for feat in self.provider.getFeatures():
        attrs = feat.attributes()

        for i in range(len(attrs)):
            self.data[i] += [attrs[i]]

        n += 1
        if n % stepp == 0:
            progress += '.'
            self.iface.mainWindow().statusBar().showMessage(progress)

    self.iface.mainWindow().statusBar().showMessage('')



  def drawDataTable(self): # Called when user switches tabWidget to the Table Preview
    if self.needsRedraw == False: return
    fields = self.fields
    self.dataTable.clear()
    self.repaint()
    self.dataTable.setColumnCount(len(fields))
    self.dataTable.setRowCount(self.provider.featureCount())
    header = []
    for i in fields.values():
      header.append(i.name())
    self.dataTable.setHorizontalHeaderLabels(header)
    formatting = True
    if formatting: # slower procedure, with formatting the table items
      for i in range(len(self.data)):
        for j in range(len(self.data[i])):
          item = QTableWidgetItem(unicode(self.data[i][j] or 'NULL'))
          item.setFlags(Qt.ItemIsSelectable | Qt.ItemIsEnabled | Qt.ItemIsEditable)
          if fields[i].type() == 6 or fields[i].type() == 2:
            item.setTextAlignment(Qt.AlignRight | Qt.AlignVCenter)
          self.dataTable.setItem(j,i,item)
    else: # about 25% faster procedure, without formatting
      for i in range(len(self.data)):
        for j in range(len(self.data[i])):
          self.dataTable.setItem(j,i,QTableWidgetItem(unicode(self.data[i][j] or 'NULL')))
    self.dataTable.resizeColumnsToContents()
    self.needsRedraw = False

  def restoreCfg(self): # Restores previous session configuration or, if fails, init defaul values
    settings = QSettings()
    self.restoreGeometry(settings.value('/Plugin-TableManager/geometry', QByteArray(), type=QByteArray))
    self.lastDirectory = settings.value('/Plugin-TableManager/lastDirectory', '.', type=unicode)
    self.lastEncoding = settings.value('/Plugin-TableManager/lastEncoding', 'UTF-8', type=unicode)
    self.lastFilter = settings.value('/Plugin-TableManager/lastFilter', '', type=unicode)

