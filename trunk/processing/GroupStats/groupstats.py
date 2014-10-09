# -*- coding: utf-8 -*-
"""
/***************************************************************************
 GroupStats
                                 A QGIS plugin
 Oblicza statystyki danych
                              -------------------
        begin                : 2012-12-21
        copyright            : (C) 2012 by Rayo
        email                : rayo001@interia.pl
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
# Import the PyQt and QGIS libraries
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
import os.path
# Initialize Qt resources from file resources.py
import processing.GroupStats.resources_rc
# Import the code for the dialog
#from groupstatsdialog import GroupStatsDialog
from processing.GroupStats.GroupStatsDialog import GroupStatsDialog


class GroupStats:

    def __init__(self, iface):
        # Save reference to the QGIS interface
        self.iface = iface
        # Create the dialog and keep reference
        self.dlg = GroupStatsDialog()
        
        # initialize locale
        pluginPath = QFileInfo(os.path.realpath(__file__)).path()  # patch by Régis Haubourg
        localeName = QLocale.system().name()
        
        if QFileInfo(pluginPath).exists():
            self.localePath = pluginPath + "/i18n/groupstats_" + localeName + ".qm"            
        
        if QFileInfo(self.localePath).exists():
            self.translator = QTranslator()
            self.translator.load(self.localePath)
            if qVersion() > '4.3.3':
                QCoreApplication.installTranslator(self.translator)

    def initGui(self):    
        
        # Create action that will start plugin configuration
        self.action = QAction(
            QIcon(":/plugins/groupstats/icon.png"),
            u"GroupStats", self.iface.mainWindow())
        # connect the action to the run method
        QObject.connect(self.action, SIGNAL("triggered()"), self.run)
        
        # Add toolbar button and menu item
        if hasattr( self.iface, 'addDatabaseToolBarIcon' ):
            self.iface.addVectorToolBarIcon(self.action)
        else:
            self.iface.addToolBarIcon(self.action)
        if hasattr( self.iface, 'addPluginToVectorMenu' ):
            self.iface.addPluginToVectorMenu( u"&Group Stats", self.action )
        else:
            self.iface.addPluginToMenu("&Group Stats", self.action)

    def unload(self):
        # Remove the plugin menu item and icon
        if hasattr( self.iface, 'removePluginVectorMenu' ):
            self.iface.removePluginVectorMenu( u"&Group Stats", self.action )
        else:
            self.iface.removePluginMenu( u"&Group Stats", self.action )
        if hasattr( self.iface, 'removeVectorToolBarIcon' ):
            self.iface.removeVectorToolBarIcon(self.action)
        else:
            self.iface.removeToolBarIcon(self.action)

    # run method that performs all the real work
    def run(self):
        
        warstwy = QgsMapLayerRegistry.instance().mapLayers()        # Wczytanie nazw warstw z projektu
        
        listaWarstw = []
        for id in warstwy.keys():                                   # Wczytanie nazw warstw do okna
            if warstwy[id].type()==0:
                listaWarstw.append((warstwy[id].name(), id))
        
        if len(listaWarstw) == 0 or len(warstwy) == 0:
            QMessageBox.information(None,QCoreApplication.translate('GroupStats','Information'), \
                QCoreApplication.translate('GroupStats','Vector layers not found'))
            return
        self.dlg.iface = self.iface
        self.dlg.ustawWarstwy(listaWarstw)
        
        # show the dialog
        self.dlg.show()

