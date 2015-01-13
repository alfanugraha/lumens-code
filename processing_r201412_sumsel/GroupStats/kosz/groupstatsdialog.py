# -*- coding: utf-8 -*-
"""
/***************************************************************************
 GroupStatsDialog
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

from PyQt4 import QtCore, QtGui
from ui_groupstats import Ui_GroupStats
# create the dialog for zoom to point


class GroupStatsDialog(QtGui.QDialog):
    def __init__(self):
        QtGui.QDialog.__init__(self)
        # Set up the user interface from Designer.
        self.ui = Ui_GroupStats()
        self.ui.setupUi(self)
