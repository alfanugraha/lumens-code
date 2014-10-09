
import os, sys
from PyQt4 import QtGui, QtCore
from processing.gui.ParametersDialog import ParametersDialog
from processing.modeler.ModelerAlgorithm import ModelerAlgorithm
from processing.modeler import ModelerAlgorithmProvider
from processing.modeler.Providers import Providers
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from qgis.core import *
from qgis.gui import *

class NumberCombine(QtGui.QDialog):
    
    def __init__(self):
        QtGui.QDialog.__init__(self)
                
        self.resize(200, 100)
        self.verticalLayout = QtGui.QVBoxLayout(self)
        self.combo = QtGui.QComboBox(self)
        for i in range(2,11):
            self.combo.addItem(str(i))
        self.verticalLayout.addWidget(self.combo)
        self.buttonBox = QtGui.QDialogButtonBox(self)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName('buttonBox')
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL('accepted()'),
                        self.okPressed)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL('rejected()'),
                        self.cancelPressed)
        
        self.verticalLayout.addWidget(self.buttonBox)
        self.setWindowTitle('Number of Data Layer')
        self.show()
        
    def okPressed(self):
        self.openCombine(self.combo.currentIndex() + 2)
        self.close()
            
    def openCombine(self, num):
        folder = \
            os.path.join(os.path.dirname(ModelerAlgorithmProvider.__file__),
                         'models')
        f = os.path.join(folder, 'combine_' + str(num) + 'vector.model')
        try:
            m = ModelerAlgorithm()
            m.openModel(f)
            self.alg = m
            self.alg.provider = Providers.providers['model']
            dlg = ParametersDialog(self.alg)
            dlg.exec_()
        except Exception, e:
            QMessageBox.information(None,QCoreApplication.translate("PUR", "Combine planning unit layers"), \
                QCoreApplication.translate("PUR","GRASS provider is not configured.\nPlease configure it before running this model."))  
            return   
        
    def cancelPressed(self):
        self.close()
        