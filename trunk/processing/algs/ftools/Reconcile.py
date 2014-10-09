# -*- coding: utf-8 -*-

"""
***************************************************************************
    Dissolve.py
    ---------------------
    Date                 : August 2012
    Copyright            : (C) 2012 by Victor Olaya
    Email                : volayaf at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************
"""

__author__ = 'Victor Olaya'
__date__ = 'August 2012'
__copyright__ = '(C) 2012, Victor Olaya'

# This will get replaced with a git SHA1 when you do a git archive

__revision__ = '$Format:%H$'

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from processing.core.GeoAlgorithm import GeoAlgorithm
from processing.core.GeoAlgorithmExecutionException import \
        GeoAlgorithmExecutionException
from processing.parameters.ParameterVector import ParameterVector
from processing.parameters.ParameterBoolean import ParameterBoolean
from processing.parameters.ParameterTableField import ParameterTableField
from processing.outputs.OutputVector import OutputVector
from processing.tools import vector, dataobjects


class Reconcile(GeoAlgorithm):

    INPUT = 'INPUT'
    OUTPUT = 'OUTPUT'
    FIELD = 'FIELD'

    #==========================================================================
    #def getIcon(self):
    #   return QtGui.QIcon(os.path.dirname(__file__) + "/icons/dissolve.png")
    #==========================================================================

    def processAlgorithm(self, progress):
        vlayerA = dataobjects.getObjectFromUri(
                self.getParameterValue(Reconcile.INPUT))
        field = vlayerA.fieldNameIndex(self.getParameterValue(Reconcile.FIELD))
        vproviderA = vlayerA.dataProvider()
        fields = vproviderA.fields()
        fieldsW = vlayerA.pendingFields()
        newFields = []
        i = 0
        for f in fieldsW:
            if i == field:
                newFields.append(f)
            i += 1
        writer = self.getOutputFromName(
                Reconcile.OUTPUT).getVectorWriter(newFields,
                                                 vproviderA.geometryType(),
                                                 vproviderA.crs())
        
        outFeat2 = QgsFeature()
        nElement = 0
        nFeat = vproviderA.featureCount()

        unique = vector.getUniqueValues(vlayerA, int(field))
        nFeat = nFeat * len(unique)
        for item in unique:
            first = True
            add = True
            features = vector.features(vlayerA)
            for inFeat in features:
                nElement += 1
                progress.setPercentage(int(nElement / nFeat * 100))
                atMap = inFeat.attributes()
                tempItem = atMap[field]
                if unicode(tempItem).strip() == unicode(item).strip():
                    if first:
                        QgsGeometry(inFeat.geometry())
                        tmpInGeom = QgsGeometry(inFeat.geometry())
                        outFeat2.setGeometry(tmpInGeom)
                        first = False
                        attrs = inFeat.attributes()
                        newAttributes = []
                        i = 0
                        for attr in attrs:
                            if i == field:
                                newAttributes.append(attr)
                            i += 1
                    else:
                        tmpInGeom = QgsGeometry(inFeat.geometry())
                        tmpOutGeom = QgsGeometry(outFeat2.geometry())
                        try:
                            tmpOutGeom = QgsGeometry(
                                    tmpOutGeom.combine(tmpInGeom))
                            outFeat2.setGeometry(tmpOutGeom)
                        except:
                            raise GeoAlgorithmExecutionException(
                                    'Geometry exception while dissolving')
            if add:
                outFeat2.setAttributes(newAttributes)
                writer.addFeature(outFeat2)
                
        del writer

    def defineCharacteristics(self):
        self.name = 'Reconcile'
        self.group = 'Vector geometry tools'
        self.addParameter(ParameterVector(Reconcile.INPUT, 'Input layer',
                          [ParameterVector.VECTOR_TYPE_POLYGON]))
        self.addParameter(ParameterTableField(Reconcile.FIELD, 'Unique ID field'
                          , Reconcile.INPUT, optional=True))
        self.addOutput(OutputVector(Reconcile.OUTPUT, 'Reconciled'))
