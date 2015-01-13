# -*- coding: utf-8 -*-

"""
***************************************************************************
    aspect.py
    ---------------------
    Date                 : October 2013
    Copyright            : (C) 2013 by Alexander Bruy
    Email                : alexander dot bruy at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************
"""

__author__ = 'Alexander Bruy'
__date__ = 'October 2013'
__copyright__ = '(C) 2013, Alexander Bruy'

# This will get replaced with a git SHA1 when you do a git archive

__revision__ = '$Format:%H$'


from PyQt4.QtGui import *

from processing.core.GeoAlgorithm import GeoAlgorithm
from processing.parameters.ParameterRaster import ParameterRaster
from processing.parameters.ParameterBoolean import ParameterBoolean
from processing.parameters.ParameterNumber import ParameterNumber
from processing.outputs.OutputRaster import OutputRaster
from processing.gdal.GdalUtils import GdalUtils
from processing.tools.system import *


class aspect(GeoAlgorithm):

    INPUT = 'INPUT'
    BAND = 'BAND'
    COMPUTE_EDGES = 'COMPUTE_EDGES'
    ZEVENBERGEN = 'ZEVENBERGEN'
    TRIG_ANGLE = 'TRIG_ANGLE'
    ZERO_FLAT = 'ZERO_FLAT'
    OUTPUT = 'OUTPUT'

    #def getIcon(self):
    #    filepath = os.path.dirname(__file__) + '/icons/dem.png'
    #    return QIcon(filepath)

    def defineCharacteristics(self):
        self.name = 'Aspect'
        self.group = '[GDAL] Analysis'
        self.addParameter(ParameterRaster(self.INPUT, 'Input layer'))
        self.addParameter(ParameterNumber(self.BAND, 'Band number', 1, 99, 1))
        self.addParameter(ParameterBoolean(self.COMPUTE_EDGES, 'Compute edges',
                          False))
        self.addParameter(ParameterBoolean(self.ZEVENBERGEN,
                "Use Zevenbergen&Thorne formula (instead of the Horn's one)",
                False))
        self.addParameter(ParameterBoolean(self.TRIG_ANGLE,
                          'Return trigonometric angle (instead of azimuth)',
                          False))
        self.addParameter(ParameterBoolean(self.ZERO_FLAT,
                          'Return o for flat (instead of -9999)',
                          False))

        self.addOutput(OutputRaster(self.OUTPUT, 'Output file'))

    def processAlgorithm(self, progress):
        arguments = ['aspect']
        arguments.append(unicode(self.getParameterValue(self.INPUT)))
        arguments.append(unicode(self.getOutputValue(self.OUTPUT)))

        arguments.append('-b')
        arguments.append(str(self.getParameterValue(self.BAND)))

        if self.getParameterValue(self.COMPUTE_EDGES):
            arguments.append('-compute_edges')

        if self.getParameterValue(self.ZEVENBERGEN):
            arguments.append('-alg')
            arguments.append('ZevenbergenThorne')

        if self.getParameterValue(self.TRIG_ANGLE):
            arguments.append('-trigonometric')

        if self.getParameterValue(self.ZERO_FLAT):
            arguments.append('-zero_for_flat')

        GdalUtils.runGdal(['gdaldem',
                          GdalUtils.escapeAndJoin(arguments)], progress)
