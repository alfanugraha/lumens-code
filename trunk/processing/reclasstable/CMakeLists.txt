FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)
FILE(GLOB ICON_FILE icons/gis/*.png)

PLUGIN_INSTALL(processing ./reclasstable ${PY_FILES} reclass_template.csv reclass_template.cpg reclass_template.dbf reclass_template.prj reclass_template.qix reclass_template.qpj reclass_template.shp reclass_template.shx)
PLUGIN_INSTALL(processing ./reclasstable/icons ${ICON_FILES})
PLUGIN_INSTALL(processing ./reclasstable/icons/gis ${ICON_FILE})
