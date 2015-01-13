FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)
FILE(GLOB ICON_FILE icons/gis/*.png)

PLUGIN_INSTALL(processing ./tablemanager ${PY_FILES})
PLUGIN_INSTALL(processing ./tablemanager/icons ${ICON_FILES})
PLUGIN_INSTALL(processing ./tablemanager/icons/gis ${ICON_FILE})
