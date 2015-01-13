FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)

ADD_SUBDIRECTORY(ui)

PLUGIN_INSTALL(processing ./raster_transparency ${PY_FILES})
PLUGIN_INSTALL(processing ./raster_transparency/icons ${ICON_FILES})
