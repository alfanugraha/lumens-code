FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./raster_transparency/ui ${PY_FILES})
