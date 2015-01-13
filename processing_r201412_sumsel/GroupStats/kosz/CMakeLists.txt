FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing GroupStats/kosz ${PY_FILES})
