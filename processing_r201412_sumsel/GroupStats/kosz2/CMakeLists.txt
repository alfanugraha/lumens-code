FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing GroupStats/kosz2 ${PY_FILES})
