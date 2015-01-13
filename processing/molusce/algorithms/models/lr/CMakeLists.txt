FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./molusce/algorithms/models/lr ${PY_FILES})
