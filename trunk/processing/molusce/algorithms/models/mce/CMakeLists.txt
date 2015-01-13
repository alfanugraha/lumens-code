FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./molusce/algorithms/models/mce ${PY_FILES})
