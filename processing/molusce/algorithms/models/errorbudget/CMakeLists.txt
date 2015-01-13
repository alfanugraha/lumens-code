FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./molusce/algorithms/models/errorbudget ${PY_FILES})
