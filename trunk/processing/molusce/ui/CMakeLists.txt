FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./molusce/ui ${PY_FILES})
