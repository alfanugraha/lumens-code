FILE(GLOB PY_FILES *.py)

ADD_SUBDIRECTORY(models)

PLUGIN_INSTALL(processing ./molusce/algorithms ${PY_FILES})
