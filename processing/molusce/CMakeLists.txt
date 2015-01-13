FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)

ADD_SUBDIRECTORY(algorithms)
ADD_SUBDIRECTORY(ui)

PLUGIN_INSTALL(processing ./molusce ${PY_FILES})
PLUGIN_INSTALL(processing ./molusce/icons ${ICON_FILES})
