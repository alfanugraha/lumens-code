FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)

ADD_SUBDIRECTORY(kosz)
ADD_SUBDIRECTORY(kosz2)

PLUGIN_INSTALL(processing ./GroupStats ${PY_FILES})
PLUGIN_INSTALL(processing ./GroupStats/icons ${ICON_FILES})
