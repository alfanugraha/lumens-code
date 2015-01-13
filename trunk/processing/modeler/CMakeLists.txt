FILE(GLOB PY_FILES *.py)
FILE(GLOB MODEL_FILES models/*.model)
FILE(GLOB HELP_FILES models/*.help)
FILE(GLOB HELP_HTML_FILES models/*.html)

PLUGIN_INSTALL(processing modeler ${PY_FILES})
PLUGIN_INSTALL(processing modeler/models ${MODEL_FILES} ${HELP_FILES} ${HELP_HTML_FILES})
