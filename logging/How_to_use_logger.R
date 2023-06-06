library(logger)

# Set the layout for the logger -------------------------------------------

# 1. Either jsut in a json format
log_layout(layout_json())


# 2. Or only rudimentary information
logger <- layout_glue_generator(format = '{time} {level}: {msg}')
log_layout(logger)


# log_threshold -----------------------------------------------------------

log_threshold(TRACE)


# Open file and append ---------------------------------------------------------------

fl = file.path(getwd(), paste0("logging/logs/test", ".log"))
log_appender(appender_file(fl))
log_info("Where is this message going?")
log_debug('Error at location A')

readLines(con = fl)
