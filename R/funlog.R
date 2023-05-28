# https://stackoverflow.com/questions/7307987/logging-current-function-name#answer-7321668
curfnfinder<-function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
                      retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t") {
  prefix <- sapply(3 + skipframes+1:sys.nframe(), function(i){
    currv <- sys.call(sys.parent(n=i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix <- gsub("function[ \t]*\\(.*", "do.call", prefix)
  if(length(prefix)==0) return(retIfNone)
  if(retStack) return(paste(rev(prefix), collapse = "|"))
  retval <- as.character(unlist(prefix[1]))
  if(length(prefix) > 1) {
    retval <- paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
  }
  return(retval)
}

#' Create a logging function
#'
#' @param default_level The default logging level of the function. Use one of the options in the global LEVEL variable.
#'
#' @return A function taking the a message and optionally a log level
#' The message can use the glue libraries {} syntax.
#' @export
#'
#' @examples
#' LOG <- makeLogger(funlog::LEVEL$TRACE)
#' LOG("2+2={2+2}")
makeLogger <- function(default_level='trace') {
  LOG <<- function(msg, level=default_level, .envir=parent.frame()) {
    FROM <- tryCatch(this.path::this.path(),error=function(e) '.')
    FUNC <- stringr::str_trim(curfnfinder(skipframes = 1))
    msg <- as.character(glue::glue(as.character(msg), .envir=.envir))
    PRE <- paste0('(',
                  crayon::italic(paste0(fs::path_rel(FROM), ':', FUNC)),
                  ') ')
    MSG <- paste0(PRE, msg)
    switch (tolower(level),
            'trace' = rlog::log_trace,
            'info'  = rlog::log_info,
            'error' = rlog::log_error,
            'fatal' = rlog::log_fatal,
            'warn'  = rlog::log_warn,
            'debug' = rlog::log_debug
    )(MSG)
  }
  LOG
}

#' Set the log level
#'
#' @param level Can be anything in `LEVEL`
#'
#' @return NULL
#' @export
#'
#' @examples
#' setLogLevel(funlog::LEVEL$TRACE)
setLogLevel <- function(level) {
  Sys.setenv(LOG_LEVEL=toupper(level))
}
