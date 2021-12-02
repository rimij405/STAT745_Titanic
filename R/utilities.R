## ---- utilities.R ----
# Helper functions for running main program.

## Imports ----

library(tictoc)
library(stringr)

## Exports ----

Util <<- new.env(parent = emptyenv())

### Console Output ----

#' Print a notification message.
Util$notify <- function(..., verbose = (exists("VERBOSE", envir = parent.frame()) && VERBOSE)) {
   if (verbose) message(...)
}

#' Print a border segment of `width`, `n` times.
Util$border <- function(border = "-", width = 10, n = 1, indent = 0) {
    segment <- str_wrap(str_dup(border, width), indent = indent)
    writeLines(c(rep(segment, times = n)))
}

#' Print a log message.
Util$log <- function(label = "SECTION LABEL:", width = str_length(label)) {
    cat(str_wrap(string = c(label), width = width), sep = "\n")
}

#' Print a bordered section label.
Util$section <- function(label = "SECTION LABEL:", border = "-", width = str_length(label), border_n = 1) {
    Util$border(border, width = width, n = border_n)
    cat(str_wrap(string = c(label), width = width), sep = "\n")
    Util$border(border, width = width, n = border_n)
}

#' Print a bordered section label.
Util$header <- function(label = "HEADER LABEL:", border = "=", width = str_length(label), border_n = 1, level = 0) {
    Util$border(border, width = width, n = border_n, indent = level)
    cat(str_wrap(string = c(label), width = width, indent = level), sep = "\n")
}

### Task Orchestration ----

#' tic function with border.
Util$tic <- function(label = "tic", width = 60, ...) { tic(label); Util$border(width = width); }

#' toc function with border.
Util$toc <- function(quiet = TRUE, width = 60, ...) { toc(quiet = quiet); Util$border(width = width); }

#' stub function.
Util$stub <- function(...) {
    return(function(label = "Stub"){
        message(sprintf("[%s]: %s\n", label, list(...)))
    })
}

#' Timed call.
Util$task <- function(label, task,
                           task.args = list(),
                           task.error = function(cond) { stop(cond); return(NA); },
                           task.warning = function(cond) { warning(cond); return(NULL); },
                           task.finally = {},
                           onStart = NULL,
                           onComplete = NULL,
                           verbose = (!is.null(onStart) || !is.null(onComplete)),
                           time = TRUE) {
    res <- tryCatch(
        expr = {
            if (time) Util$tic(label)
            if (!is.null(onStart) && verbose) Util$log(onStart)
            do.call(task, task.args)
        },
        error = task.error,
        warning = task.warning,
        finally = {
            if (!is.null(onComplete) && verbose) Util$log(onComplete)
            if (time) Util$toc(quiet = !verbose)
        }
    )
    return(res)
}

### Non-standard Evaluation (NSE) Parsing ----

#' Get function name as String.
Util$parse <- function(FUNC) {
    str_c(as.character(substitute(FUNC)), sep = "", collapse = NULL)
}
