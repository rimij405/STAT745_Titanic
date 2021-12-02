## ---- packages.R ----
#' Installs missing packages necessary for the project to run.

## Packages ----

#' Constant containing all packages used in this project.
PACKAGES <<- list(
    util = c("renv", "here", "tictoc"),
    report = c("knitr", "rmarkdown", "markdown"),
    stat = c("sjstats", "ltm", "naniar", "skimr"),
    tidy = c("magrittr", "readr", "dplyr", "tidyverse"),
    vis = c("ggplot2", "qqplotr", "ggmosaic", "visdat", "vcd"),
    model = c("simputation", "e1071", "randomForest", "neuralnet")
)

## Helpers ----

#' Verify if a package is installed. If not, attempt to install it.
verify.package <- function(name, quiet = TRUE) {
    out <- tryCatch(
        expr = {
            if(!(name %in% installed.packages()[,"Package"])) {
                warning(sprintf("Package '%s' has not been installed.\n", name))
            } else {
                if(!quiet) {
                    message(sprintf("Package '%s' is already installed.\n", name))
                }
            }
        },
        error = function(cond){
            # An error occurred.
            message(sprintf("Error: %s", cond))
            return(FALSE)
        },
        warning = function(cond) {
            # A warning was raised.
            message(cond)
            res <- tryCatch(
                expr = {
                    install.packages(name)
                    return(TRUE);
                },
                error = function(cond) {
                    if(!quiet) {
                        message(cond);
                        cat("\n\n");
                    }
                    return(FALSE)
                }
            )
            return(res && (name %in% installed.packages()[,"Package"]))
        })
    return(out)
}

#' Vectorize scalar verify.package function.
verify.packages <- function(names, quiet = TRUE) {
    if (exists("VERBOSE", envir = parent.frame()) && VERBOSE) message("Verifying dependencies...")
    verify.all <- Vectorize(verify.package, vectorize.args = c("name"))
    res <- verify.all(name = unlist(names), quiet = quiet)
    if (exists("VERBOSE", envir = parent.frame()) && VERBOSE) message("Done.")
    return(res)
}
