## ---- code.R ----
# Pipeline script that will:
# - Load the data.
# - Engineer new features.
# - Impute missing data.
# - Fit a set of statistical learning models.
# - Output predictions to a submission '*.csv'.

## Flags ----

# Set project verbosity. TRUE if run interactively.
VERBOSE <<- interactive()
# VERBOSE <<- FALSE # Uncomment to turn off verbose logging.

## Imports ----

# Only dependency we need to manually install if missing.
if(!("here" %in% installed.packages()[,"Package"])) {
    message("Installing missing dependency.")
    install.packages("here")
}

### Packages ----
source(here::here("R/packages.R"))

### Utilities ----
source(here::here("R/utilities.R"))

### Wrangling ----
source(here::here("R/loader.R"))

### Feature Engineering ----
source(here::here("R/features.R"))

### Model Learning ----

Model <<- new.env(parent = emptyenv())

### Model Validation ----

Analyse <<- new.env(parent = emptyenv())

### Prediction ----

Predict <<- new.env(parent = emptyenv())



## Pipeline Composer ----

# Construct an environment for storing task functions.
Pipeline <<- new.env(parent = emptyenv())

#' Execute the Tasks pipeline.
Pipeline$execute <- function(verbose = VERBOSE) {

    # Install package dependencies.
    Util$task("Package Verifier",
              task = verify.packages,
              task.args = list(name = PACKAGES, quiet = !verbose),
              onStart = "Verifying packages...")

    # Load the data.
    Util$task("Loader",
              task = Load$dataset,
              onStart = "Loading Titanic data subsets...")

    # Engineer features.
    Util$task("Feature Engineering",
              task = Features$extract,
              onStart = "Engineering features for dataset...")

    # Impute missing data.
    Util$task("Data Imputation",
              task = Features$impute,
              onStart = "Imputing missing values for dataset...")

}

## Interactive ----

if (interactive()) {
    # Execute on source and report runtime when used interactively.
    Util$task("Pipeline",
              task = Pipeline$execute,
              onStart = "Executing prediction pipeline for RIT.STAT745.Team2...",
              onComplete = "Pipeline execution complete.")
}
