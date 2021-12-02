## ---- features.R ----
# Functions for feature engineering.

## Utilities ----
source(here::here("R/utilities.R"))

## Imports ----


## Exports ----

#' Environment for feature extractors.
Engineer <<- new.env(parent = emptyenv())

#' Environment for feature engineering tasks.
Features <<- new.env(parent = emptyenv())

### Helpers ----

#' Generate a range of formatted, sequential names.
Engineer$name_range <- function(prefix = "X", range = 1:5, suffix = NULL, sep = "", width = 1) {
    number.format <- sprintf("%%0%id", width)
    name.format <- paste(prefix, number.format, suffix, sep = sep, collapse = "")
    if (is.null(suffix)) { name.format }
    return(c(sprintf(name.format, range)))
}

### Engineering ----

#' Extract features.
Features$extract <- function() {

    Util$task("Feature Eng.: Age",
              task = Util$stub(
                  "AgeKnown...",
                  "AgeDiscrete..."),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Age features...")

    Util$task("Feature Eng.: Fare",
              task = Util$stub(
                  "FareDiscrete..."),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Fare features...")

    Util$task("Feature Eng.: Embarked",
              task = Util$stub(
                  "EmbarkedKnown..."),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Fare features...")

    Util$task("Feature Eng.: Cabin",
              task = Util$stub(
                  "CabinKnown...",
                  "Cabin.#...",
                  "Cabin.#.Deck...",
                  "Cabin.#.Number...",
                  "Cabin.Count"),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Cabin features...")

    Util$task("Feature Eng.: Ticket",
              task = Util$stub(
                  "Ticket.Number",
                  "Ticket.Prefix",
                  "Ticket.Category",
                  "Ticket.Adjacent"),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Ticket features...")

    Util$task("Feature Eng.: Name",
              task = Util$stub(
                  "Title",
                  "Name",
                  "Given.Forename",
                  "Given.Middlename",
                  "Surname",
                  "Spouse.Forename",
                  "Spouse.Middlename",
                  "Spouse.Surname",
                  "IsMarried",
                  "Guardian.Forename",
                  "Guardian.Middlename",
                  "Guardian.Surname",
                  "IsGuardian"),
              task.args = list(label = "Extract"),
              onStart = "Extracting latent Ticket features...")

}

### Imputation ----

#' Impute missing values.
Features$impute <- function() {

    Util$task("Impute: Age",
              task = Util$stub(
                  "Age...",
                  "AgeDiscrete..."),
              task.args = list(label = "Impute"),
              onStart = "Imputing missing values in ...")

    Util$task("Impute: Fare",
              task = Util$stub(
                  "Fare...",
                  "FareDiscrete..."),
              task.args = list(label = "Impute"),
              onStart = "Imputing missing values in ...")

    Util$task("Impute: Fare",
              task = Util$stub(
                  "Fare...",
                  "FareDiscrete..."),
              task.args = list(label = "Impute"),
              onStart = "Imputing missing values in ...")

    Util$task("Impute: Cabin",
              task = Util$stub(
                  "Cabin.1.Deck...",
                  "Cabin.1.Number...",
                  "Cabin.1...",
                  "Cabin..."),
              task.args = list(label = "Impute"),
              onStart = "Imputing missing values in ...")

    Util$task("Impute: Embarked",
              task = Util$stub(
                  "Embarked..."),
              task.args = list(label = "Impute"),
              onStart = "Imputing missing values in ...")

}
