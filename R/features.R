## ---- features.R ----
# Functions for feature engineering.

## Utilities ----
source(here::here("R/utilities.R"))

## Imports ----

library(dplyr)
library(naniar)

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

#' Update the dependent sets.
Engineer$update <- function(df) {
    # Update dataset.
    Data$df <- df
    Data$X_df <- Data$df %>% select(-Survived)
    Data$y_df <- Data$df %>% select(Survived)

    # Update training subset.
    Data$train <- Data$df %>% filter(Subset == "TRAIN")
    Data$X_train <- Data$train %>% select(-Survived)
    Data$y_train <- Data$train %>% select(Survived)

    # Update test subset.
    Data$test <- Data$df %>% filter(Subset == "TEST")
    Data$X_test <- Data$test %>% select(-Survived)
    Data$y_test <- Data$test %>% select(Survived)
}

### Engineering ----

#' Engineer AgeKnown.
Engineer$AgeKnown <- function(label = "Verify") {
    Util$tag("AgeKnown...", label = label)
    Data$df %>%
        mutate(AgeKnown = (!(is.na(Age) | Age != round(Age) & Age > 1))) %>%
        Engineer$update()
}

#' Engineer AgeDiscrete.
Engineer$AgeDiscrete <- function(label = "Cut") {
    Util$tag("AgeDiscrete...", label = label)
    Data$df %>%
        mutate(AgeDiscrete = cut(
            Age,
            breaks = c(-Inf, 1, 17, 25, 30, 50, Inf),
            right = TRUE, ordered_result= TRUE)
            ) %>%
        Engineer$update()
}

#' Engineer FareDiscrete.
Engineer$FareDiscrete <- function(label = "Cut") {
    Util$tag("FareDiscrete...", label = label)
    Data$df %>%
        mutate(FareDiscrete = cut(Fare,
                breaks = c(-Inf, seq(0, 550, by = 50), Inf),
                right = TRUE, ordered_result= TRUE)
               ) %>%
        Engineer$update()
}

#' Engineer FareRounded.
Engineer$FareRounded <- function(label = "Round") {
    Util$tag("FareRounded...", label = label)
    Data$df %>%
        mutate(FareRounded = round(Fare / 100, 1) * 100) %>%
        Engineer$update()
}

#' Engineer EmbarkedKnown
Engineer$EmbarkedKnown <- function(label = "Verify") {
    Util$tag("EmbarkedKnown...", label = label)
    Data$df %>%
        mutate(EmbarkedKnown = !is.na(Embarked)) %>%
        Engineer$update()
}

#' Extract features.
Features$extract <- function() {

    Util$task("Feature Eng.: Age",
              task = (function(...){
                  Engineer$AgeKnown(...)
                  Engineer$AgeDiscrete(...)
                  Data$df %>% select(Age, AgeKnown, AgeDiscrete) %>%
                      print() %>%
                      miss_var_summary() %>% print()
              }),
              onStart = "Extracting latent Age features...")

    Util$task("Feature Eng.: Fare",
              task = (function(...){
                  Engineer$FareRounded(...)
                  Engineer$FareDiscrete(...)
                  Data$df %>% select(Fare, FareRounded, FareDiscrete) %>%
                      print() %>%
                      miss_var_summary() %>% print()
              }),
              onStart = "Extracting latent Fare features...")

    Util$task("Feature Eng.: Embarked",
              task = (function(...){
                  Engineer$EmbarkedKnown(...)
                  Data$df %>% select(Embarked, EmbarkedKnown) %>%
                      print() %>%
                      miss_var_summary() %>% print()
              }),
              onStart = "Extracting latent Embarked features...")

    stop()

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
