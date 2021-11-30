# loader.R
# --------
# Functions for loading data.

# Load the magrittr library.
library(magrittr)

## ---- Data Specifications ----

#' Get filenames.
get.filenames <- function() {
    return(list(
        train = here::here("data/train.csv"),
        test = here::here("data/test.csv")
    ))
}

#' Get column names.
get.colnames <- function() {
    return(c(
        "PassengerId",
        "Survived",
        "Pclass",
        "Name",
        "Sex",
        "Age",
        "SibSp",
        "Parch",
        "Ticket",
        "Fare",
        "Cabin",
        "Embarked"
    ))
}

#' Get column types.
get.coltypes <- function() {
    return(list(
        # Unique passenger id. Index variable.
        `PassengerId` = readr::col_double(),

        # Survival response. 0 = No, 1 = Yes.
        `Survived` = readr::col_factor(),

        # Ticket class. 1 = 1st, 2 = 2nd, 3 = 3rd.
        # Proxy for socio-economic status (SES).
        `Pclass` = readr::col_factor(levels = c("1", "2", "3")),

        # Name of the passenger.
        `Name` = readr::col_character(),

        # Gender.
        `Sex` = readr::col_factor(),

        # Age (in years). Fractional if less than 1.
        `Age` = readr::col_double(),

        # Number of siblings / spouses aboard
        `SibSp` = readr::col_integer(),

        # Number of parents / children aboard
        `Parch` = readr::col_integer(),

        # Unique ticket number.
        `Ticket` = readr::col_character(),

        # Passenger fare.
        `Fare` = readr::col_double(),

        # Cabin number.
        `Cabin` = readr::col_character(),

        # Port of embarkment.
        `Embarked` = readr::col_factor(levels = c("S", "C", "Q"))
    ))
}

## ---- Loading Train/Test Data ----

#' Load training data into memory.
load.data.train <- function() {
    filenames <- get.filenames()
    col_names <- get.colnames()
    col_types <- get.coltypes()
    Titanic.train <<- readr::read_csv(
        file = filenames$train,
        col_names = col_names,
        col_types = col_types,
        skip = 1
    ) %>% dplyr::mutate(Survived = factor(Survived, labels = c("Died", "Survived")))
    return(Titanic.train)
}

#' Load test data into memory.
load.data.test <- function() {
    filenames <- get.filenames()
    col_names <- get.colnames()
    col_types <- get.coltypes()
    Titanic.test <<- readr::read_csv(
        file = filenames$test,
        col_names = magrittr::extract(col_names, -2),
        col_types = magrittr::extract(col_types, -2),
        skip = 1
    )
    return(Titanic.test)
}

#' Load the train and test data.
load.data <- function() {
    df <- new.env(parent = emptyenv())
    df$train <- load.data.train()
    df$test <- load.data.test()
    Titanic.df <<- df$train %>%
        dplyr::mutate(Subset = "Train") %>%
        dplyr::bind_rows(df$test %>% dplyr::mutate(Survived = NA, Subset = "Test"))
    return(df)
}

#' Sample rows from a dataframe.
sample.df <- function(df, size, replace = FALSE, prob = NULL) {
    rowcount <- nrow(df)
    indices <- sample(1:rowcount, size, replace = replace, prob = prob)
    return(df[indices,])
}
