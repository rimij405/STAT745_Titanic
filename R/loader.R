## ---- loader.R ----
# Functions for loading data.

## Imports ----

library(magrittr)
library(readr)
library(dplyr)
library(tibble)

## Exports ----

#' Environment for data constants.
Data <<- new.env(parent = emptyenv())

#' Environment for data loader functions.
Load <<- new.env(parent = emptyenv())

### Constants ----

#### FILENAMES ----
#' List of dataset subset files.
Data$FILENAMES <- list(
    train = here::here("data/train.csv"),
    test = here::here("data/test.csv")
)

#### FIELDS ----
#' List of all field names.
Data$FIELDS <- list(
    `PassengerId` = "PassengerId",
    `Survived` = "Survived",
    `Pclass` = "Pclass",
    `Name` = "Name",
    `Sex` = "Sex",
    `Age` = "Age",
    `SibSp` = "SibSp",
    `ParCh` = "ParCh",
    `Ticket` = "Ticket",
    `Fare` = "Fare",
    `Cabin` = "Cabin",
    `Embarked` = "Embarked"
)

#### FIELDTYPES ----
#' Field data types.
Data$FIELDTYPES <- readr::cols(
    # Unique passenger id. Index variable.
    `PassengerId` = readr::col_double(),

    # Survival response. 0 = No, 1 = Yes.
    `Survived` = readr::col_factor(levels = c("0", "1")),

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
    `ParCh` = readr::col_integer(),

    # Unique ticket number.
    `Ticket` = readr::col_character(),

    # Passenger fare.
    `Fare` = readr::col_double(),

    # Cabin number.
    `Cabin` = readr::col_character(),

    # Port of embarkment.
    `Embarked` = readr::col_factor(levels = c("S", "C", "Q"))
)


#### RESPONSE ----
#' Response column names.
Data$RESPONSE <- Data$FIELDS %>%
    magrittr::extract(2)

#### FEATURES ----
#' Feature column names.
Data$FEATURES <- Data$FIELDS %>%
    magrittr::extract(-2)

#### FEATURE TYPES ----
Data$FEATURETYPES <- Data$FIELDTYPES %>%
    magrittr::extract2(1) %>%
    magrittr::extract(-2)

### Loaders ----

#' Load the training subset.
Load$subset.train <- function() {
    Util$notify("Loading training subset...")
    Data$train <- read_csv(
        file = Data$FILENAMES$train,
        col_names = names(Data$FIELDS),
        col_types = Data$FIELDTYPES,
        skip = 1) %>%
        dplyr::mutate(
            Survived = factor(Survived, labels = c("Died", "Survived")))
    Data$X.train <- Data$train %>% select(-Survived)
    Data$Y.train <- Data$train %>% select(Survived)
    return(Data$train)
}

#' Load the testing subset.
Load$subset.test <- function() {
    Util$notify("Loading test subset...")
    Data$test <- read_csv(
        file = Data$FILENAMES$test,
        col_names = names(Data$FEATURES),
        col_types = Data$FEATURETYPES,
        skip = 1)
    Data$X.test <- Data$test %>% select(everything())
    Data$Y.test <- Data$test %>% transmute(Survived = NA)
    return(Data$test)
}

#' Load the training and testing subsets.
Load$dataset <- function() {
    train.df <- Load$subset.train()
    test.df <- Load$subset.test() %>% mutate(Survived = NA)
    Util$notify("Combining subsets into Titanic superset...")
    Data$df <- list(
            TRAIN = train.df,
            TEST = test.df) %>%
        bind_rows(.id = "Subset")
    Data$X.df <- Data$df %>% select(-Survived)
    Data$Y.df <- Data$df %>% select(Survived)
    return(Data$df)
}
