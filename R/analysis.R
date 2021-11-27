# analysis.R
# -------
# Functions for fitting models.

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
        `Pclass` = readr::col_factor(),

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
    )
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
    return(df)
}

## ---- Age Imputer ----

#' Fit age regressor on the training model..
age.lm <- function(df.train) {
    # Prepare dataset with no missing ages.
    df.train.ages <- df.train %>%
        dplyr::filter(!is.na(Age))

    # Fit a model for predicting Age.
    return(lm(Age ~ Pclass + Sex + Parch + Fare, df.train.ages))
}

#' Predict set of missing ages.
#' @param age.model Fitted model that will predict age based on set of predictors.
#' @param df.missing Dataset with missing ages.
predict.ages <- function(age.model, df.missing) {
    return(predict(age.model, newdata = df.missing))
}

#' Impute missing ages into dataset.
#' @param age.model Fitted model that will predict age based on set of predictors.
#' @param df.passengers Dataset with potentially missing ages.
impute.ages <- function(age.model, df.passengers) {
    # Select entries missing an age value.
    df.missing <- df.passengers %>%
        dplyr::filter(is.na(Age))

    # Make predictions and assign it back to the ages field.
    df.ages <- df.missing %>%
        dplyr::mutate(Age = predict.ages(age.model, df.missing))

    # Return the modified dataset.
    df.passengers <- df.passengers %>%
        dplyr::rows_update(df.ages)

    # Return output dataset with imputed ages.
    return(df.passengers)
}

## ---- Cabin Imputer ----

#' Fit a classifier on the training model.
cabin.nb <- function(df.train) {
    # Prepare dataset with no missing cabins.
    df.train.cabins <- df.train %>%
        dplyr::filter(!is.na(Cabin))

    # Fit a model for predicting the Cabin.
    return(e1071::naiveBayes(Cabin ~ Pclass + Sex + SibSp + Parch + Fare + Ticket,
                             df.train.cabins, laplace = 1))
}

#' Predict set of missing cabins.
#' @param cabin.model Fitted model that will predict cabin based on set of predictors.
#' @param df.missing Dataset with missing cabins.
predict.cabins <- function(cabin.model, df.missing) {
    return(predict(cabin.model, newdata = df.missing))
}

#' Impute missing cabins into dataset.
#' @param cabin.model Fitted model that will predict cabin based on set of predictors.
#' @param df.passengers Dataset with potentially missing cabins.
impute.cabins <- function(cabin.model, df.passengers) {
    # Select entries missing a cabin value.
    df.missing <- df.passengers %>%
        dplyr::filter(is.na(Cabin))

    # Make predictions and assign it back to the cabin field.
    df.cabins <- df.missing %>%
        dplyr::mutate(Cabin = predict.cabins(cabin.model, df.missing))

    # Return the modified dataset.
    df.passengers <- df.passengers %>%
        dplyr::rows_update(df.cabins)

    # Return output dataset with imputed Cabins.
    return(df.passengers)
}

## ---- Embarked Imputer ----

#' Fit a classifier on the training model.
embarked.nb <- function(df.train) {
    # Prepare dataset with no missing embarkments.
    df.train.embarkments <- df.train %>%
        dplyr::filter(!is.na(Embarked))

    # Fit a model for predicting the Embarked
    return(e1071::naiveBayes(Embarked ~ Pclass + PassengerId + Fare + Ticket,
                             df.train.embarkments, laplace = 1))
}

#' Predict set of missing embarkments.
#' @param embarked.model Fitted model that will predict Embarked based on set of predictors.
#' @param df.missing Dataset with missing embarkments.
predict.embarkments <- function(embarked.model, df.missing) {
    return(predict(embarked.model, newdata = df.missing))
}

#' Impute missing embarkments into dataset.
#' @param embarked.model Fitted model that will predict Embarked based on set of predictors.
#' @param df.passengers Dataset with potentially missing embarkments.
impute.embarkments <- function(embarked.model, df.passengers) {
    # Select entries missing a embarked value.
    df.missing <- df.passengers %>%
        dplyr::filter(is.na(Embarked))

    # Make predictions and assign it back to the embarked field.
    df.embarkments <- df.missing %>%
        dplyr::mutate(Embarked = predict.embarkments(embarked.model, df.missing))

    # Return the modified dataset.
    df.passengers <- df.passengers %>%
        dplyr::rows_update(df.embarkments)

    # Return output dataset with imputed embarkments.
    return(df.passengers)
}

## ---- Create Imputer Models ----

#' Prepare models.
#' @param df.train Training subset for preparing imputers.
prepare.imputers <- function(df.train) {
    # Must use training subset and not testing subset
    # when fitting imputers in order to avoid data leakage.
    return(list(
        age = age.lm(df.train),
        cabin = cabin.nb(df.train),
        embarked = embarked.nb(df.train)
    ))
}


## ---- Preprocess Data ----

#' Preprocess input dataframe, agnostic to training or test subset.
#' @param df.passengers Train or test subset data to be imputed.
#' @param
preprocess.data <- function(df.passengers, imputers) {
    # Prepare raw dataframe for processing.
    df.raw <- df.passengers

    # Impute the ages.
    df.imputed.ages <- impute.ages(imputers$age, df.raw)

    # Impute the cabin.
    df.imputed.cabins <- impute.cabins(imputers$cabin, df.imputed.ages)

    # Impute the embarkments.
    df.imputed.embarkments <- impute.embarkments(imputers$embarked, df.imputed.cabins)

    # Update the final preprocessed frame.
    df.clean <- df.imputed.embarkments

    # Return the preprocessed dataframe.
    return(df.clean)
}

## ---- Data Pipeline ----

#' Fit training data.
prepare.data <- function() {
    # Load the raw training and testing dataset.
    df.raw <- load.data()
    # Train the imputer.
    df.imputers <- prepare.imputers(df.raw$train)
    # Impute missing data.
    df.clean <- list(
        train = preprocess.data(df.raw$train, df.imputers),
        test = preprocess.data(df.raw$test, df.imputers)
    )
    # Update training and test data.
    Titanic.train <<- Titanic.train %>% dplyr::rows_update(df.clean$train)
    Titanic.test <<- Titanic.test %>% dplyr::rows_update(df.clean$test)

    # Return prepared data.
    return(df.clean)
}

## ---- e1071::naiveBayes Model ----

#' Fit model on the dataset.
Titanic.nb <- function(df.train) {
    # Create the fitted model.
    return(e1071::naiveBayes(Survived ~ .,
                             df.train, laplace = 1))
}

## ---- randomForest::randomForest Model ----

#' Fit model on a RandomForest.
Titanic.rf <- function(df.train) {
    p = ncol(df.train) - 1 # Count of columns less 1.
    rf.model <- randomForest::randomForest(Survived ~ .,
                                           df.train,
                                           ntree = 2000,
                                           mtry = floor(sqrt(p)),
                                           importance = TRUE)
    return(rf.model)
}

## ---- neuralnet::neuralnet Model ----

#' Fit model on a neuralnet.
Titanic.nn <- function(df.train) {

    # Create the formula for the neural network.
    nn.features <- paste0(c(names(df.train %>% dplyr::select(-Survived))), collapse = " + ")
    nn.formula <- paste0(c("Survived ~ ", nn.features), collapse = "")
    nn.formula <- as.formula(nn.formula)

    # Standard scaler.
    normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
    }

    # Mutate formulas.
    df.matrix <- model.matrix(~ 0 + Pclass + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked, df.train)
    df.matrix %>%
        dplyr::mutate(
            Age = normalize(Age),
            SibSp = normalize(SibSp),
            Parch = normalize(Parch)
            )

    # Calculate the neuralnet and return it.
    return(neuralnet::neuralnet(nn.formula, data = df.matrix, hidden = 3, threshold = 0.001))

}

## ---- Confusion Matrix ----

#' Predict using model.
predict.survival.table <- function(df.preds, df.truth) {
    truth.labels <- ifelse(df.truth == 1, "Survived", "Perished")
    preds.labels <- ifelse(df.preds == 1, "Survived", "Perished")
    return(table(preds.labels, truth.labels))
}

## ---- Calculation of Training Metrics ----

#' Calculate a training error rate.
calc.train.metrics <- function(fit.model, df.train) {
    df.preds <- predict(fit.model, df.train)
    df.truth <- df.train$Survived
    df.table <- predict.survival.table(df.preds, df.truth)
    df.error <- (df.table[1,2] + df.table[2,1]) / sum(df.table)
    return(list(
        ERR = df.error,
        ACC = 1 - df.error
    ))
}

## ---- Generate Predictions (randomForest) ----

#' Make test predictions with RandomForest package.
predict.survival.rf <- function() {

    # Prepare cleaned data.
    df.cleaned <- prepare.data()

    # Fit random forest with training data.
    survival.rf <- Titanic.rf(df.cleaned$train)

    # Predict test data.
    rf.predictions <- predict(survival.rf, df.cleaned$test)

    # If NA, set to 0.
    df.predictions <- df.cleaned$test
    df.predictions %<>%
        dplyr::mutate(Survived = rf.predictions) %>%
        dplyr::mutate(Survived = tidyr::replace_na(Survived, 0)) %>%
        dplyr::select(c(PassengerId, Survived))

    df.predictions %>%
        readr::write_csv(file = here::here("data/rf_submission.v1.csv"))

    # Output the predictions.
    return(df.predictions)
}

## ---- Generate Predictions (neuralnet) ----

#' Make test predictions with NeuralNet package.
predict.survival.nn <- function() {

    # Prepare cleaned data.
    df.cleaned <- prepare.data()

    # Fit neural network with training data.
    survival.nn <- Titanic.nn(df.cleaned$train)

    # Compute the predictions.
    nn.predictions <- neuralnet::compute(survival.nn, df.cleaned$test)

    # If NA, set to 0.
    df.predictions <- df.cleaned$test
    df.predictions %<>%
        dplyr::mutate(Survived = nn.predictions) %>%
        dplyr::mutate(Survived = tidyr::replace_na(Survived, 0)) %>%
        dplyr::select(c(PassengerId, Survived))

    df.predictions %>%
        readr::write_csv(file = here::here("data/nn_submission.v1.csv"))

    # Output the predictions.
    return(df.predictions)

}

## ---- Feature Selection ----

#' Show correlation of features.
show.correlations <- function(df.passengers) {

    # Feature formula.
    # features <- as.formula(paste0(c("~ 0", get.colnames()[-2]), collapse = " + "))

    # Feature tibble.
    df.features <- df.passengers %>% dplyr::select(where(is.numeric))

    # Feature matrix.
    # feature.matrix <- model.matrix(features, df.features)

    # Get the correlation of features.
    feature.corr <- round(cor(df.features), 2)
    feature.p.mat <- ggcorrplot::cor_pmat(df.features)

    # Display plot.
    p <- corrplot::corrplot(feature.corr, method = 'pie', p.mat = feature.p.mat)
    print(p)
    return(p)
}

#' Show AOV of features against `Survived`.
show.aov <- function(df.passengers) {

    # List column names.
    colnames <- names(df.passengers)
    colnames <- colnames[-which(colnames == "Survived")]

    # Get possible predictive features. Excludes response and `Name`.
    aov.features <- paste0(colnames, collapse = " + ")

    # Prepare formula for features.
    aov.formula <- as.formula(paste0(c("as.numeric(Survived) ~ ", aov.features), collapse = ""))

    # ANOVA on possible predictors (excludes `Name`).
    aov.fit <- aov(aov.formula, data = df.passengers)

    # ANOVA results.
    print.noquote("ANOVA Results:")
    print(summary(aov.fit))
    return(aov.fit)

}

show.varImp <- function(df.passengers) {

    # List column names.
    colnames <- names(df.passengers)
    colnames <- colnames[-which(colnames == "Survived")]

    # Get possible predictive features. Excludes response and `Name`.
    model.features <- paste0(colnames, collapse = " + ")

    # Prepare formula for features.
    model.formula <- as.formula(paste0(c("Survived ~ ", model.features), collapse = ""))

    # Set up model control.
    model.control <- caret::trainControl(method="repeatedcv", number=10, repeats=3)

    # Set up hyper parameter tuning grid.
    mtry <- sqrt(ncol(df.passengers))
    model.grid <- expand.grid(.mtry = mtry)

    # Fit the model.
    metric <- "Accuracy"
    set.seed(123)
    model <- caret::train(model.formula, data=df.passengers, method="rf", metric="Accuracy", tuneGrid=model.grid, trControl=model.control)
    importance <- caret::varImp(model, scale=FALSE)

    print.noquote("Variable Importance:")
    print.noquote(importance)
    print(model)
    print(summary(model))
    return(model)
}

#' Reference: https://datascience.stackexchange.com/questions/893/how-to-get-correlation-between-two-categorical-variable-and-a-categorical-variab
show.cramer.v <- function(category_a, category_b) {

    feature.mat <- table(category_a, category_b)
    chi2 <- chisq.test(feature.mat, correct=F)
    chi2.results <- c(chi2$statistic, chi2$p.value)
    cramer.v <- sqrt(chi2$statistic / sum(feature.mat))

    print("ChiSquared Test Results")
    print(sprintf("Statistic = %.2f", chi2$statistic))
    print(sprintf("p.value = %.2f", chi2$p.value))
    print(sprintf("Cramér V / Phi = %.2f", cramer.v))

    return(list(
        chi2 = chi2$statistic,
        p.value = chi2$p.value,
        cramer.v = cramer.v
    ))

}

## ---- Feature Importance ----

