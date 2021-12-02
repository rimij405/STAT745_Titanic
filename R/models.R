## ---- models.R ----
# Functions for feature engineering.

# Functions for creating and fitting models.

## Utilities ----
source(here::here("R/utilities.R"))

## Exports ----

#' Environment for feature extractors.
Models <<- new.env(parent = emptyenv())

### Model Creation ----

### Model Validation ----

### Model Fitting ----

#' Fit all models.
Models$fit <- function(X.train, y.train) {

    Util$task("Fit: Logistic Regression Classifier",
              task = Util$stub(
                  "Performing feature selection...",
                  "Fitting model...",
                  "Measuring training performance...",
                  "Measuring validation performance..."
              ))

    Util$task("Fit: Random Forest Classifier",
              task = Util$stub(
                  "Performing feature selection...",
                  "Fitting model...",
                  "Measuring training performance...",
                  "Measuring validation performance..."
              ))

    Util$task("Fit: Latent Discriminat Analysis Classifier",
              task = Util$stub(
                  "Performing feature selection...",
                  "Fitting model...",
                  "Measuring training performance...",
                  "Measuring validation performance..."
              ))

    Util$task("Fit: Quadractic Discriminat Analysis Classifier",
              task = Util$stub(
                  "Performing feature selection...",
                  "Fitting model...",
                  "Measuring training performance...",
                  "Measuring validation performance..."
              ))

}

### Model Prediction ----

#' Make predictions with all models.
Models$predict <- function(X.test) {

    Util$task("Predict: Logistic Regression Classifier",
              task = Util$stub(
                  "Predicting with model...",
                  "Exporting predictions..."
              ))

    Util$task("Predict: Random Forest Classifier",
              task = Util$stub(
                  "Predicting with model...",
                  "Exporting predictions..."
              ))

    Util$task("Predict: Latent Discriminat Analysis Classifier",
              task = Util$stub(
                  "Predicting with model...",
                  "Exporting predictions..."
              ))

    Util$task("Predict: Quadractic Discriminat Analysis Classifier",
              task = Util$stub(
                  "Predicting with model...",
                  "Exporting predictions..."
              ))

}
