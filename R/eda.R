# eda.R
# -------
# Functions for analysis of data.

# Loading functions.
source(here::here("R/loader.R"))

# Load in the Titanic.train and Titanic.test subsets.
load.data()

## ---- Descriptive Statistics ----

describe <- function(df.passengers) {
    dplyr::glimpse(df.passengers)
    summary(df.passengers)
}

describe.miss_summary <- function(df.passengers) {
    naniar::miss_case_table(df.passengers)
}

describe.miss_age <- function(df.passengers) {
    df.passengers %>%
        dplyr::group_by(Pclass) %>%
        naniar::miss_var_summary() %>%
        dplyr::filter(variable == "Age")
}

describe.miss_embarked <- function(df.passengers) {
    df.passengers %>%
        dplyr::group_by(Pclass) %>%
        naniar::miss_var_summary() %>%
        dplyr::filter(variable == "Embarked")
}


## ---- Graphical Statistics ----

visualize.data <- function(df.passengers) {
    visdat::vis_dat(df.passengers)
}

visualize.nullvalues <- function(df.passengers) {
    visdat::vis_miss(df.passengers)
}

visualize.age_vs_class <- function(df.passengers) {
    ggplot2::ggplot(df.passengers, ggplot2::aes(x = Age, y = Pclass)) +
        naniar::geom_miss_point()
}

visualize.age_vs_fare <- function(df.passengers) {
    ggplot2::ggplot(df.passengers, ggplot2::aes(x = Age, y = Fare)) +
        naniar::geom_miss_point()
}

visualize.age_vs_sibsp <- function(df.passengers) {
    ggplot2::ggplot(df.passengers, ggplot2::aes(x = Age, y = SibSp)) +
        naniar::geom_miss_point()
}

visualize.age_vs_parch <- function(df.passengers) {
    ggplot2::ggplot(df.passengers, ggplot2::aes(x = Age, y = Parch)) +
        naniar::geom_miss_point()
}

visualize.age_imputed <- function(df.passengers) {
    df.passengers %>%
        naniar::nabular() %>%
        simputation::impute_lm(Age ~ Pclass, add_residual="normal") %>%
        simputation::impute_median(Age ~ Pclass) %>%
        ggplot2::ggplot(ggplot2::aes(x = Age, y = Pclass, colour = Age_NA)) +
        ggplot2::geom_point(position='jitter')
}
