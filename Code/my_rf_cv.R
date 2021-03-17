#' Random Forest Cross Validation
#'
#' Performs random forest cross validation on the "penguins" data set,
#' predicting "body_mass_g" with covariates "bill_length_mm", "bill_depth_mm",
#' and "flipper_length_mm", with 100 trees.
#'
#' @param k Numeric denoting number of folds to use in the cross validation.
#' @keywords prediction
#'
#' @return Numeric that is the mean squared error of predicted values compared
#'   to true body mass.
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(30)
#'
#' @export
my_rf_cv <- function(k) {
  if (!is.numeric(k)){
    stop("Number of folds must be numeric")
  } else if (k <= 0) {
    stop("Number of folds must be positive")
  }

  fold <- sample(rep(1:k, length = nrow(my_penguins)))
  train_fold <- data.frame(my_penguins) %>% dplyr::mutate("split" = fold)

  sse <- 0
  for (i in 1:k) {
    data_train <- train_fold %>% dplyr::filter(split != i)
    data_test <- train_fold %>% dplyr::filter(split == i)

    model <- randomForest::randomForest(
      body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = data_train, ntree = 100)
    predictions <- stats::predict(model, data_test[, -6])

    sse <- sse +
      sum(((train_fold %>% dplyr::filter(split == i))[6] - predictions)^2)
  }

  return(sse / nrow(my_penguins))
}
