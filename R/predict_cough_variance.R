#' Predict cough variance based on mean-variance regression model
#'
#' Format of the regression model: (format: variance = exp( b1 *log(mean) +  intercept )).
#' You can generate these coefficients using the function `fit_model_to_cough()`.
#'
#' @param cough_rate The mean cough rate (coughs per hour) for which you want to
#' predict the variance.
#' @param b1 The first coefficient of the model.
#' @param intercept The second coefficient of the model.
#'
#' @return The predicted variance.
#' @export
#'
predict_cough_variance <- function(cough_rate,
                                   b1,
                                   intercept){
  var_return <- exp(b1*log(cough_rate) + intercept)
  return(var_return)
}
