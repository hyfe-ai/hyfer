#' Predict cough variance based on mean-variance regression model
#'
#' The format of the regression model used is: `variance = exp( b1 *log(mean) +  intercept )`.
#' You can make up the coefficients `b1` and `intercept` yourself, or generate them
#' using the function `fit_model_to_cough()`. The default values come from a large Hyfe study in northern Spain,
#' and should produce realistic variance values.
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
                                   b1 = 1.339539,
                                   intercept = 1.90719){
  var_return <- exp(b1*log(cough_rate) + intercept)
  return(var_return)
}
