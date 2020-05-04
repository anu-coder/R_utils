


#' A Predicted plot Function
#'
#' This function allows you plot the predicted and observed data
#' @param predicts , give the prediced data frame, object
#' @param observed, give the actual observed test values (precisely)
#' @keywords density
#' @export
#' @examples
#' predictplot()

predictplot=function(predicts,observed){
  plot(density(predicts),col="red",
       main="Predicted Vs observed", xlab="", )
  lines(density(observed), col="blue")

}
