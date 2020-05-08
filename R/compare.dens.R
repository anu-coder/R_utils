#' A density plot and compare Function
#'
#' This function allows you plot and compare any two graphs.
#' Takes the margin according as the max density
#' @param v1 , give the data frame, object, for first graph
#' @param v2, give the data frame, object, for second graph
#' @param legend, in format legend= c("v1","v2"): to show the legend of the graph
#' @param main, provide a heading, default: v1 v/s v2
#' @keywords density, legend, main,
#' @export
#' @examples pred=rnorm(1000,0,1) obs=rnorm(1000,0.2,2) compare.dens(pred,obs, legend=c("predicted","observed"))
#' compare.dens()


compare.dens <- function(v1, v2, legend=c("v1", "v2"),
                         main=NULL, ...) {

  # check for main
  if (is.null(main)){
    main <- paste(legend, collapse = " v/s ")
  }


  v1.dens <- density(v1) # v1 density
  v2.dens <- density(v2) # v2 density

  # Find the max peak of the two
  max.dens <- max(c(v1.dens$y, v2.dens$y))

  plot(v1.dens, col="red", xlab="" ,
       ylim = c(0, max.dens), main=main, ...)
  lines(v2.dens, col="blue")

  legend("topright",
         legend = legend,
         lty=1,
         col=c("red", "blue"))

}



