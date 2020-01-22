#' Sample correlation matrix
#'
#' Returns a correlation matrix for a data sample
#'
#'
#' Accepts a data set and creates a correlation matrix using the sampcov function and the definition of the
#' correlation matrix
#' @param x data set
#' @param varnames list of names
#'
#' @return
#' @export
#'
#' @examples
#' sampcov(data, c("WEIGHT", "LENGTH", "DENSITY"))
sampcor = function(x, varnames = NULL){
  nrows = dim(x)[1]
  ncols = dim(x)[2]
  output = matrix(nrow = ncols, ncol = ncols, dimnames = varnames)

  s = sampcov(x)

  for(i in 1:ncols){
    for(k in 1:ncols){
      output[i,k] = s[i,k] / (sqrt(s[i,i]*s[k,k]))
    }
  }
  return(output)
}
