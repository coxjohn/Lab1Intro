#' Sample correlation coefficient
#'
#' @param x
#' @param varnames
#'
#' @return
#' @export
#'
#' @examples
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
