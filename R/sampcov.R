#' Sample Covariance (biased)
#'
#' @param x
#' @param varnames
#'
#' @return
#' @export
#'
#' @examples
sampcov = function(x, varnames = NULL){
  nrows = dim(x)[1]
  ncols = dim(x)[2]
  output = matrix(nrow = ncols, ncol = ncols, dimnames = varnames)

  for(i in 1:ncols){
    for(k in 1:ncols){
      output[i,k]=0
      for(j in 1:nrows){
        output[i, k] = output[i,k] + (data[j,i] - mean(unlist(data[i])))*(data[j,k] - mean(unlist(data[k])))
      }
      output[i,k] = (1/(nrows))*output[i,k]
    }
  }
  return(output)
}
