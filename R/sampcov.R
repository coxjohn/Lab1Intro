#' Sample Covariance (biased)
#'
#' Returns a (biased) covariance matrix for a data matrix
#'
#'
#' Accepts a data set and creates a $p\times p$ covariance matrix, where p is the number of variables.
#' The function also accepts variable names that will be used to lable the columns of the matrix.
#' These results are biased, as they use $\frac{1}{n}$ in the definition of the variance, rather than $\frac{1}{n-1}$.
#' @param x a data set
#' @param varnames a list of variable names
#'
#' @return
#' @export
#'
#' @examples
#' sampcov(data, c("WEIGHT", "LENGTH", "DENSITY"))
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
