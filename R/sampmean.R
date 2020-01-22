#' Sample mean
#'
#' Returns a vector of the sample mean
#'
#'
#' Accepts a data set and takes the mean of each variable and then outputs a
#' vector containing each of those means
#' @param x a data matrix
#'
#' @return
#' @export
#'
#' @examples
#' sampmean(data)
sampmean = function(x){
  output = vector(mode = "numeric", length = dim(x)[2])

  for(i in 1:dim(x)[2]){
    output[i] = sum(unlist(x[i])) / dim(x[i])[1]
  }
  return(output)
}
