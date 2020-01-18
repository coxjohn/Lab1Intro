#' Sample mean
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sampmean = function(x){
  output = vector(mode = "numeric", length = dim(x)[2])

  for(i in 1:dim(x)[2]){
    output[i] = mean(unlist(x[i]))
  }
  return(output)
}
