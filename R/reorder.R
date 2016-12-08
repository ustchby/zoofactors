#' Reorder the levels of factors based on the value of a second variable.
#'
#' @param x factor
#' @param y a vector of the same length as x
#' @param FUN a function whose first argument is a vector and returns a scalar
#' @param ... optional: extra arguments supplied to FUN
#' @param order logical, whether return value will be an ordered factor rather than a factor
#'
#' @return factor
#' @export
#'
#' @examples
#' reorder(InsectSprays$spray, InsectSprays$count)
reorder <- function(x, y, FUN= mean, ..., order = is.ordered(y)){
  if(is.factor(x)==FALSE){
    stop("y should be a factor")
  }
  if(length(x)!=length(y)){
    stop("x and y should be of the same length")
  }
  if(is.function(FUN)==FALSE){
    stop("FUN should be a function")
  }
  if(is.logical(order)==FALSE){
    stop("This argument should be logical")
  }
  else{
    scores <- tapply(X=y, INDEX = x, FUN = FUN, ...)
    new_factors <- c()

    tmp <- data.frame(names = levels(x), scores = scores)
    tmp <- dplyr::arrange(tmp, dplyr::desc(tmp$scores))
    z=c()
    for(i in 1:length(tmp[,2])){
      z=c(z,as.character(tmp[i,1]))
    }
    new_factors=factor(x,levels=z)
    attr(new_factors, "scores") <- scores
    return(new_factors)
  }
}
