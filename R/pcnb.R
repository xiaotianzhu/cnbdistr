#'
#' @title
#' CDF of Conditional Negative Binomial
#'
#'
#' @description
#' Cumulative distribution function of the conditional distribution of X given X + Y = D,
#' where X ~ NB(r1, p1) and Y ~ NB(r2, p2) are drawn from two negative binomials,
#' independent of each other,
#' and assuming p1/p2 = theta.
#'
#' @details
#' Need to specify full list of arguments, as default values have not been set.
#'
#' @author Xiaotian Zhu, \email{xiaotian.zhu.psualum@@gmail.com}
#'
#' @param x a nonempty vector of real numbers.
#' @param D a positive integer.
#' @param r1 a positive value.
#' @param r2 a positive value.
#' @param theta a positive value.
#'
#' @return A vector providing values of Pr(X <= x | X + Y = D) for each element in x.
#'
#' @examples
#' pcnb(980, 2000, 120, 90, 0.994)
#' pcnb(0:7, 7, 2, 0.4, 0.6)
#'
#' @seealso
#' \code{\link{dcnb}, \link{qcnb}, \link{rcnb}.}
#'
#'
#' @export
"pcnb" <- function(x, D, r1, r2, theta){

  if (missing(x) || missing(D) || missing(r1) || missing(r2) || missing(theta))
    stop("Need to specify a full set of arguments: x, D, r1, r2, theta.")

  if (!is.numeric(D) || length(D)!=1 || !D%%1==0 || D<=0)
    stop("D needs to be a positive integer.")

  if (!is.numeric(x) || length(x)<1)
    stop("x needs to be a nonempty vector of real numbers.")

  if (!is.numeric(r1) || length(r1)!=1 || r1<=0)
    stop("r1 needs to be a positive value.")

  if (!is.numeric(r2) || length(r2)!=1 || r2<=0)
    stop("r2 needs to be a positive value.")

  if (!is.numeric(theta) || length(theta)!=1 || theta<=0)
    stop("theta needs to be a positive value.")


  return_value <- rep(0, length(x))

  x <- floor(x)

  if (theta <= 1){

    tmpbase <- -log(D + 1) -
      lgamma(r1) - lgamma(D + r2) + lgamma(D + 2) -
      log(Re(hypergeo::hypergeo(-D, r1, -D-r2+1, theta)))

    for (i in 1:length(x)){

      if (x[i] >= D){
        return_value[i] <- 1
      }else if (x[i] < 0){
        return_value[i] <- 0
      }else{
        lnvalue <- tmpbase + lgamma(r2 + D -x[i] -1) + lgamma(r1 + x[i] + 1) -
          lgamma(D -x[i]) - lgamma(x[i] + 2) +
          log(Re(hypergeo::genhypergeo(c(1, r1 + x[i] + 1, -D + x[i] + 1), c(x[i] + 2, -r2 - D + x[i] + 2), theta))) +
          (x[i] + 1) * log(theta)

        return_value[i] <- 1 - exp(lnvalue)
      }

    }

  }else{

    return_value <- 1 - pcnb(D-x-1, D, r2, r1, 1/theta)

  }


  return_value

}
