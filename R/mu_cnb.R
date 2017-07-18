#'
#' @title
#' Mean of Conditional Negative Binomial
#'
#'
#' @description
#' Function calculating mean of the conditional distribution of X given X + Y = D,
#' where X ~ NB(r1, p1) and Y ~ NB(r2, p2) are drawn from two negative binomials,
#' independent of each other,
#' and assuming p1/p2 = theta.
#'
#' @details
#' Need to specify full list of arguments, as default values have not been set.
#'
#' @author Xiaotian Zhu, \email{xiaotian.zhu.psualum@@gmail.com}
#'
#' @param D a positive integer.
#' @param r1 a positive value.
#' @param r2 a positive value.
#' @param theta a positive value.
#'
#' @return E(X | X + Y = D).
#'
#' @examples
#' mu_cnb(7, 2, 0.4, 0.6)
#'
#' @seealso
#' \code{\link{sigma2_cnb}}
#'
#'
#' @export
"mu_cnb" <- function(D, r1, r2, theta){

  if (missing(D) || missing(r1) || missing(r2) || missing(theta))
    stop("Need to specify a full set of arguments: D, r1, r2, theta.")

  if (!is.numeric(D) || length(D)!=1 || !D%%1==0 || D<=0)
    stop("D needs to be a positive integer.")

  if (!is.numeric(r1) || length(r1)!=1 || r1<=0)
    stop("r1 needs to be a positive value.")

  if (!is.numeric(r2) || length(r2)!=1 || r2<=0)
    stop("r2 needs to be a positive value.")

  if (!is.numeric(theta) || length(theta)!=1 || theta<=0)
    stop("theta needs to be a positive value.")


  return_value <- 0

  if (theta <= 1) {

    lnvalue <- log(Re(hypergeo::hypergeo(-D+1, r1+1, -D-r2+2, theta))) - log(Re(hypergeo::hypergeo(-D, r1, -D-r2+1, theta)))

    return_value <- D * theta * r1 / (r2 + D - 1) * exp(lnvalue)

  }else{

    return_value <- D - mu_cnb(D, r2, r1, 1/theta)

  }


  return_value

}

