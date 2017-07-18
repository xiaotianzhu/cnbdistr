#'
#' @title
#' Random Number Generation from Conditional Negative Binomial
#'
#'
#' @description
#' Random number generation from the conditional distribution of X given X + Y = D,
#' where X ~ NB(r1, p1) and Y ~ NB(r2, p2) are drawn from two negative binomials,
#' independent of each other,
#' and assuming p1/p2 = theta.
#'
#' @details
#' Need to specify full list of arguments, as default values have not been set.
#'
#' @author Xiaotian Zhu, \email{xiaotian.zhu.psualum@@gmail.com}
#'
#' @param n a positive integer.
#' @param D a positive integer.
#' @param r1 a positive value.
#' @param r2 a positive value.
#' @param theta a positive value.
#'
#' @return n iid draws from X|X+Y=D.
#'
#' @examples
#' x <- rcnb(1e3, 7, 2, 0.4, 0.6)
#' hist(x)
#'
#' @seealso
#' \code{\link{dcnb}, \link{pcnb}, \link{qcnb}.}
#'
#'
#' @export
"rcnb" <- function(n, D, r1, r2, theta){

  if (missing(n) || missing(D) || missing(r1) || missing(r2) || missing(theta))
    stop("Need to specify a full set of arguments: n, D, r1, r2, theta.")

  if (!is.numeric(D) || length(D)!=1 || !D%%1==0 || D<=0)
    stop("D needs to be a positive integer.")

  if (!is.numeric(n) || length(n)!=1 || !n%%1==0 || n<=0)
    stop("n needs to be a positive integer.")

  if (!is.numeric(r1) || length(r1)!=1 || r1<=0)
    stop("r1 needs to be a positive value.")

  if (!is.numeric(r2) || length(r2)!=1 || r2<=0)
    stop("r2 needs to be a positive value.")

  if (!is.numeric(theta) || length(theta)!=1 || theta<=0)
    stop("theta needs to be a positive value.")


  return_value <- rep(0, n)

  for (i in 1:n) {

    u <- stats::runif(1)

    return_value[i] <- qcnb(u, D, r1, r2, theta)

  }


  return_value

}
