################################################################################
### Numeric Expectation for given prior
### Hongfei
################################################################################

#' Calculate numeric expectation for a given prior.
#'
#' @param f A function, E[f(X)] will be returned. default: f(x) = x
#' @param g A function for kernel of prior. default: g(y) = 1
#' @param lower lower bound for y in g(y) function
#' @param upper upper bound for y in g(y) function
#'
#' @return E^\pi[f(X)], \pi: g(x).
#'
#' @examples
#' neprior()
#'
#' @examples
#' neprior(f = function(x) {x^2}, g = function(y) {y}, lower = -10, upper = 3 )
#'
#' @examples
#' norm.kernel <- function(x) {exp(- (x - 2)^2 / (2 * 3^2))} # prior: N(2,3)
#' neprior(function(x) x^2, g = norm.kernel, lower = -Inf, upper = Inf) # E[X^2] = 9 + 2^2 = 13

neprior <- function(f = function(x){x}, g = function(x){1}, lower = -Inf, upper = Inf, ...){
    ## define multiplying function.
    "%*f%" <- function(x, y) {
        force(x)
        force(y)
        function(z) x(z) * y(z)
    }
    h <- f %*f% g
    num <- integrate(h, lower = lower, upper = upper, ...)
    denom <- integrate(g, lower = lower, upper = upper, ...)
    return(num$value / denom$value)
}
