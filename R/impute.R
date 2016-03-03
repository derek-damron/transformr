#' Impute missing values
#'
#' \code{impute} returns a vector with missing values imputed.
#'
#' \code{impute} returns the vector \code{x} with missing values imputed via \code{method}.
#'
#' \code{impute} is designed to be readable from the function call.
#'   For example:
#' \itemize{
#'   \item \code{impute(x, -1)} can be read as
#'     "\strong{impute} missing values for \strong{x} as \strong{-1}".
#'   \item \code{impute(x, mean)} can be read as
#'     "\strong{impute} missing values for \strong{x} as \strong{the mean}".
#'   \item \code{impute(x, sample, replace=TRUE)} can be read as
#'     "\strong{impute} missing values for \strong{x} using \strong{random sampling with replacement}".
#' }
#'
#' When \code{method} is a function it takes the non-\code{NA} values as input, so there is no
#'   need to supply those pesky \code{na.rm=TRUE} arguments. :)
#'
#' @param x A vector.
#' @param method Either a value, a vector of values, or a function to use for imputation.
#'   See Details for more information.
#' @param ... Additional arguments for method when it is a function.
#' @return The output of \code{impute} is the vector \code{x} vector with missing values imputed.
#' @export
#' @examples
#' x <- c(NA, 1, 1, 2, 3, NA)
#'
#' # Impute -1
#' impute(x, -1)
#'
#' # Impute mean
#' impute(x, mean)

impute <- function(x, method, ...) {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to impute", call.=FALSE)
    } else if (all(!is.na(x))) {
        stop("x contains no missing values to impute", call.=FALSE)
    }

    # Check method
    if (missing(method)) {
        stop("Please provide a method for imputation", call.=FALSE)
    }

    # Isolate non-missing values
    x_nonNA <- x[!is.na(x)]

    # Impute
    if (!is.function(method)) {
        x[is.na(x)] <- method
    } else {
        x[is.na(x)] <- method(x_nonNA, ...)
    }
    x
}
