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
#' \code{method} can either be a value, a vector of values, or a function.  Standard recycling
#'   is used if the vector of values is less than the number of \code{NA} values. When \code{method}
#'   is a function it takes only the non-\code{NA} values as input so that there is no need to
#'   supply those pesky \code{na.rm=TRUE} arguments. :)
#'
#' @param x A vector.
#' @param method Either a value, a vector of values, or a function to use for imputation.
#'   See Details for more information.
#' @param ... Additional arguments for \code{method} when it is a function.
#' @return The output of \code{impute} is the vector \code{x} with missing values imputed.
#' @export
#' @examples
#' x <- c(1, 1, 2, 3, NA, NA)
#'
#' #####
#' # Common use cases
#' #
#'
#' # I want to impute the mean!
#' x_mean <- impute(x, mean)
#' data.frame(x, x_mean)
#'
#' # I want to impute missing values as -1 (e.g. for inclusion in a regression analysis)
#' x_m1 <- impute(x, -1)
#' data.frame(x, x_m1)
#'
#' # I want to impute as "Missing"! (e.g. for inclusion in a regression analysis)
#' x_missing <- impute(x, "Missing")
#' data.frame(x, x_missing)
#'
#' #####
#' # impute_helper functions
#' #
#'
#' # I want to impute the most common category!
#' x_mode <- impute(x, impute_mode)
#' data.frame(x, x_mode)
#'
#' # I want to impute from the empirical cdf!
#' x_ecdf_1 <- impute(x, impute_ecdf)
#' x_ecdf_2 <- impute(x, impute_ecdf)
#' data.frame(x, x_ecdf_1, x_ecdf_2)
#'
#' # I want to impute using sampling with replacement!
#' x_sample_1 <- impute(x, impute_sample)
#' x_sample_2 <- impute(x, impute_sample)
#' data.frame(x, x_sample_1, x_sample_2)

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

    # Number of missing values
    n_NA <- sum(is.na(x))

    # Impute
    if (!is.function(method)) {
        x[is.na(x)] <- method
    } else if (grepl("^impute_", deparse(substitute(method)))) {
        x[is.na(x)] <- method(x_nonNA, n_NA, ...)
    } else {
        x[is.na(x)] <- method(x_nonNA, ...)
    }
    x
}
