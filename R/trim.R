#' Trim a numeric vector
#'
#' \code{trim} returns a trimmed numeric vector.
#'
#' \code{trim} returns a trimmed version of the numeric vector \code{x}.
#'   \code{NA} values in \code{x} are ignored during the trimming process
#'   but are preserved in the output.  \code{trim} will do one-sided trimming
#'   if only one \code{lo} or \code{hi} argument is provided
#'   (e.g. \code{trim(x, lo=-1)} will trim \code{x} at a lower value of -1).
#'
#' \code{trim} is designed to be readable from the function call.  For example:
#' \itemize{
#'   \item \code{trim(x, "value", lo=-1, hi=1)} can be read as
#'     "\strong{Trim} \strong{x} at \strong{-1} and \strong{1}".
#'   \item \code{trim(x, "percentile", lo=.05, hi=.95)} can be read as
#'     "\strong{Trim} \strong{x} at the \strong{5}th and \strong{95}th percentiles".
#' }
#'
#' The arguments \code{lo} and \code{hi} are used based on \code{method}.
#'   \code{trim} offers several different options for \code{method}:
#'
#' \itemize{
#'   \item \strong{value}: \code{lo} and \code{hi} are used as raw values
#'     (e.g. .05 is the value .05).
#'   \item \strong{percentile}: \code{lo} and \code{hi} are used as percentiles
#'     (e.g. .05 is 5th percentile).
#' }
#'
#' @param x A numeric vector.
#' @param method A character string indicating the desired method of trimming
#'   with \code{"value"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"value"} or \code{"percentile"}.
#'   See Details for more information.
#' @param lo The lower value/percentile for trimming.  See Details for more information.
#' @param hi The upper value/percentile for trimming.  See Details for more information.
#' @param replace Either \code{NULL} ("rounds" values according to the lo and hi arguments),
#'   \code{NA},
#'   or a single value that will replace the trimmed values.  The default is NULL.
#' @return The output of \code{trim} is a trimmed numeric vector with the same
#'   length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' #####
#' # Common use cases
#' #
#'
#' # I want to trim at the values -1 and 1!
#' x_val <- trim(x, lo=-1, hi=1)
#' summary(x_val)
#'
#' # I want to trim at 5th and 95th percentiles!
#' x_per <- trim(x, "percentile", lo=.05, hi=.95)
#' summary(x_per)
#'
#' # I only want to trim values above the 95th percentile!
#' x_hi <- trim(x, "percentile", hi=.95)
#' summary(x_hi)
#'
#' # I want the trimmed values converted to NAs!
#' x_NA <- trim(x, "percentile", lo=.05, hi=.95, replace=NA)
#' summary(x_NA)
#'
#' # I want to trim negative values to -1! (Weird but okay!)
#' x_neg <- trim(x, lo=0, replace=-1)
#' summary(x_neg)

trim <- function(x, method=c("value", "percentile"), lo=NULL, hi=NULL, replace=NULL) {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to trim", call.=FALSE)
    } else if (!is.numeric(x)) {
        stop("x must be a numeric vector", call.=FALSE)
    }

    # Check method
    method <- match.arg(method)

    # Check that at least one lo/hi value provided
    if (is.null(lo) && is.null(hi)) {
        stop("Please provide at least one lo or hi value")
    }

    # Check lo
    if (!is.null(lo)) {
        if (!is.numeric(lo)) {
            stop("lo must be a numeric value if specified", call.=FALSE)
        } else if (length(lo) != 1) {
            stop("lo must be a single value if specified", call.=FALSE)
        }
    }

    # Check hi
    if (!is.null(hi)) {
        if (!is.numeric(hi)) {
            stop("hi must be a numeric value if specified", call.=FALSE)
        } else if (length(hi) != 1) {
            stop("hi must be a single value if specified", call.=FALSE)
        }
    }

    # Check lo <= hi
    if (!is.null(lo) && !is.null(hi) && hi < lo) {
        stop("lo must be less than or equal to the hi", call.=FALSE)
    }

    # Check replace
    if (!is.null(replace)) {
        if (!is.numeric(replace) && !is.na(replace)) {
            stop("replace must be a numeric value or NA if specified", call.=FALSE)
        } else if (length(replace) != 1) {
            stop("replace must be a single value if specified", call.=FALSE)
        }
    }

    # Check lo/hi in [0,1] if method="percentile"
    if (method=="percentile") {
        if (!is.null(lo) && (lo < 0 | lo > 1)) {
            stop("lo must be in the range 0 <= lo <= 1 for method='percentile'", call.=FALSE)
        }
        if (!is.null(hi) && (hi < 0 | hi > 1)) {
            stop("hi must be in the range 0 <= hi <= 1 for method='percentile'", call.=FALSE)
        }
    }

    # Derive percentiles if method="percentile"
    if (method=="percentile") {
        if (!is.null(lo)) {
            lo <- quantile(x, prob=lo, type=8, na.rm=TRUE)
        }
        if (!is.null(hi)) {
            hi <- quantile(x, prob=hi, type=8, na.rm=TRUE)
        }
    }

    # Trim
    if (!is.null(lo)) {
        x[x < lo] <- ifelse(!is.null(replace), replace, lo)
    }
    if (!is.null(hi)) {
        x[x > hi] <- ifelse(!is.null(replace), replace, hi)
    }
    x
}
