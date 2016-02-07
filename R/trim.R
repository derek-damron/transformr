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
#' The arguments \code{lo} and \code{hi} are used based on \code{type}.
#'   \code{trim} offers several different options for \code{type}:
#'
#' \itemize{
#'   \item \strong{smart}: The default option that derives \code{type}
#'     based on the arguments \code{lo} and \code{hi}, which removes the
#'     need to specify \code{type} when the use case for \code{trim} is
#'     "obvious".  Currently, smart will use percentiles if \code{lo}
#'     and \code{hi} are both in the range [0,1] and use values otherwise.
#'   \item \strong{value}: \code{lo} and \code{hi} are used as raw values
#'     (e.g. .05 is the value .05).
#'   \item \strong{percentile}: \code{lo} and \code{hi} are used as percentiles
#'     (e.g. .05 is 5th percentile).
#' }
#'
#' @param x A numeric vector.
#' @param type A character string indicating the desired type of trimming
#'   with \code{"smart"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"smart"}, \code{"value"}, or \code{"percentile"}.
#'   See Details for more information.
#' @param lo The lower value/percentile for trimming.
#' @param hi The upper value/percentile for trimming.
#' @param replace The value that will replace the trimmed values.  The default is
#'   missing, which will "round" values according to the lo and hi arguments.  If
#'   specified, can be either numeric/integer or NA.
#' @return The output of \code{trim} is a trimmed numeric vector with the same
#'   length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' # Trim at -1 and 1
#' # "Smart" type default
#' x_val <- trim(x, lo=-1, hi=1)
#' summary(x_val)
#' # Explicit type
#' x_val <- trim(x, "value", lo=-1, hi=1)
#' summary(x_val)
#'
#' # Trim at 5th and 95th percentiles
#' # "Smart" type default
#' x_per <- trim(x, lo=.05, hi=.95)
#' summary(x_per)
#' # Explicit type
#' x_per <- trim(x, "percentile", lo=.05, hi=.95)
#' summary(x_per)
#'
#' # Trim at the values .05 and .95 - Must specify type as "value"
#' x_val <- trim(x, "value", lo=.05, hi=.95)
#' summary(x_val)
#'
#' # One-sided trims
#' # Trim lower at -1
#' x_lower_trim <- trim(x, lo=-1)
#' summary(x_lower_trim)
#' # Trim upper at 95th percentile
#' x_upper_trim <- trim(x, hi=.95)
#' summary(x_upper_trim)
#'
#' # Trimming to NA (useful if extreme values are likely caused
#' # by data collection errors)
#' x_trim_NA <- trim(x, lo=-3, hi=3, replace=NA)
#' summary(x_trim_NA)
#'
#' # Trimming negative values to -1 (for funsies)
#' x_trim_minus1 <- trim(x, "value", lo=0, replace=-1)
#' summary(x_trim_minus1)

trim <- function(x, type=c("smart", "value", "percentile"), lo, hi, replace) {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to trim", call.=FALSE)
    } else if (!is.numeric(x)) {
        stop("x must be a numeric vector", call.=FALSE)
    }

    # Check lo
    if (missing(lo)) {
        lo <- missing_arg()
    } else if (!is.numeric(lo)) {
        stop("lo must be a numeric value if specified", call.=FALSE)
    } else if (length(lo) != 1) {
        stop("lo must be a single value if specified", call.=FALSE)
    }

    # Check hi
    if (missing(hi)) {
        hi <- missing_arg()
    } else if (!is.numeric(hi)) {
        stop("hi must be a numeric value if specified", call.=FALSE)
    } else if (length(hi) != 1) {
        stop("hi must be a single value if specified", call.=FALSE)
    }

    # Check lo <= hi
    if (specified(lo) && specified(hi) && hi < lo) {
        stop("lo must be less than or equal to the hi", call.=FALSE)
    }

    # Check replace
    if (missing(replace)) {
        replace <- missing_arg()
    } else if (!is.numeric(replace) && !is.na(replace)) {
        stop("replace must be a numeric value or NA if specified", call.=FALSE)
    } else if (length(replace) != 1) {
        stop("replace must be a single value if specified", call.=FALSE)
    }

    # Check type
    type <- match.arg(type)
    # Derive smart type if applicable
    if (type == "smart") {
        type <- derive_smart_trim_type(lo, hi)
    }

    # Trim
    f <- paste("trim", type, sep=".")
    do.call(f, list(x=x, lo=lo, hi=hi, replace=replace))
}

trim.value <- function(x, lo, hi, replace) {
    # Trim
    if (specified(lo)) {
        x[x < lo] <- ifelse(specified(replace), replace, lo)
    }
    if (specified(hi)) {
        x[x > hi] <- ifelse(specified(replace), replace, hi)
    }
    x
}

trim.percentile <- function(x, lo, hi, replace) {
    # Check lo/hi
    if (specified(lo) && (lo < 0 | lo > 1)) {
        stop("lo must be in the range 0 <= lo <= 1 for type='percentile'", call.=FALSE)
    } else if (specified(hi) && (hi < 0 | hi > 1)) {
        stop("hi must be in the range 0 <= hi <= 1 for type='percentile'", call.=FALSE)
    }

    # Trim
    if (specified(lo)) {
        lo <- quantile(x, prob=lo, type=8, na.rm=TRUE)
        x[x < lo] <- ifelse(specified(replace), replace, lo)
    }
    if (specified(hi)) {
        hi <- quantile(x, prob=hi, type=8, na.rm=TRUE)
        x[x > hi] <- ifelse(specified(replace), replace, hi)
    }
    x
}

derive_smart_trim_type <- function(lo, hi) {
    if (!specified(lo) && !specified(hi)) {
        stop("Please provide at least one lo or hi value")
    } else if (between_0_and_1_or_missing(lo) && between_0_and_1_or_missing(hi)) {
        "percentile"
    } else {
        "value"
    }
}

between_0_and_1_or_missing <- function(arg) {
    if (!specified(arg) || (arg >= 0 & arg <= 1)) {
        TRUE
    } else {
        FALSE
    }
}
