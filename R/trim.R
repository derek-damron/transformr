#' Trim a numeric vector
#'
#' \code{trim} returns a trimmed numeric vector.
#'
#' \code{trim} returns a trimmed version of the numeric vector \code{x}.  \code{NA} values in
#'   \code{x} are ignored during the trimming process but preserved in the output.
#'
#' \code{trim} is designed to be readable from the function call.  For example,
#'   \code{trim(x, lo=-1, hi=1)} can be read as "\strong{Trim} \strong{x} at \strong{-1}
#'   and \strong{1}".
#'
#' The arguments \code{lo} and \code{hi} are used based on \code{type}.
#'   \code{trim} offers several different options for \code{type}:
#'
#' \itemize{
#'   \item \strong{smart}: The default option that derives \code{type}
#'     based on the values of \code{x}, \code{lo}, and \code{hi}.  Currently, smart will
#'     use percentiles if \code{lo} and \code{hi} are between 0 and 1 and use values otherwise.
#'   \item \strong{value}: \code{lo} and \code{hi} are used as raw values
#'     (e.g. .05 is the value .05).
#'   \item \strong{percentile}: \code{lo} and \code{hi} are used as percentiles
#'     (e.g. .05 is 5th percentile).
#' }
#'
#' @param x A numeric vector.
#' @param lo The lower value for trimming.
#' @param hi The upper value for trimming.
#' @param type A character string indicating the desired type of trimming with \code{"smart"}
#'   being the default.  This must be (an abbreviation of) one of the strings \code{"smart"},
#'   \code{"value"}, or \code{"percentile"}.  See Details for more information.
#' @return The output of \code{trim} is a trimmed numeric vector with the same length as \code{x}.
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' # Trim at -1 and 1
#' # Smart
#' x_val <- trim(x, lo=-1, hi=1)
#' summary(x_val)
#' # Explicit
#' x_val <- trim(x, lo=-1, hi=1, type='v')
#' summary(x_val)
#'
#' # Trim at 5th and 95th percentiles
#' # Smart
#' x_per <- trim(x, lo=.05, hi=.95)
#' summary(x_per)
#' x_per <- trim(x, lo=.05, hi=.95, type='p')
#' summary(x_per)
#'
#' # One-sided trims
#' # Trim lower at -1
#' x_lower_trim <- trim(x, lo=-1)
#' summary(x_lower_trim)
#' # Trim upper at 95th percentile
#' x_upper_trim <- trim(x, hi=.95)
#' summary(x_upper_trim)

trim <- function(x, lo, hi, type=c('smart','value','percentile')) {
  # Check x
  if (missing(x)) {
    stop('Please provide a vector x', call.=FALSE)
  } else if (!is.integer(x) & !is.numeric(x)) {
    stop('x must be integer or numeric', call.=FALSE)
  }

  # Check lo/hi
  if (missing(lo)) {
    lo <- '_missing_'
  } else if (!is.integer(lo) & !is.numeric(lo)) {
    stop('lo must be integer or numeric if specified')
  }
  if (missing(hi)) {
    hi <- '_missing_'
  } else if (!is.integer(hi) & !is.numeric(hi)) {
    stop('hi must be integer or numeric if specified')
  }

  # Check type
  type <- match.arg(type)

  # Derive smart type if applicable
  if (type == 'smart') {
    type <- derive_smart_trim_type(lo, hi)
  }

  # Trim
  f <- paste('trim', type, sep="_")
  do.call(f, list(x=x, lo=lo, hi=hi))
}

trim_value <- function(x, lo, hi) {
  # Check lo/hi
  if (specified(lo) & specified(hi) & hi < lo) {
    stop('Lower argument lo must be less than or equal to the upper argument hi', call.=FALSE)
  }

  # Trim
  if (specified(lo)) {
    x[x <= lo] <- lo
  }
  if (specified(hi)) {
    x[x >= hi] <- hi
  }
  return(x)
}

trim_percentile <- function(x, lo, hi) {
  # Check lo/hi
  if (specified(lo) & (lo < 0 | lo > 1)) {
    stop('Lower argument lo must be in the range 0 <= lo <= 1', call.=FALSE)
  } else if (specified(hi) & (hi < 0 | hi > 1)) {
    stop('Upper argument hi must be in the range 0 <= hi <= 1', call.=FALSE)
  } else if (specified(lo) & specified(hi) & hi < lo) {
    stop('Lower argument lo must be less than or equal to the upper argument hi', call.=FALSE)
  }

  # Trim
  if (specified(lo)) {
    lo <- quantile(x, prob=lo, type=8, na.rm=TRUE)
    x[x <= lo] <- lo
  }
  if (specified(hi)) {
    hi <- quantile(x, prob=hi, type=8, na.rm=TRUE)
    x[x >= hi] <- hi
  }
  return(x)
}

specified <- function(arg) {
  if (arg == '_missing_') {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

derive_smart_trim_type <- function(lo, hi) {
  if (!specified(lo) & !specified(hi)) {
    stop('Please provide at least one lo or hi value')
  } else if (between_0_and_1_or_missing(lo) & between_0_and_1_or_missing(hi)) {
    return('percentile')
  } else {
    return('value')
  }
}

between_0_and_1_or_missing <- function(arg) {
  if ((arg >= 0 & arg <= 1) | !specified(arg)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
