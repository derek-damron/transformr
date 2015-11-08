#' Trim a numeric vector
#'
#' \code{trim} returns a trimmed numeric vector.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#' 
#' \code{trim} test2.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' 
#'
#' \dontrun{
#' sum("a")
#' }

trim <- function(x, lo, hi, type=c('smart','value','percentile')) {
  # Argument check
  if (missing(x)) {
    stop('Please provide a vector x')
  } else if (missing(lo)) {
    stop('Please provide a lower argument lo')
  } else if (missing(hi)) {
    stop('Please provide a upper argument hi')
  }
  # Type check
  if (missing(type)) {
    type <- 'smart'
  } else {
    type <- match.arg(type)    
  }
  # If applicable, derive smart type
  if (type == 'smart') {
    if (lo >= 0 & lo <= 1 & hi >= 0 & hi <= 1) {
      type <- 'percentile'
    } else {
      type <- 'value'
    }
    message(paste('Smart type -', type))
  }
  # Trim
  do.call(paste('trim', type, sep="_"),
          list(x=x, lo=lo, hi=hi))
}

trim_percentile <- function(x, lo, hi) {
  # Argument check
  if (lo < 0 | lo > 1) {
    stop('Lower argument lo must be in the range [0,1]')
  } else if (hi < 0 | hi > 1) {
    stop('Upper argument hi must be in the range [0,1]')
  } else if (hi < lo) {
    stop('Lower argument lo must be smaller than upper argument hi')
  }
  lo <- quantile(x, prob=lo, type=8)
  hi <- quantile(x, prob=hi, type=8)
  x[x <= lo] <- lo
  x[x >= hi] <- hi
  return(x)
}

trim_value <- function(x, lo, hi) {
  if (hi < lo) {
    stop('Lower argument lo must be smaller than upper argument hi')
  }
  x[x <= lo] <- lo
  x[x >= hi] <- hi
  return(x)
}
