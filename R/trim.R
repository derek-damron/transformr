#' Trim a numeric vector
#'
#' \code{trim} returns a trimmed numeric vector. 
#'
#' \code{trim} returns a trimmed version of the numeric vector \code{x}.  \code{NA} values in 
#'   \code{x} are ignored during the trimming process but preserved in the output.
#'
#' \code{trim} is designed to be readable from the function call.  For example, 
#'   \code{trim(x, lo=-1, hi=1)} can be read as "Trim x at -1 and 1".
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
#' 
#' # Using smart type default
#' x_val <- trim(x, lo=-1, hi=1)
#' summary(x_val)
#' x_per <- trim(x, lo=.05, hi=.95)
#' summary(x_per)
#' 
#' # Using explicit type arguments
#' x_val <- trim(x, lo=.05, hi=.95, type='v')
#' summary(x_val)
#' x_per <- trim(x, lo=.05, hi=.95, type='p')
#' summary(x_per)

trim <- function(x, lo, hi, type=c('smart','value','percentile')) {
  # Argument existence check
  if (missing(x)) {
    stop('Please provide a vector x')
  } else if (missing(lo)) {
    stop('Please provide a lower argument lo')
  } else if (missing(hi)) {
    stop('Please provide a upper argument hi')
  }
  # Argument validity check
  if (!is.integer(x) & !is.numeric(x)) {
    stop('x must be integer or numeric')
  } else if (!is.integer(lo) & !is.numeric(lo)) {
    stop('lo must be integer or numeric')
  } else if (!is.integer(hi) & !is.numeric(hi)) {
    stop('hi must be integer or numeric')
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
    message(paste('Smart type:', type))
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