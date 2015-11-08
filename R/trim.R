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
