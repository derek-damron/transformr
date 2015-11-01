trim_value <- function(x, lo, hi) {
  if (missing(x)) {
    stop('Please provide a vector x')
  }
  if (missing(lo)) {
    stop('Please provide a lower value lo')
  }
  if (missing(hi)) {
    stop('Please provide a lower value lo')
  }
  x[x <= lo] <- lo
  x[x >= hi] <- hi
  return(x)
}

trim_percent <- function(x, lo=0, hi=1) {
  if (missing(x)) {
    stop('Please provide a vector x')
  }
  lo_val <- quantile(x, prob=lo, type=8)
  hi_val <- quantile(x, prob=hi, type=8)
  x[x <= lo_val] <- lo_val
  x[x >= hi_val] <- hi_val
  return(x)
}