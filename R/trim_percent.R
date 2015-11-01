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