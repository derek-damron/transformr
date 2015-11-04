trim <- function() {
  return(1)
}

trim_percentile <- function() {
  return(0)
}

trim_value <- function() {
  return(0)
}

# trim <- function(x, lo, hi, type=c('value','percentile')) {
#   if (missing(x)) {
#     stop('Please provide a vector x')
#   } else if (missing(lo)) {
#     stop('Please provide a lower value lo')
#   } else if (missing(hi)) {
#     stop('Please provide a upper value hi')
#   } 
#   if (missing(type)) {
#     warning('No type provided - Using value')
#     type <- 'value'
#   }
#   type <- match.arg(type)
#   if (type == 'percentile') {
#     lo <- lo / 100
#     lo <- quantile(x, prob=lo, type=8)
#     hi <- 1 - hi / 100
#     hi <- quantile(x, prob=hi, type=8)
#   }
#   x[x <= lo] <- lo
#   x[x >= hi] <- hi
#   return(x)
# }
