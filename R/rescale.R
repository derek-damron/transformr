#' Rescale a numeric vector
#'
#' \code{rescale} returns a rescaled numeric vector.
#'
#' \code{rescale} returns a rescaled version of the numeric vector \code{x}.  \code{NA} values in
#'   \code{x} are ignored during the rescaling process but preserved in the output.
#'
#' \code{rescale} is designed to be readable from the function call.  For example,
#'   \code{rescale(x, "normal", mean=0, sd=1)} can be read as "\strong{Rescale} \strong{x} using
#'   a \strong{normal} transformation with \strong{mean 0} and \strong{standard deviation 1}".
#'
#' The arguments \code{mean}, \code{sd}, \code{min} and \code{max} are used based on \code{type}.
#'   \code{rescale} offers a couple different options for \code{type}:
#'
#' \itemize{
#'   \item \strong{normal}: The default option that rescales \code{x} using a normal transformation
#'     into a distribution with mean \code{mean} and standard deviation \code{sd}.  The default values
#'     for this rescaling are \code{mean=0} and \code{sd=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{(x - \mu_x)/(\sigma_x) * \sigma + \mu}
#'
#'     where \eqn{\mu_x} is the sample mean of \code{x}, \eqn{\sigma_x} is the sample standard deviation
#'     of \code{x}, \eqn{\mu} is the desired mean, and \eqn{\sigma} is the desired standard deviation.
#'
#'   \item \strong{uniform}: rescales \code{x} using a uniform transformation into a distribution
#'     with minimum \code{min} and maximum \code{max}.  The default values for this rescaling are
#'     \code{min=0} and \code{max=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{((x - min_x)/(max_x - min_x)) * (max - min) + min}
#'
#'     where \eqn{min_x} is the sample minimum of \code{x}, \eqn{max_x} is the sample maximum of \code{x},
#'     \eqn{min} is the desired minimum, and \eqn{max} is the desired maximum.
#' }
#'
#' @param x A numeric vector.
#' @param type A character string indicating the desired type of rescaling with \code{"normal"}
#'   being the default.  This must be (an abbreviation of) one of the strings \code{"normal"}
#'   or \code{"uniform"}.  See Details for more information.
#' @param mean The desired mean value for normal-style scaling.  Used only when \code{type="normal"}.
#' @param sd The desired standard deviation value for normal-style scaling.  Used only when
#'   \code{type="normal"}.
#' @param min The desired minimum value for uniform-style scaling.  Used only when \code{type="uniform"}.
#' @param max The desired mean value for uniform-style scaling.  Used only when \code{type="uniform"}.
#' @return The output of \code{rescale} is a rescaled numeric vector with the same length as \code{x}.
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' # Rescale to standard normal
#' x_normal <- rescale(x, 'n')
#' summary(x_normal)
#' mean(x_normal); sd(x_normal)
#'
#' # Rescale to standard uniform
#' x_uniform <- rescale(x, 'u')
#' summary(x_uniform)
#' min(x_uniform); max(x_uniform)
#'
#' # Rescale to normal with mean=5 and sd=2
#' x_normal <- rescale(x, 'n', mean=5, sd=2)
#' summary(x_normal)
#' mean(x_normal); sd(x_normal)
#'
#' # Rescale to uniform with min=-3 and max=10
#' x_uniform <- rescale(x, 'u', min=-3, max=10)
#' summary(x_uniform)
#' min(x_uniform); max(x_uniform)

rescale <- function(x, type=c('normal','uniform'), mean=0, sd=1, min=0, max=1) {
  # Check x
  if (missing(x)) {
    stop('Please provide a vector x', call.=FALSE)
  } else if (!numeric_or_integer(x)) {
    stop('x must be integer or numeric', call.=FALSE)
  }

  # Check type
  type <- match.arg(type)

  # Rescale
  if (type == "normal") {
    rescale_normal(x=x, mean=mean, sd=sd)
  } else {
    rescale_uniform(x=x, min=min, max=max)
  }
}

rescale_normal <- function(x, mean, sd) {
  # Check mean
  if (!numeric_or_integer(mean)) {
    stop('mean must be a numeric or integer value', call.=FALSE)
  }

  # Check sd
  if (!numeric_or_integer(sd) | sd <= 0) {
    stop('sd must be a positive numeric or integer value', call.=FALSE)
  }

  # Rescale
  mean_x <- mean(x, na.rm=TRUE)
  sd_x <- sd(x, na.rm=TRUE)
  x <- (x - mean_x) / (sd_x) * sd + mean
  return(x)
}

rescale_uniform <- function(x, min, max) {
  # Check min
  if (!numeric_or_integer(min)) {
    stop('min must be a numeric or integer value', call.=FALSE)
  }

  # Check max
  if (!numeric_or_integer(max)) {
    stop('max must be a numeric or integer value', call.=FALSE)
  }

  # Check min <= max
  if (min > max) {
    stop('min must be less than or equal to max', call.=FALSE)
  }

  # Rescale
  min_x <- min(x, na.rm=TRUE)
  max_x <- max(x, na.rm=TRUE)
  x <- (x - min_x) / (max_x - min_x) * (max - min) + min
  return(x)
}

numeric_or_integer <- function(arg) {
  if (is.numeric(arg) | is.integer(arg)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
