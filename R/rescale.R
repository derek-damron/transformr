#' Rescale a numeric vector
#'
#' \code{rescale} returns a rescaled numeric vector.
#'
#' \code{rescale} returns a rescaled version of the numeric vector \code{x}.
#'   \code{NA} values in \code{x} are ignored during the rescaling process but
#'   are preserved in the output.
#'
#' \code{rescale} is designed to be readable from the function call.
#'   For example:
#' \itemize{
#'   \item \code{rescale(x, "normal", mean=0, sd=1)} can be read as
#'     "\strong{Rescale} \strong{x} using a \strong{normal}-style
#'     transformation with \strong{mean 0} and \strong{standard deviation 1}".
#'   \item \code{rescale(x, "uniform", min=0, max=1)} can be read as
#'     "\strong{Rescale} \strong{x} using a \strong{uniform}-style
#'     transformation with \strong{min 0} and \strong{max 1}".
#' }
#'
#' The arguments \code{mean}, \code{sd}, \code{min} and \code{max} are used
#'   based on \code{type}.  \code{rescale} offers a couple of different options
#'   for \code{type}:
#'
#' \itemize{
#'   \item \strong{normal}: The default option that rescales \code{x} using
#'     a normal-style transformation into a distribution with mean \code{mean}
#'     and standard deviation \code{sd}.  The default values for this rescaling
#'     are \code{mean=0} and \code{sd=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{(x - \mu_x)/(\sigma_x) * \sigma + \mu}
#'
#'     where \eqn{\mu_x} is the sample mean of \code{x}, \eqn{\sigma_x}
#'     is the sample standard deviation of \code{x}, \eqn{\mu} is the desired
#'     mean, and \eqn{\sigma} is the desired standard deviation.
#'
#'   \item \strong{uniform}: rescales \code{x} using a uniform-style
#'     transformation into a distribution with minimum \code{min} and maximum
#'     \code{max}.  The default values for this rescaling are \code{min=0}
#'     and \code{max=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{((x - min_x)/(max_x - min_x)) * (max - min) + min}
#'
#'     where \eqn{min_x} is the sample minimum of \code{x}, \eqn{max_x}
#'     is the sample maximum of \code{x}, \eqn{min} is the desired minimum,
#'     and \eqn{max} is the desired maximum.
#' }
#'
#' @param x A numeric vector.
#' @param type A character string indicating the desired type of rescaling
#'   with \code{"normal"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"normal"} or \code{"uniform"}.  See Details
#'   for more information.
#' @param mean The desired mean value for normal-style scaling.
#'   Used only when \code{type="normal"}.
#' @param sd The desired standard deviation value for normal-style scaling.
#'   Used only when \code{type="normal"}.
#' @param min The desired minimum value for uniform-style scaling.
#'   Used only when \code{type="uniform"}.
#' @param max The desired maximum value for uniform-style scaling.
#'   Used only when \code{type="uniform"}.
#' @return The output of \code{rescale} is a rescaled numeric vector with
#'   the same length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' # Rescale to standard normal
#' x_normal <- rescale(x, "normal")
#' summary(x_normal)
#' mean(x_normal); sd(x_normal)
#'
#' # Rescale to normal with mean=5 and sd=2
#' x_normal <- rescale(x, "normal", mean=5, sd=2)
#' summary(x_normal)
#' mean(x_normal); sd(x_normal)
#'
#' # Rescale to standard uniform
#' x_uniform <- rescale(x, "uniform")
#' summary(x_uniform)
#' min(x_uniform); max(x_uniform)
#'
#' # Rescale to uniform with min=-3 and max=10
#' x_uniform <- rescale(x, "uniform", min=-3, max=10)
#' summary(x_uniform)
#' min(x_uniform); max(x_uniform)

rescale <- function(x, type=c("normal", "uniform"), mean=0, sd=1, min=0, max=1) {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to rescale", call.=FALSE)
    } else if (!is.numeric(x)) {
        stop("x must be a numeric vector", call.=FALSE)
    }

    # Check type
    type <- match.arg(type)

    # Rescale
    f <- paste("rescale", type, sep=".")
    do.call(f, list(x=x, mean=mean, sd=sd, min=min, max=max))
}

rescale.normal <- function(x, mean=0, sd=1, ...) {
    # Check mean and sd
    if (!is.numeric(mean)) {
        stop("mean must be a numeric value", call.=FALSE)
    } else if (length(mean) != 1) {
        stop("mean must be a single value", call.=FALSE)
    } else if (!is.numeric(sd) || sd <= 0) {
        stop("sd must be a positive numeric value", call.=FALSE)
    } else if (length(sd) != 1) {
        stop("sd must be a single value", call.=FALSE)
    }

    # Rescale
    mean_x <- mean(x, na.rm=TRUE)
    sd_x <- sd(x, na.rm=TRUE)
    (x - mean_x) / (sd_x) * sd + mean
}

rescale.uniform <- function(x, min=0, max=1, ...) {
    # Check min and max
    if (!is.numeric(min)) {
        stop("min must be a numeric value", call.=FALSE)
    } else if (length(min) != 1) {
        stop("min must be a single value", call.=FALSE)
    } else if (!is.numeric(max)) {
        stop("max must be a numeric value", call.=FALSE)
    } else if (length(max) != 1) {
        stop("max must be a single value", call.=FALSE)
    } else if (min > max) {
        stop("min must be less than or equal to max", call.=FALSE)
    }

    # Rescale
    min_x <- min(x, na.rm=TRUE)
    max_x <- max(x, na.rm=TRUE)
    (x - min_x) / (max_x - min_x) * (max - min) + min
}
