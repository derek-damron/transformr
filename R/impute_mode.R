#' Helper function that calculates the mode of a vector with no missing values
#'
#' \code{impute_mode} returns a vector of values imputed from the mode(s) of \code{x}.
#'
#' @param x A vector.
#' @param n The number of values to impute.  Only relevant when  when there is not a unique mode.
#' @param tiebreaker How to break "ties" if there is not a single unique mode.  The default is
#'   "random", which samples randomly with replacement from the different modes.
#' @return The output of \code{impute_mode} is a vector of imputed values of length \code{n}
#' @export
#' @examples
#' x <- c("mode", "mode", "other", NA, NA)
#'
#' # Impute mode
#' x_impute <- impute(x, impute_mode)
#'
#' # Compare
#' data.frame(x, x_impute)

impute_mode <- function(x, n=1, tiebreaker="random") {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to compute the mode", call.=FALSE)
    }

    # Check tiebreaker
    tiebreaker <- match.arg(tiebreaker)

    # Tabulate
    x_tab <- table_rcpp(x)

    # Sort in decreasing order
    x_tab <- sort(x_tab, decreasing=TRUE)

    # Mode(s)
    x_mode <- names(x_tab[x_tab == max(x_tab)])

    # Handle ties
    if (length(x_mode) > 1) {
        if (tiebreaker == "random") {
            x_mode <- sample(x_mode, size=n, replace=TRUE)
        }
    }

    # Convert back to numeric, if applicable
    if (is.numeric(x)) {
        x_mode <- as.numeric(x_mode)
    }

    # Return
    x_mode
}
