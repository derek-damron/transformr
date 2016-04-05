#' Helper function that samples with replacement
#'
#' \code{impute} returns a vector of values imputed from resampling \code{x}.
#'
#' @param x A vector.
#' @param n The number of values to impute.
#' @param replace Whether or not to sample with replacement.  The default is TRUE.
#' @return The output of \code{impute_sample} is a vector of imputed values of length \code{n}
#' @export
#' @family imputers
#' @examples
#' x <- c("mode", "mode", "other", NA, NA)
#'
#' # Impute via sampling
#' x_impute <- impute(x, impute_sample)
#'
#' # Compare
#' data.frame(x, x_impute)

impute_sample <- function(x, n=1, replace=TRUE) {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to use for sampling", call.=FALSE)
    }

    # Check n
    if (!is.numeric(n) || n < 1) {
        stop("n must be a positive integer", call.=FALSE)
    }
    n <- floor(n)

    # Tabulate
    x_tab <- table_rcpp(x)

    # Convert counts to percentages
    x_prob <- x_tab / length(x)

    # Unique values
    x_unique <- names(x_tab)

    # Sample
    x_sample <- sample(x_unique, size=n, replace=replace, prob=x_prob)

    # Convert back to numeric, if applicable
    if (is.numeric(x)) {
        x_sample <- as.numeric(x_sample)
    }

    # Return
    x_sample
}
