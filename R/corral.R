#' Corral a character vector
#'
#' \code{corral} returns a corralled factor vector.
#'
#' \code{corral} returns a corralled vector \code{x}.
#'   \code{NA} values in \code{x} are not grouped during the corralling
#'   process but are preserved in the output.
#'
#' \code{corral} is designed to be readable from the function call.
#'   For example:
#' \itemize{
#'   \item \code{corral(x, method="size", groups=5)} can be read as
#'     "\strong{corral} \strong{x} by \strong{size} into \strong{5} groups".
#'   \item \code{corral(x, method="asis", groups=c("a","b"))} can be read as
#'     "\strong{corral} \strong{x} \strong{as is} and keep only \strong{a} and
#'     \strong{b} distinct".
#' }
#'
#' The output of \code{corral} is determined by the arguments \code{method} and
#'   \code{groups}.
#'
#' \code{corral} offers a few different options for \code{method}:
#'
#' \itemize{
#'   \item \strong{size}: The default option that corrals \code{x} based on
#'     the number of occurrences in \code{x}.
#'   \item \strong{asis}: Corrals \code{x} based on the order in which values are observed.
#'   \item \strong{name}: Corrals \code{x} based on alphanumerical order.
#' }
#'
#' \code{corral} accepts either numeric or character values for \code{groups}:
#' \itemize{
#'   \item \strong{numeric}: Creates \code{groups} groups based on
#'     \code{method} and combines all other values into the \code{collect} category.
#'   \item \strong{character}: Creates a group for each value in \code{groups}
#'     and combines all other values into the \code{collect} category.
#' }
#'
#' See the examples for some explicit illustration on how different combinations
#'   of \code{method} and \code{groups} result in different outputs.
#'
#' @param x A character vector (or any vector than can be coerced into a
#'   character).
#' @param method A character string indicating the desired method of corralling
#'   with \code{"size"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"size"}, \code{"asis"}, or \code{"name"}.  See
#'   Details for more information.
#' @param groups Either NULL (all values are kept distinct),
#'   a single number with the desired number of groups (floating numbers are truncated),
#'   or a vector of values to keep distinct.  The default is NULL.
#' @param collect Either \code{NA} or a character string that denotes the name that the
#'   "collected" values are given.  The default is "Other".
#' @return The output of \code{corral} is a corralled factor vector with the
#'   same length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- sample(letters, 1e4, replace=TRUE)
#' summary(x)
#'
#' #####
#' # Common use cases
#' #
#'
#' # I want to factorize by sample size!
#' x_all <- corral(x, "size")
#' summary(x_all)
#' # All values are kept and ordered by the number of occurrences
#'
#' # I want to factorize by sample size but only have 5 values!
#' x_5 <- corral(x, "size", groups=5)
#' summary(x_5)
#' # The four most common values are kept and
#' # everything else is combined into "Other"
#'
#' # I want to factorize but keep only specific values!
#' x_bar <- corral(x, "asis", groups=c("b", "a", "r"))
#' summary(x_bar)
#' # The values "b", "a", and "r" are explicitly kept and
#' # leveled based on the order provided (i.e. "b" then "a" then "r")
#'
#' # I want to change the collected values to NA rather than "Other"!
#' x_NA <- corral(x, "asis", groups=c("b", "a", "r"), collect=NA)
#' summary(x_NA)
#' # The values "b", "a", and "r" are factorized as in the previous example and
#' # the rest of the values are changed to NA

corral <- function(x, method=c("size", "asis", "name"), groups=NULL, collect="Other") {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to corral", call.=FALSE)
    }
    if (!is.character(x)) {
        x <- as.character(x)
    }

    # Check method
    method <- match.arg(method)

    # Check groups
    if (!is.null(groups)) {
        if (is.numeric(groups) && length(groups)==1) {
            groups <- floor(groups)
            if (groups <= 0) {
                stop("groups must be a positive integer if supplied as a numeric value", call.=FALSE)
            }
        } else {
            groups <- as.character(groups)
        }
    }

    # Check collect
    if (length(collect) > 1) {
        stop("collect must be a single character string", call.=FALSE)
    }
    collect <- as.character(collect)

    # Derive the unique values and the number of unique values
    if (method == "size") {
        x_tab <- table_rcpp(x)
        x_tab <- x_tab[!is.na(names(x_tab))]
        x_tab <- sort(x_tab, decreasing=TRUE)
        x_unique <- names(x_tab)
    } else if (method == "name") {
        x_unique <- sort(unique(x)[!is.na(unique(x))])
    } else {
        x_unique <- unique(x)[!is.na(unique(x))]
    }
    x_unique_n <- length(x_unique)

    # Derive levels of the output
    if (is.null(groups)) {
        x_levels <- x_unique
    } else if (is.numeric(groups)) {
        if (groups == 1L) {
            x_levels <- NULL
        } else if (groups < x_unique_n) {
            x_levels <- x_unique[1:(groups - 1)]
        } else {
            x_levels <- x_unique
        }
    } else {
        if (method == "asis") {
            x_levels <- intersect(groups, x_unique)
        } else {
            x_levels <- intersect(x_unique, groups)
        }
    }

    # Corral the rest
    x[!x %in% x_levels & !is.na(x)] <- collect

    # Add collect to levels if necessary
    if (collect %in% x) {
        x_levels <- c(x_levels, collect)
    }

    # Factorize
    factor(x, levels=x_levels)
}
