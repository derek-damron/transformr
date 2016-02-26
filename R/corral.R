#' Corral a character vector
#'
#' \code{corral} returns a corralled factor vector.
#'
#' \code{corral} returns a corralled version of the character vector \code{x}.
#'   \code{NA} values in \code{x} are not grouped during the corralling
#'   process but are preserved in the output.
#'
#' \code{corral} is designed to be readable from the function call.
#'   For example:
#' \itemize{
#'   \item \code{corral(x, method="size", groups=5)} can be read as
#'     "\strong{corral} \strong{x} by \strong{size} into \strong{5} groups".
#'   \item \code{corral(x, method="name", groups=c("a","b"))} can be read as
#'     "\strong{corral} \strong{x} by \strong{name} into explicit groups for
#'     \strong{a} and \strong{b}".
#' }
#'
#' The output of \code{corral} is determined by the arguments \code{method} and
#'   \code{groups}.
#'
#' \code{corral} offers a couple different options for \code{method}:
#'
#' \itemize{
#'   \item \strong{size}: The default option that corrals \code{x} based on
#'     the number of occurrences in \code{x}.
#'   \item \strong{name}: Corrals \code{x} based on alphanumerical order.
#'   \item \strong{asis}: Corrals \code{x} based on the order in which values are observed.
#' }
#'
#' \code{corral} accepts either numeric or character values for \code{groups}:
#' \itemize{
#'   \item \strong{numeric}: Creates \code{groups} groups based on
#'     \code{method}.
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
#'   one of the strings \code{"size"}, \code{"name"}, or \code{"asis"}.  See
#'   Details for more information.
#' @param groups Either an integer with the desired number of groups or a
#'   character vector with the groups to keep.  If no argument is provided then
#'   all values will be kept distinct.
#' @param collect A character string that denotes the name that the "collected" values
#'   are given.  The default is "Other".
#' @return The output of \code{corral} is a corralled factor vector with the
#'   same length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- sample(letters, 1e4, replace=TRUE)
#' summary(x)
#'
#' # Corral by size into 5 groups
#' x_5 <- corral(x, "size", groups=5)
#' summary(x_5)
#' # The four most common values are kept and
#' # everything else is combined into "Other"
#'
#' # Corral by name into 5 groups
#' x_5 <- corral(x, "name", group=5)
#' summary(x_5)
#' # The four values first in alphanumerical order are kept and
#' # everything else is combined into "Other"
#'
#' # Corral as is into 5 groups
#' x_5 <- corral(x, "asis", group=5)
#' summary(x_5)
#' # The four unique values that first appear in x are kept in observed order and
#' # everything else is combined into "Other"
#'
#' # Corral by size into groups b, a, r, and others
#' x_bar <- corral(x, "size", groups=c("b","a","r"))
#' summary(x_bar)
#' # The values "b", "a", and "r" are explicitly kept and
#' # leveled based on size (i.e. "r" is the most common value followed "a" then "b")
#'
#' # Corral by name into groups b, a, r, and others
#' x_bar <- corral(x, "name", groups=c("b","a","r"))
#' summary(x_bar)
#' # The values "b", "a", and "r" are explicitly kept and
#' # leveled based on alphanumerical order (i.e. "a" comes before "b" which comes before "r")
#'
#' # Corral as is into groups b, a, r, and others
#' x_bar <- corral(x, "asis", groups=c("b","a","r"))
#' summary(x_bar)
#' # The values "b", "a", and "r" are explicitly kept and
#' # leveled based on the order provided (i.e. "b" then "a" then "r")
#'
#' # Corral as is into groups b, a, r, and "Other Letters"
#' x_bar <- corral(x, "asis", groups=c("b","a","r"), collect="Other Letters")
#' summary(x_bar)
#' # The "collected" letters are given the label "Other Letters" instead of just "Other"

corral <- function(x, method=c("size", "name", "asis"), groups=NULL, collect="Other") {
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
