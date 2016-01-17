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
#'   \item \code{corral(x, type="size", groups=5)} can be read as
#'     "\strong{corral} \strong{x} by \strong{size} into \strong{5} groups".
#'   \item \code{corral(x, type="name", groups=c("a","b"))} can be read as
#'     "\strong{corral} \strong{x} by \strong{name} into explicit groups for
#'     \strong{a} and \strong{b}".
#' }
#'
#' The output of \code{corral} is determined by the arguments \code{type} and
#'   \code{groups}.
#'
#' \code{corral} offers a couple different options for \code{type}:
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
#'     \code{type}.
#'
#'   \item \strong{character}: Creates a group for each value in \code{groups}
#'     and combines all other values into the \code{collect} category.
#' }
#'
#' See the examples for some explicit illustration on how different combinations
#'   of \code{type} and \code{groups} result in different outputs.
#'
#' @param x A character vector (or any vector than can be coerced into a
#'   character).
#' @param type A character string indicating the desired type of corralling
#'   with \code{"size"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"size"}, \code{"name"}, or \code{"asis"}.  See
#'   Details for more information.
#' @param groups Either an integer with the desired number of groups or a
#'   character vector with the groups to keep.  If no argument is provided then
#'   all values will be corralled.
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

corral <- function(x, type=c("size", "name", "asis"), groups, collect="Other") {
    # Check x
    if (missing(x)) {
        stop("Please provide a vector x to corral", call.=FALSE)
    }

    # Check groups
    if (missing(groups)) {
        #stop("Please provide a value for groups", call.=FALSE)
        groups <- missing_arg()
    } else if (is.numeric(groups) && length(groups)==1) {
        groups <- floor(groups)
        if (groups <= 0) {
            stop("groups must be a positive integer if supplied as a numeric value", call.=FALSE)
        }
    } else {
        groups <- as.character(groups)
    }

    # Check type
    type <- match.arg(type)

    # Check collect
    if (length(collect) > 1) {
        collect <- collect[1]
    }
    collect <- as.character(collect)

    # Corral
    f <- paste("corral", type, sep=".")
    do.call(f, list(x=x, groups=groups, collect=collect))
}

corral.size <- function(x, groups, collect) {
    # Derive groups
    x_tab <- table(x)
    x_tab <- sort(x_tab, decreasing=TRUE)
    x_n_unique <- length(x_tab)
    if (!specified(groups)) {
        x_groups <- names(x_tab)
        x_levels <- x_groups
    } else if (is.numeric(groups)) {
        if (groups == 1L) {
            x_groups <- NULL
            x_levels <- collect
        } else if (groups < x_n_unique) {
            x_groups <- names(x_tab)[1:(groups - 1)]
            x_levels <- c(x_groups, collect)
        } else {
            x_groups <- names(x_tab)
            x_levels <- x_groups
        }
    } else {
        x_groups <- names(x_tab)[names(x_tab) %in% groups]
        if (length(x_groups) < x_n_unique) {
            x_levels <- c(x_groups, collect)
        } else {
            x_levels <- x_groups
        }
    }

    # Corral the rest
    x[!x %in% x_groups & !is.na(x)] <- collect

    # Convert to factor and level
    factor(x, levels=x_levels)
}

corral.name <- function(x, groups, collect) {
    # Find groups
    x_unique <- sort(unique(x))
    x_n_unique <- length(x_unique)
    if (!specified(groups)) {
        x_groups <- x_unique
        x_levels <- x_groups
    } else if (is.numeric(groups)) {
        if (groups == 1L) {
            x_groups <- NULL
            x_levels <- collect
        } else if (groups < x_n_unique) {
            x_groups <- x_unique[1:(groups - 1)]
            x_levels <- c(x_groups, collect)
        } else {
            x_groups <- x_unique
            x_levels <- x_groups
        }
    } else {
        x_groups <- x_unique[x_unique %in% groups]
        if (length(x_groups) < x_n_unique) {
            x_levels <- c(x_groups, collect)
        } else {
            x_levels <- x_groups
        }
    }

    # Corral the rest
    x[!x %in% x_groups & !is.na(x)] <- collect

    # Convert to factor and level
    factor(x, levels=x_levels)
}

corral.asis <- function(x, groups, collect) {
    # Find groups
    x_unique <- unique(x)[!is.na(unique(x))]
    x_n_unique <- length(x_unique)
    if (!specified(groups)) {
        x_groups <- x_unique
        x_levels <- x_groups
    } else if (is.numeric(groups)) {
        if (groups == 1L) {
            x_groups <- NULL
            x_levels <- collect
        } else if (groups < x_n_unique) {
            x_groups <- x_unique[1:(groups - 1)]
            x_levels <- c(x_groups, collect)
        } else {
            x_groups <- x_unique
            x_levels <- x_groups
        }
    } else {
        x_groups <- groups[groups %in% x_unique]
        if (length(x_groups) < x_n_unique) {
            x_levels <- c(x_groups, collect)
        } else {
            x_levels <- x_groups
        }
    }

    # Corral the rest
    x[!x %in% x_groups & !is.na(x)] <- collect

    # Convert to factor and level
    factor(x, levels=x_levels)
}
