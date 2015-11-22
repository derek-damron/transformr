#' Corral a character vector
#'
#' \code{corral} returns a corralled factor vector.
#'
#' \code{corral} returns a corralled version of the character vector \code{x}.  \code{NA} values in
#'   \code{x} are not grouped during the corralling process but preserved in the output.
#'
#' \code{corral} is designed to be readable from the function call.  For example,
#'   \code{corral(x, 5, "size")} can be read as "\strong{corral} \strong{x} into
#'   \strong{5} groups by \strong{size}".
#'
#' @param x A character vector (or any vector than can be coerced into a character).
#' @param groups Either an integer with the desired number of groups or a character vector
#'   with the groups to keep.
#' @param by A character string indicating the desired type of corralling with \code{"size"}
#'   being the default.  This must be (an abbreviation of) one of the strings \code{"size"} or
#'   \code{"name"}.  See Details for more information.
#' @return The output of \code{corral} is a corralled factor vector with the same length as \code{x}.
#' @examples
#' set.seed(1337)
#' x <- sample(letters, 1e4, replace=TRUE)
#' summary(x)
#'
#' # Corral into 5 groups by size
#' x_5 <- corral(x, 5, by='s')
#' summary(x_5)
#'
#' # Corral into 5 groups by name
#' x_5 <- corral(x, 5, by='n')
#' summary(x_5)
#'
#' # Corral into groups 'a', 'b', 'c', and others by size
#' x_abc <- corral(x, c('a','b','c'), by='s')
#' summary(x_abc)
#'
#' # Corral into groups 'a', 'b', 'c', and others by name
#' x_abc <- corral(x, c('a','b','c'), by='n')
#' summary(x_abc)

corral <- function(x, groups, by=c("size","name")) {
  # Check x
  if (missing(x)) {
    stop('Please provide a vector x', call.=FALSE)
  }

  # Check groups
  if (numeric_or_integer(groups)) {
    groups <- as.integer(groups)
    if (groups <= 0) {
      stop('groups must be a positive integer if supplied as a numeric value')
    }
  } else {
    groups <- as.character(groups)
  }

  # Check by
  by <- match.arg(by)

  # Corral
  f <- paste('corral', by, sep="_")
  do.call(f, list(x=x, groups=groups))
}

corral_size <- function(x, groups) {
  # Derive groups
  x_tab <- table(x)
  x_tab <- sort(x_tab, decreasing=TRUE)
  x_n_unique <- length(x_tab)
  if (is.integer(groups)) {
    if (groups == 1) {
      x_groups <- NULL
      x_levels <- 'Other'
    } else if (groups < x_n_unique) {
      x_groups <- names(x_tab)[1:(groups - 1)]
      x_levels <- c(x_groups, 'Other')
    } else {
      x_groups <- names(x_tab)
      x_levels <- x_groups
    }
  } else {
    x_groups <- names(x_tab)[names(x_tab) %in% groups]
    if (length(x_groups) < x_n_unique) {
      x_levels <- c(x_groups, 'Other')
    } else {
      x_levels <- x_groups
    }
  }

  # Corral the rest
  x[!x %in% x_groups & !is.na(x)] <- 'Other'

  # Convert to factor and level
  x <- factor(x, levels=x_levels)
  x
}

corral_name <- function(x, groups) {
  # Find groups
  x_unique <- sort(unique(x))
  x_n_unique <- length(x_unique)
  if (is.integer(groups)) {
    if (groups == 1) {
      x_groups <- NULL
      x_levels <- 'Other'
    } else if (groups < x_n_unique) {
      x_groups <- x_unique[1:(groups - 1)]
      x_levels <- c(x_groups, 'Other')
    } else {
      x_groups <- x_unique
      x_levels <- x_groups
    }
  } else {
    x_groups <- x_unique[x_unique %in% groups]
    if (length(x_groups) < x_n_unique) {
      x_levels <- c(x_groups, 'Other')
    } else {
      x_levels <- x_groups
    }
  }

  # Corral the rest
  x[!x %in% x_groups & !is.na(x)] <- 'Other'

  # Convert to factor and level
  x <- factor(x, levels=x_levels)
  x
}

numeric_or_integer <- function(arg) {
  if (is.integer(arg) | is.numeric(arg)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
