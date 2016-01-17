missing_arg <- function() {
  structure(list(), class="missing_arg")
}

is.missing_arg <- function(arg) {
  if (class(arg) == "missing_arg") {
    TRUE
  } else {
    FALSE
  }
}

specified <- function(arg) {
    !is.missing_arg(arg)
}
