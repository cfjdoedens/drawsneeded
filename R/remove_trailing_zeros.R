#' remove trailing zeros from string containing at least one "."
#'
#' Also removes "." if that is the last character.
#' The idea is that we trim zeros from a number containing decimal places.
#'
#' @param s The input string.
#'
#' @returns
#'   The input string with trailing zeros, after ".", if any, removed.
#'
#' examples
#'   remove_trailing_zeros("37.5600")
remove_trailing_zeros <- function(s) {
  # Check whether s contains a ".".
  dot_found <- FALSE
  for (i  in 1:nchar(s)) {
    if (substring(s, i, i) == ".") {
      dot_found <- TRUE
      break
    }
  }

  # If no dot in s, we are done.
  if (!dot_found) {
    return(s)
  }

  # Remove trailing zeros.
  while (nchar(s) > 1) {
    if (substring(s, nchar(s), nchar(s)) == "0") {
      s <- substring(s, 1, nchar(s) - 1)
    }
    else
      break
  }

  # Remove trailing dot.
  if (substring(s, nchar(s), nchar(s)) == ".") {
    s <- substring(s, 1, nchar(s) - 1)
  }

  # If s is empty, set to "0".
  if (nchar(s) == 0) {
    s <- "0"
  }

  return(s)
}

#' print number in fixed point decimal notation, but no trailing zeros
#'
#' @param nr The input number.
#'
#' @returns
#'   The input number, as a string in fixed point decimal notation,
#'   with trailing zeros removed.
#'
#' examples
#'   formatf_without_trailing_zeros(37.5600)
formatf_without_trailing_zeros <- function(nr) {
  stopifnot(is.numeric(nr))
  stopifnot(length(nr) == 1)
  remove_trailing_zeros(sprintf("%f", nr))
}
