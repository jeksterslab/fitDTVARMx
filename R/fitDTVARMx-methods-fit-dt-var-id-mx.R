#' Print Method for Object of Class `fitdtvaridmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `fitdtvaridmx`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method print fitdtvaridmx
#' @keywords methods
#' @export
print.fitdtvaridmx <- function(x,
                               means = TRUE,
                               ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = x$output,
      FUN = coef
    )
  )
  if (means) {
    cat("\nMeans of the estimated paramaters per individual.\n")
    out <- colMeans(out)
  } else {
    cat("\nEstimated paramaters per individual.\n")
  }
  base::print(out)
}

#' Summary Method for Object of Class `fitdtvaridmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `fitdtvaridmx`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method summary fitdtvaridmx
#' @keywords methods
#' @export
summary.fitdtvaridmx <- function(object,
                                 means = TRUE,
                                 ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = object$output,
      FUN = coef
    )
  )
  if (means) {
    if (interactive()) {
      cat("\nMeans of the estimated paramaters per individual.\n")
    }
    out <- colMeans(out)
  } else {
    if (interactive()) {
      cat("\nEstimated paramaters per individual.\n")
    }
  }
  return(out)
}
