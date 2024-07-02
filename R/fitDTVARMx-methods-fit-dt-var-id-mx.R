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

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitdtvaridmx`.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param ... additional arguments.
#' @return Returns a list of vectors of parameter estimates.
#'
#' @method coef fitdtvaridmx
#' @keywords methods
#' @export
coef.fitdtvaridmx <- function(object,
                              psi = TRUE,
                              ...) {
  coef_1 <- coef(object$output[[1]])
  if (psi) {
    idx <- names(coef_1)
  } else {
    idx <- grep(
      pattern = "^beta_",
      x = names(coef_1)
    )
  }
  return(
    lapply(
      X = object$output,
      FUN = function(x,
                     idx) {
        return(coef(x)[idx])
      },
      idx = idx
    )
  )
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitdtvaridmx`.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov fitdtvaridmx
#' @keywords methods
#' @export
vcov.fitdtvaridmx <- function(object,
                              psi = TRUE,
                              ...) {
  coef_1 <- coef(object$output[[1]])
  if (psi) {
    idx <- names(coef_1)
  } else {
    idx <- grep(
      pattern = "^beta_",
      x = names(coef_1)
    )
  }
  return(
    lapply(
      X = object$output,
      FUN = function(x,
                     idx) {
        return(vcov(x)[idx, idx])
      },
      idx = idx
    )
  )
}
