#' Print Method for Object of Class `dtvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `dtvarmx`.
#' @param ... further arguments.
#'
#' @method print dtvarmx
#' @keywords methods
#' @export
print.dtvarmx <- function(x,
                          ...) {
  base::print(
    summary(x$output)
  )
}

#' Summary Method for Object of Class `dtvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `dtvarmx`.
#' @param ... further arguments.
#'
#' @method summary dtvarmx
#' @keywords methods
#' @export
summary.dtvarmx <- function(object,
                            ...) {
  return(
    summary(object$output)
  )
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `dtvarmx`.
#' @param alpha Logical.
#'   If `alpha = TRUE`,
#'   include estimates of the `alpha` vector, if available.
#'   If `alpha = FALSE`,
#'   exclude estimates of the `alpha` vector.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix, if available.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a vector of parameter estimates.
#'
#' @method coef dtvarmx
#' @keywords methods
#' @export
coef.dtvarmx <- function(object,
                         alpha = FALSE,
                         psi = FALSE,
                         theta = FALSE,
                         ...) {
  coefs <- coef(object$output)
  parnames <- names(coefs)
  idx <- grep(
    pattern = "^beta_",
    x = parnames
  )
  if (alpha) {
    idx <- c(
      idx,
      grep(
        pattern = "^alpha_",
        x = parnames
      )
    )
  }
  if (psi) {
    idx <- c(
      idx,
      grep(
        pattern = "^psi_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  return(
    coefs[idx]
  )
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `dtvarmx`.
#' @param alpha Logical.
#'   If `alpha = TRUE`,
#'   include estimates of the `alpha` vector, if available.
#'   If `alpha = FALSE`,
#'   exclude estimates of the `alpha` vector.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix, if available.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov dtvarmx
#' @keywords methods
#' @export
vcov.dtvarmx <- function(object,
                         alpha = FALSE,
                         psi = FALSE,
                         theta = FALSE,
                         ...) {
  vcovs <- vcov(object$output)
  parnames <- rownames(vcovs)
  idx <- grep(
    pattern = "^beta_",
    x = parnames
  )
  if (alpha) {
    idx <- c(
      idx,
      grep(
        pattern = "^alpha_",
        x = parnames
      )
    )
  }
  if (psi) {
    idx <- c(
      idx,
      grep(
        pattern = "^psi_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  return(
    vcovs[idx, idx, drop = FALSE]
  )
}
