#' Print Method for Object of Class `fitdtvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `fitdtvarmx`.
#' @param ... further arguments.
#'
#' @method print fitdtvarmx
#' @keywords methods
#' @export
print.fitdtvarmx <- function(x,
                             ...) {
  base::print(
    summary(x$output)
  )
}

#' Summary Method for Object of Class `fitdtvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `fitdtvarmx`.
#' @param ... further arguments.
#'
#' @method summary fitdtvarmx
#' @keywords methods
#' @export
summary.fitdtvarmx <- function(object,
                               ...) {
  return(
    summary(object$output)
  )
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitdtvarmx`.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a vector of parameter estimates.
#'
#' @method coef fitdtvarmx
#' @keywords methods
#' @export
coef.fitdtvarmx <- function(object,
                            psi = FALSE,
                            theta = FALSE,
                            ...) {
  coefs <- coef(object$output)
  parnames <- names(coefs)
  idx <- grep(
    pattern = "^beta_",
    x = parnames
  )
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
#' @param object Object of class `fitdtvarmx`.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov fitdtvarmx
#' @keywords methods
#' @export
vcov.fitdtvarmx <- function(object,
                            psi = FALSE,
                            theta = FALSE,
                            ...) {
  vcovs <- vcov(object$output)
  parnames <- rownames(vcovs)
  idx <- grep(
    pattern = "^beta_",
    x = parnames
  )
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
