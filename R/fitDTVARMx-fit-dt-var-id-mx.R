#' Fit First Order Discrete-Time Vector Autoregressive Model by ID
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param data Data frame.
#'   A data frame object of data for potentially
#'   multiple subjects that contain
#'   a column of subject ID numbers
#'   (i.e., an ID variable), and
#'   at least one column of observed values.
#' @param observed Character vector.
#'   A vector of character strings
#'   of the names of the observed variables in the data.
#' @param id Character string.
#'   A character string of the name of the ID variable in the data.
#' @param beta_start Numeric matrix.
#'   Optional starting values for `beta`.
#' @param beta_lbound Numeric matrix.
#'   Optional lower bound for `beta`.
#' @param beta_ubound Numeric matrix.
#'   Optional upper bound for `beta`.
#' @param psi_start Numeric matrix.
#'   Optional starting values for `psi`.
#' @param psi_lbound Numeric matrix.
#'   Optional lower bound for `psi`.
#' @param psi_ubound Optional upper bound for `psi`.
#' @param psi_diag Logical.
#'   If `psi_diag = TRUE`,
#'   `psi` is a diagonal matrix.
#' @param theta_fixed Logical.
#'   If `theta_fixed = TRUE`,
#'   the measurement error matrix `theta` is fixed to zero.
#'   If `theta_fixed = FALSE`,
#'   estimate the diagonal measurement error matrix `theta`.
#' @param theta_start Optional starting values for `theta`.
#'   Ignored if `theta_fixed = TRUE`.
#' @param theta_lbound Optional lower bound for `theta`.
#'   Ignored if `theta_fixed = TRUE`.
#' @param theta_ubound Optional upper bound for `theta`.
#'   Ignored if `theta_fixed = TRUE`.
#' @param mu0_fixed Logical.
#'   If `mu0_fixed = TRUE`,
#'   initial mean vector `mu0` is fixed.
#'   If `mu0_fixed = FALSE`,
#'   initial mean vector `mu0` is estimated.
#' @param mu0_start Optional starting values for `mu0`.
#'   If `mu0_fixed = TRUE`, `mu0_start` will be used as fixed values.
#'   If `mu0_fixed = FALSE`, `mu0_start` will be used as starting values.
#' @param mu0_lbound Optional lower bound for `mu0`.
#'   Ignored if `mu0_fixed = TRUE`.
#' @param mu0_ubound Optional upper bound for `mu0`.
#'   Ignored if `mu0_fixed = TRUE`.
#' @param sigma0_fixed Logical.
#'   If `sigma0_fixed = TRUE`,
#'   initial mean vector `sigma0` is fixed.
#'   If `sigma0_fixed = FALSE`,
#'   initial mean vector `sigma0` is estimated.
#' @param sigma0_diag Logical.
#'   If `sigma0_diag = TRUE`,
#'   `sigma0` is a diagonal matrix.
#' @param sigma0_start Optional starting values for `sigma0`.
#'   If `sigma0_fixed = TRUE`, `sigma0_start` will be used as fixed values.
#'   If `sigma0_fixed = FALSE`, `sigma0_start` will be used as starting values.
#' @param sigma0_lbound Optional lower bound for `sigma0`.
#'   Ignored if `sigma0_fixed = TRUE`.
#' @param sigma0_ubound Optional upper bound for `sigma0`.
#'   Ignored if `sigma0_fixed = TRUE`.
#' @param try Positive integer.
#'   Number of extra tries for [OpenMx::mxTryHard()].
#' @param ncores Positive integer.
#'   Number of cores to use.
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR fit
#' @import OpenMx
#' @importFrom stats coef vcov
#' @export
FitDTVARIDMx <- function(data,
                         observed,
                         id,
                         beta_start = NULL,
                         beta_lbound = NULL,
                         beta_ubound = NULL,
                         psi_diag = TRUE,
                         psi_start = NULL,
                         psi_lbound = NULL,
                         psi_ubound = NULL,
                         theta_fixed = TRUE,
                         theta_start = NULL,
                         theta_lbound = NULL,
                         theta_ubound = NULL,
                         mu0_fixed = TRUE,
                         mu0_start = NULL,
                         mu0_lbound = NULL,
                         mu0_ubound = NULL,
                         sigma0_fixed = TRUE,
                         sigma0_diag = TRUE,
                         sigma0_start = NULL,
                         sigma0_lbound = NULL,
                         sigma0_ubound = NULL,
                         try = 1000,
                         ncores = NULL) {
  args <- list(
    data = data,
    observed = observed,
    id = id,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_diag = psi_diag,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores
  )
  output <- .FitDTVAR(
    data = data,
    observed = observed,
    id = id,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_diag = psi_diag,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitDTVARIDMx",
    output = output
  )
  class(out) <- c(
    "fitdtvaridmx",
    class(out)
  )
  return(out)
}
