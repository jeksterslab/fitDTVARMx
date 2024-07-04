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
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   estimate the diagonal measurement error matrix `theta`.
#' @param theta_start Optional starting values for `theta`.
#'   Ignored if `theta = FALSE`.
#' @param theta_lbound Optional lower bound for `theta`.
#'   Ignored if `theta = FALSE`.
#' @param theta_ubound Optional upper bound for `theta`.
#'   Ignored if `theta = FALSE`.
#'
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
                         psi_start = NULL,
                         psi_lbound = NULL,
                         psi_ubound = NULL,
                         psi_diag = TRUE,
                         theta = FALSE,
                         theta_start = NULL,
                         theta_lbound = NULL,
                         theta_ubound = NULL,
                         try = 1000,
                         ncores = NULL) {
  args <- list(
    data = data,
    observed = observed,
    id = id,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    theta = theta,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
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
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    psi_diag = psi_diag,
    theta = theta,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
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
