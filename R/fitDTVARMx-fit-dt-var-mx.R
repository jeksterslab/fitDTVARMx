#' Fit the First-Order Discrete-Time Vector Autoregressive Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams FitDTVARIDMx
#'
#' @return Returns an object of class `fitdtvarmx` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fun}{Function used ("FitDTVARMx").}
#'     \item{output}{A fitted OpenMx model.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Generate data using the simStateSpace package------------------------------
#' set.seed(42)
#' sim <- simStateSpace::SimSSMVARFixed(
#'   n = 5,
#'   time = 100,
#'   mu0 = rep(x = 0, times = 3),
#'   sigma0_l = t(chol(diag(3))),
#'   alpha = rep(x = 0, times = 3),
#'   beta = matrix(
#'     data = c(
#'       0.7, 0.5, -0.1,
#'       0.0, 0.6, 0.4,
#'       0, 0, 0.5
#'     ),
#'     nrow = 3
#'   ),
#'   psi_l = t(chol(diag(3)))
#' )
#' data <- as.data.frame(sim)
#'
#' # Fit the model--------------------------------------------------------------
#' library(fitDTVARMx)
#' fit <- FitDTVARMx(
#'   data = data,
#'   observed = c("y1", "y2", "y3"),
#'   id = "id"
#' )
#' print(fit)
#' summary(fit)
#' coef(fit)
#' vcov(fit)
#' }
#'
#' @family DTVAR Functions
#' @keywords fitDTVARMx fit
#' @export
FitDTVARMx <- function(data,
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
  byid <- FALSE
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
    ncores = ncores,
    byid = byid
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
    ncores = ncores,
    byid = byid
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitDTVARMx",
    output = output
  )
  class(out) <- c(
    "fitdtvarmx",
    class(out)
  )
  return(out)
}
