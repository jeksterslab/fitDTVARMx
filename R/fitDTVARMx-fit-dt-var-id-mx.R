#' Fit the First-Order Discrete-Time Vector Autoregressive Model by ID
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
#'   Number of extra optimization tries.
#' @param ncores Positive integer.
#'   Number of cores to use.
#'
#' @return Returns an object of class `fitdtvaridmx` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fun}{Function used ("FitDTVARIDMx").}
#'     \item{output}{A list of fitted OpenMx models.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Generate data using the simStateSpace package------------------------------
#' set.seed(42)
#' beta_mu <- matrix(
#'   data = c(
#'     0.7, 0.5, -0.1,
#'     0.0, 0.6, 0.4,
#'     0, 0, 0.5
#'   ),
#'   nrow = 3
#' )
#' beta_sigma <- diag(3 * 3)
#' beta <- simStateSpace::SimBetaN(
#'   n = 5,
#'   beta = beta_mu,
#'   vcov_beta_vec_l = t(chol(beta_sigma))
#' )
#' sim <- simStateSpace::SimSSMVARIVary(
#'   n = 5,
#'   time = 100,
#'   mu0 = list(rep(x = 0, times = 3)),
#'   sigma0_l = list(t(chol(diag(3)))),
#'   alpha = list(rep(x = 0, times = 3)),
#'   beta = beta,
#'   psi_l = list(t(chol(diag(3))))
#' )
#' data <- as.data.frame(sim)
#'
#' # Fit the model--------------------------------------------------------------
#' library(fitDTVARMx)
#' fit <- FitDTVARIDMx(
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
#' @references
#' Hunter, M. D. (2017).
#' State space modeling in an open source, modular,
#' structural equation modeling environment.
#' *Structural Equation Modeling: A Multidisciplinary Journal*,
#' *25*(2), 307–324.
#' \doi{10.1080/10705511.2017.1369354}
#'
#' Neale, M. C., Hunter, M. D., Pritikin, J. N.,
#' Zahery, M., Brick, T. R., Kirkpatrick, R. M., Estabrook, R.,
#' Bates, T. C., Maes, H. H., & Boker, S. M. (2015).
#' OpenMx 2.0: Extended structural equation and statistical modeling.
#' *Psychometrika*,
#' *81*(2), 535–549.
#' \doi{10.1007/s11336-014-9435-8}
#'
#' @family DTVAR Functions
#' @keywords fitDTVARMx fit
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
  byid <- TRUE
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
    fun = "FitDTVARIDMx",
    output = output
  )
  class(out) <- c(
    "fitdtvaridmx",
    class(out)
  )
  return(out)
}
