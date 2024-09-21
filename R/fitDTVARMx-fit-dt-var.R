#' Fit the First-Order Discrete-Time Vector Autoregressive Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details Note that the mean and covariance matrix
#' of the initial condition are fixed to a null vector
#' and an identity matrix, resprectively.
#' The [DTVAR()] function fits four versions of the
#' first-order discrete-time vector autoregressive model.
#' Use the [FitDTVARIDMx()] or [FitDTVARMx()] functions
#' to have more control over the model specification.
#' ## Model 1
#' The measurement model is given by
#' \deqn{
#'   \mathbf{y}_{i, t}
#'   =
#'   \boldsymbol{\eta}_{i, t}
#' }
#' where \eqn{\mathbf{y}_{i, t}}
#' represents a vector of observed variables
#' and \eqn{\boldsymbol{\eta}_{i, t}}
#' a vector of latent variables
#' for individual \eqn{i} and time \eqn{t}.
#'
#' The dynamic structure is given by
#' \deqn{
#'   \boldsymbol{\eta}_{i, t}
#'   =
#'   \boldsymbol{\beta}
#'   \boldsymbol{\eta}_{i, t - 1}
#'   +
#'   \boldsymbol{\zeta}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\zeta}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Psi}
#'   \right)
#' }
#' where
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' are random variables,
#' and
#' \eqn{\boldsymbol{\beta}},
#' and
#' \eqn{\boldsymbol{\Psi}}
#' are model parameters.
#' Here,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' is a vector of latent variables
#' at time \eqn{t} and individual \eqn{i},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}}
#' represents a vector of latent variables
#' at time \eqn{t - 1} and individual \eqn{i},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' represents a vector of dynamic noise
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\beta}}
#' denotes a matrix of autoregression
#' and cross regression coefficients,
#' and
#' \eqn{\boldsymbol{\Psi}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\zeta}_{i, t}}.
#' In this model,
#' \eqn{\boldsymbol{\Psi}} is a diagonal matrix.
#'
#' ## Model 2
#' The measurement model is given by
#' \deqn{
#'   \mathbf{y}_{i, t}
#'   =
#'   \boldsymbol{\Lambda}
#'   \boldsymbol{\eta}_{i, t}
#'   +
#'   \boldsymbol{\varepsilon}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\varepsilon}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Theta}
#'   \right)
#' }
#' where
#' \eqn{\mathbf{y}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' are random variables
#' and
#' \eqn{\boldsymbol{\Lambda}},
#' and
#' \eqn{\boldsymbol{\Theta}}
#' are model parameters.
#' \eqn{\mathbf{y}_{i, t}}
#' represents a vector of observed random variables,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' a vector of latent random variables,
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' a vector of random measurement errors,
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\Lambda}}
#' denotes a matrix of factor loadings,
#' and
#' \eqn{\boldsymbol{\Theta}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\varepsilon}}.
#' In this model,
#' \eqn{\boldsymbol{\Lambda}} is an identity matrix and
#' \eqn{\boldsymbol{\Theta}} is a diagonal matrix.
#'
#' The dynamic structure is given by
#' \deqn{
#'   \boldsymbol{\eta}_{i, t}
#'   =
#'   \boldsymbol{\beta}
#'   \boldsymbol{\eta}_{i, t - 1}
#'   +
#'   \boldsymbol{\zeta}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\zeta}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Psi}
#'   \right)
#' }
#' where
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' are random variables,
#' and
#' \eqn{\boldsymbol{\beta}},
#' and
#' \eqn{\boldsymbol{\Psi}}
#' are model parameters.
#' Here,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' is a vector of latent variables
#' at time \eqn{t} and individual \eqn{i},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}}
#' represents a vector of latent variables
#' at time \eqn{t - 1} and individual \eqn{i},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' represents a vector of dynamic noise
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\beta}}
#' denotes a matrix of autoregression
#' and cross regression coefficients,
#' and
#' \eqn{\boldsymbol{\Psi}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\zeta}_{i, t}}.
#' In this model,
#' \eqn{\boldsymbol{\Psi}} is a diagonal matrix.
#'
#' ## Model 3
#' The measurement model is given by
#' \deqn{
#'   \mathbf{y}_{i, t}
#'   =
#'   \boldsymbol{\Lambda}
#'   \boldsymbol{\eta}_{i, t}
#'   +
#'   \boldsymbol{\varepsilon}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\varepsilon}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Theta}
#'   \right)
#' }
#' where
#' \eqn{\mathbf{y}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' are random variables
#' and
#' \eqn{\boldsymbol{\Lambda}},
#' and
#' \eqn{\boldsymbol{\Theta}}
#' are model parameters.
#' \eqn{\mathbf{y}_{i, t}}
#' represents a vector of observed random variables,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' a vector of latent random variables,
#' and
#' \eqn{\boldsymbol{\varepsilon}_{i, t}}
#' a vector of random measurement errors,
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\Lambda}}
#' denotes a matrix of factor loadings,
#' and
#' \eqn{\boldsymbol{\Theta}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\varepsilon}}.
#' In this model,
#' \eqn{\boldsymbol{\Lambda}} is an identity matrix and
#' \eqn{\boldsymbol{\Theta}} is a diagonal matrix.
#'
#' The dynamic structure is given by
#' \deqn{
#'   \boldsymbol{\eta}_{i, t}
#'   =
#'   \boldsymbol{\alpha}
#'   +
#'   \boldsymbol{\beta}
#'   \boldsymbol{\eta}_{i, t - 1}
#'   +
#'   \boldsymbol{\zeta}_{i, t},
#'   \quad
#'   \mathrm{with}
#'   \quad
#'   \boldsymbol{\zeta}_{i, t}
#'   \sim
#'   \mathcal{N}
#'   \left(
#'   \mathbf{0},
#'   \boldsymbol{\Psi}
#'   \right)
#' }
#' where
#' \eqn{\boldsymbol{\eta}_{i, t}},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' are random variables,
#' and
#' \eqn{\boldsymbol{\alpha}},
#' \eqn{\boldsymbol{\beta}},
#' and
#' \eqn{\boldsymbol{\Psi}}
#' are model parameters.
#' Here,
#' \eqn{\boldsymbol{\eta}_{i, t}}
#' is a vector of latent variables
#' at time \eqn{t} and individual \eqn{i},
#' \eqn{\boldsymbol{\eta}_{i, t - 1}}
#' represents a vector of latent variables
#' at time \eqn{t - 1} and individual \eqn{i},
#' and
#' \eqn{\boldsymbol{\zeta}_{i, t}}
#' represents a vector of dynamic noise
#' at time \eqn{t} and individual \eqn{i}.
#' \eqn{\boldsymbol{\alpha}}
#' denotes a vector of intercepts,
#' \eqn{\boldsymbol{\beta}}
#' a matrix of autoregression
#' and cross regression coefficients,
#' and
#' \eqn{\boldsymbol{\Psi}}
#' the covariance matrix of
#' \eqn{\boldsymbol{\zeta}_{i, t}}.
#' In this model,
#' \eqn{\boldsymbol{\Psi}} is a diagonal matrix.
#'
#' ## Model 4
#' Model 4 is similar to Model 3 except that
#' \eqn{\boldsymbol{\Psi}} is a symmetric matrix in Model 4.
#'
#' @inheritParams FitDTVARIDMx
#' @param model Model number (1, 2, 3, or 4).
#'   See Details for model description.
#' @param byid Logical.
#'   If `byid = TRUE`,
#'   fit the model by `id`.
#'
#' @return Returns an object of class
#'   `dtvaridmx` if `byid = TRUE` or
#'   `dtvarmx` if `byid = FALSE`.
#'   The returned object is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fun}{Function used ("FitDTVARIDMx" if `byid = TRUE` or
#'     "FitDTVARMx" if `byid = FALSE`).}
#'     \item{output}{A list of fitted OpenMx models `byid = TRUE` or
#'     a single fitted OpenMx model if `byid = FALSE`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # ---------------------------------------------------------------------------
#' # byid = FALSE
#' # ---------------------------------------------------------------------------
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
#' fit <- DTVAR(
#'   data = data,
#'   observed = c("y1", "y2", "y3"),
#'   id = "id",
#'   byid = FALSE
#' )
#' print(fit)
#' summary(fit)
#' coef(fit)
#' vcov(fit)
#'
#' # ---------------------------------------------------------------------------
#' # byid = TRUE
#' # ---------------------------------------------------------------------------
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
#' fit <- DTVAR(
#'   data = data,
#'   observed = c("y1", "y2", "y3"),
#'   id = "id",
#'   byid = TRUE
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
#' @export
DTVAR <- function(data,
                  observed,
                  id,
                  byid = FALSE,
                  model = 1,
                  try = 1000,
                  ncores = NULL,
                  ...) {
  stopifnot(
    model %in% 1:4
  )
  if (model == 1) {
    if (byid) {
      return(
        FitDTVARIDMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = TRUE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = TRUE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    } else {
      return(
        FitDTVARMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = TRUE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = TRUE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    }
  }
  if (model == 2) {
    if (byid) {
      return(
        FitDTVARIDMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = TRUE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    } else {
      return(
        FitDTVARMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = TRUE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    }
  }
  if (model == 3) {
    if (byid) {
      return(
        FitDTVARIDMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = FALSE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    } else {
      return(
        FitDTVARMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = FALSE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = TRUE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    }
  }
  if (model == 4) {
    if (byid) {
      return(
        FitDTVARIDMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = FALSE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = FALSE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    } else {
      return(
        FitDTVARMx(
          data = data,
          observed = observed,
          id = id,
          alpha_fixed = FALSE,
          alpha_values = NULL,
          alpha_free = NULL,
          alpha_lbound = NULL,
          alpha_ubound = NULL,
          beta_values = NULL,
          beta_free = NULL,
          beta_lbound = NULL,
          beta_ubound = NULL,
          psi_diag = FALSE,
          psi_values = NULL,
          psi_free = NULL,
          psi_lbound = NULL,
          psi_ubound = NULL,
          theta_fixed = FALSE,
          theta_values = NULL,
          theta_free = NULL,
          theta_lbound = NULL,
          theta_ubound = NULL,
          mu0_fixed = TRUE,
          mu0_values = NULL,
          mu0_free = NULL,
          mu0_lbound = NULL,
          mu0_ubound = NULL,
          sigma0_fixed = TRUE,
          sigma0_diag = TRUE,
          sigma0_values = NULL,
          sigma0_free = NULL,
          sigma0_lbound = NULL,
          sigma0_ubound = NULL,
          try = try,
          ncores = ncores,
          ...
        )
      )
    }
  }
}
