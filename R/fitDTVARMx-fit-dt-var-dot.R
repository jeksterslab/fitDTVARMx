.FitDTVAR <- function(data,
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
  k <- length(observed)
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  ids <- sort(
    unique(data[, id])
  )
  beta <- .FitDTVARBeta(
    k = k,
    idx = idx,
    statenames = statenames,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound
  )
  gamma <- .FitDTVARGamma(k = k)
  lambda <- .FitDTVARLambda(
    k = k,
    observed = observed,
    statenames = statenames
  )
  kappa <- .FitDTVARKappa(k = k)
  psi <- .FitDTVARPsi(
    k = k,
    idx = idx,
    statenames = statenames,
    psi_diag = psi_diag,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound
  )
  theta <- .FitDTVARTheta(
    k = k,
    idx = idx,
    observed = observed,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound
  )
  mu0 <- .FitDTVARMu0(
    k = k,
    idx = idx,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound
  )
  sigma0 <- .FitDTVARSigma0(
    k = k,
    idx = idx,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound
  )
  x <- .FitDTVARX()
  par <- FALSE
  # nocov start
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  # nocov end
  expectation <- OpenMx::mxExpectationStateSpace(
    A = "beta",
    B = "gamma",
    C = "lambda",
    D = "kappa",
    Q = "psi",
    R = "theta",
    x0 = "mu0",
    P0 = "sigma0",
    u = "x",
    dimnames = observed
  )
  if (par) {
    # nocov start
    OpenMx::mxOption(
      key = "Number of Threads",
      value = 1
    )
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- parallel::parLapply(
      cl = cl,
      X = ids,
      fun = function(i) {
        model <- OpenMx::mxModel(
          model = "DTVAR",
          beta,
          gamma,
          lambda,
          kappa,
          psi,
          theta,
          mu0,
          sigma0,
          x,
          expectation,
          OpenMx::mxFitFunctionML(),
          OpenMx::mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        OpenMx::mxTryHardctsem(
          model = model,
          extraTries = try
        )
      }
    )
    # nocov end
  } else {
    output <- lapply(
      X = ids,
      FUN = function(i) {
        model <- OpenMx::mxModel(
          model = "DTVAR",
          beta,
          gamma,
          lambda,
          kappa,
          psi,
          theta,
          mu0,
          sigma0,
          x,
          expectation,
          OpenMx::mxFitFunctionML(),
          OpenMx::mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        OpenMx::mxTryHardctsem(
          model = model,
          extraTries = try
        )
      }
    )
  }
  return(output)
}
