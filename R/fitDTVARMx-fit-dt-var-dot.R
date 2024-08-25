.FitDTVAR <- function(data,
                      observed,
                      id,
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
                      try = 1000,
                      ncores = NULL,
                      byid = TRUE,
                      ...) {
  k <- length(observed)
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  return(
    .FitDTVARRun(
      data = data,
      observed = observed,
      id = id,
      beta = .FitDTVARBeta(
        k = k,
        idx = idx,
        statenames = statenames,
        beta_values = beta_values,
        beta_free = beta_free,
        beta_lbound = beta_lbound,
        beta_ubound = beta_ubound
      ),
      gamma = .FitDTVARGamma(
        k = k,
        idx = idx,
        alpha_fixed = alpha_fixed,
        alpha_values = alpha_values,
        alpha_free = alpha_free,
        alpha_lbound = alpha_lbound,
        alpha_ubound = alpha_ubound
      ),
      lambda = .FitDTVARLambda(
        k = k,
        observed = observed,
        statenames = statenames
      ),
      kappa = .FitDTVARKappa(
        k = k
      ),
      psi = .FitDTVARPsi(
        k = k,
        idx = idx,
        statenames = statenames,
        psi_diag = psi_diag,
        psi_values = psi_values,
        psi_free = psi_free,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      ),
      theta = .FitDTVARTheta(
        k = k,
        idx = idx,
        observed = observed,
        theta_fixed = theta_fixed,
        theta_values = theta_values,
        theta_free = theta_free,
        theta_lbound = theta_lbound,
        theta_ubound = theta_ubound
      ),
      mu0 = .FitDTVARMu0(
        k = k,
        idx = idx,
        statenames = statenames,
        mu0_fixed = mu0_fixed,
        mu0_values = mu0_values,
        mu0_free = mu0_free,
        mu0_lbound = mu0_lbound,
        mu0_ubound = mu0_ubound
      ),
      sigma0 = .FitDTVARSigma0(
        k = k,
        idx = idx,
        sigma0_fixed = sigma0_fixed,
        sigma0_diag = sigma0_diag,
        sigma0_values = sigma0_values,
        sigma0_free = sigma0_free,
        sigma0_lbound = sigma0_lbound,
        sigma0_ubound = sigma0_ubound
      ),
      covariate = .FitDTVARX(
        alpha_fixed = alpha_fixed
      ),
      try = try,
      ncores = ncores,
      byid = byid,
      ...
    )
  )
}
