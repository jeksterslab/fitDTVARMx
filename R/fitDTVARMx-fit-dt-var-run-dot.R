.FitDTVARRun <- function(data,
                         observed,
                         id,
                         beta,
                         gamma,
                         lambda,
                         kappa,
                         psi,
                         theta,
                         mu0,
                         sigma0,
                         covariate,
                         try = 1000,
                         ncores = NULL,
                         byid = TRUE) {
  if (byid) {
    return(
      .FitDTVARRunID(
        data = data,
        observed = observed,
        id = id,
        beta = beta,
        gamma = gamma,
        lambda = lambda,
        kappa = kappa,
        psi = psi,
        theta = theta,
        mu0 = mu0,
        sigma0 = sigma0,
        covariate = covariate,
        try = try,
        ncores = ncores
      )
    )
  } else {
    return(
      .FitDTVARRunMultiGroup(
        data = data,
        observed = observed,
        id = id,
        beta = beta,
        gamma = gamma,
        lambda = lambda,
        kappa = kappa,
        psi = psi,
        theta = theta,
        mu0 = mu0,
        sigma0 = sigma0,
        covariate = covariate,
        try = try,
        ncores = ncores
      )
    )
  }
}
