.FitDTVARModel <- function(data,
                           observed,
                           beta,
                           gamma,
                           lambda,
                           kappa,
                           psi,
                           theta,
                           mu0,
                           sigma0,
                           covariate) {
  return(
    OpenMx::mxModel(
      model = "DTVAR",
      beta,
      gamma,
      lambda,
      kappa,
      psi,
      theta,
      mu0,
      sigma0,
      covariate,
      OpenMx::mxExpectationStateSpace(
        A = "beta",
        B = "gamma",
        C = "lambda",
        D = "kappa",
        Q = "psi",
        R = "theta",
        x0 = "mu0",
        P0 = "sigma0",
        u = "covariate",
        dimnames = observed
      ),
      OpenMx::mxFitFunctionML(),
      OpenMx::mxData(
        observed = data,
        type = "raw"
      )
    )
  )
}
