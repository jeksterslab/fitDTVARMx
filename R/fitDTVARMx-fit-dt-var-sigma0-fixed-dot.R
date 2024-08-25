.FitDTVARSigma0Fixed <- function(k,
                                 sigma0_diag = TRUE,
                                 sigma0_values = NULL) {
  # sigma0_values will be the fixed value
  # R0
  # initial condition
  # covariance
  if (sigma0_diag) {
    return(
      .FitDTVARSigma0FixedDiag(
        k = k,
        sigma0_values = sigma0_values
      )
    )
  } else {
    return(
      .FitDTVARSigma0FixedFull(
        k = k,
        sigma0_values = sigma0_values
      )
    )
  }
}
