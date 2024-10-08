.FitDTVARSigma0 <- function(k,
                            idx,
                            sigma0_fixed = TRUE,
                            sigma0_diag = TRUE,
                            sigma0_values = NULL,
                            sigma0_free = NULL,
                            sigma0_lbound = NULL,
                            sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  if (sigma0_fixed) {
    return(
      .FitDTVARSigma0Fixed(
        k = k,
        sigma0_diag = sigma0_diag,
        sigma0_values = sigma0_values
      )
    )
  } else {
    if (sigma0_diag) {
      return(
        .FitDTVARSigma0Diag(
          k = k,
          idx = idx,
          sigma0_values = sigma0_values,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    } else {
      return(
        .FitDTVARSigma0Full(
          k = k,
          idx = idx,
          sigma0_values = sigma0_values,
          sigma0_free = sigma0_free,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    }
  }
}
