.FitDTVARTheta <- function(k,
                           idx,
                           observed,
                           theta_fixed = TRUE,
                           theta_values = NULL,
                           theta_lbound = NULL,
                           theta_ubound = NULL) {
  # R
  # measurement error
  if (theta_fixed) {
    return(
      .FitDTVARThetaFixed(
        k = k,
        observed = observed
      )
    )
  } else {
    return(
      .FitDTVARThetaDiag(
        k = k,
        idx = idx,
        observed = observed,
        theta_values = theta_values,
        theta_lbound = theta_lbound,
        theta_ubound = theta_ubound
      )
    )
  }
}
