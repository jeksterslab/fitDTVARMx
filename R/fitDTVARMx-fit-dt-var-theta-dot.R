.FitDTVARTheta <- function(k,
                           observed,
                           theta = FALSE,
                           theta_start = NULL,
                           theta_lbound = NULL,
                           theta_ubound = NULL) {
  # R
  # measurement error
  if (theta) {
    return(
      .FitDTVARThetaDiag(
        k = k,
        observed = observed,
        theta_start = theta_start,
        theta_lbound = theta_lbound,
        theta_ubound = theta_ubound
      )
    )
  } else {
    return(
      .FitDTVARThetaNull(
        k = k,
        observed = observed
      )
    )
  }
}
