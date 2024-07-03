.FitDTVARPsi <- function(k,
                         psi_start = NULL,
                         psi_lbound = NULL,
                         psi_ubound = NULL,
                         psi_diag = TRUE) {
  # Q
  # process noise
  if (psi_diag) {
    return(
      .FitDTVARPsiDiag(
        k = k,
        psi_start = psi_start,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      )
    )
  } else {
    return(
      .FitDTVARPsiFull(
        k = k,
        psi_start = psi_start,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      )
    )
  }
}
