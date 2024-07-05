.FitDTVARPsi <- function(k,
                         idx,
                         statenames,
                         psi_diag = TRUE,
                         psi_start = NULL,
                         psi_lbound = NULL,
                         psi_ubound = NULL) {
  # Q
  # process noise
  if (psi_diag) {
    return(
      .FitDTVARPsiDiag(
        k = k,
        idx = idx,
        statenames = statenames,
        psi_start = psi_start,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      )
    )
  } else {
    return(
      .FitDTVARPsiFull(
        k = k,
        idx = idx,
        statenames = statenames,
        psi_start = psi_start,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      )
    )
  }
}
