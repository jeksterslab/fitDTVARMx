.FitDTVARPsi <- function(k,
                         idx,
                         statenames,
                         psi_diag = TRUE,
                         psi_values = NULL,
                         psi_free = NULL,
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
        psi_values = psi_values,
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
        psi_values = psi_values,
        psi_free = psi_free,
        psi_lbound = psi_lbound,
        psi_ubound = psi_ubound
      )
    )
  }
}
