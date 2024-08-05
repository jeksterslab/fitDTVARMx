.FitDTVARPsiDiag <- function(k,
                             idx,
                             statenames,
                             psi_values = NULL,
                             psi_lbound = NULL,
                             psi_ubound = NULL) {
  # Q
  # process noise
  if (is.null(psi_values)) {
    psi_values <- rep(
      x = 0.10,
      times = k
    )
  } else {
    if (is.matrix(psi_values)) {
      psi_values <- diag(psi_values)
    }
    stopifnot(
      is.vector(psi_values),
      length(psi_values) == k
    )
  }
  psi_labels <- paste0(
    "psi_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(psi_lbound)) {
    psi_lbound <- rep(
      x = .Machine$double.xmin,
      times = k
    )
  } else {
    if (is.matrix(psi_lbound)) {
      psi_lbound <- diag(psi_lbound)
    }
    stopifnot(
      is.vector(psi_lbound),
      length(psi_lbound) == k
    )
  }
  if (is.null(psi_ubound)) {
    psi_ubound <- rep(
      x = NA,
      times = k
    )
  } else {
    if (is.matrix(psi_ubound)) {
      psi_ubound <- diag(psi_ubound)
    }
    stopifnot(
      is.vector(psi_ubound),
      length(psi_ubound) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = psi_values,
      labels = psi_labels,
      lbound = psi_lbound,
      ubound = psi_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "psi"
    )
  )
}
