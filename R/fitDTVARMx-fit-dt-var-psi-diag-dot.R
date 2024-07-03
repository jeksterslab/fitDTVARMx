.FitDTVARPsiDiag <- function(k,
                             psi_start = NULL,
                             psi_lbound = NULL,
                             psi_ubound = NULL) {
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  # Q
  # process noise
  if (is.null(psi_start)) {
    psi_start <- rep(
      x = 0.10,
      times = k
    )
  } else {
    if (is.matrix(psi_start)) {
      psi_start <- diag(psi_start)
    }
    stopifnot(
      is.vector(psi_start),
      length(psi_start) == k
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
      values = psi_start,
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
