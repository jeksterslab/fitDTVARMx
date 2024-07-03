.FitDTVARPsiFull <- function(k,
                             psi_start = NULL,
                             psi_lbound = NULL,
                             psi_ubound = NULL) {
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  # Q
  # process noise
  if (is.null(psi_start)) {
    psi_start <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(psi_start),
      dim(psi_start) == c(k, k)
    )
  }
  psi_labels <- matrix(
    data = NA,
    nrow = k,
    ncol = k
  )
  for (j in seq_len(k)) {
    for (i in seq_len(k)) {
      if (i >= j) {
        psi_labels[j, i] <- psi_labels[i, j] <- paste0(
          "psi_",
          i,
          j
        )
      }
    }
  }
  if (is.null(psi_lbound)) {
    psi_lbound <- .Machine$double.xmin * diag(k)
  } else {
    stopifnot(
      is.matrix(psi_lbound),
      dim(psi_lbound) == c(k, k)
    )
  }
  if (is.null(psi_ubound)) {
    psi_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(psi_ubound),
      dim(psi_ubound) == c(k, k)
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
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
