.FitDTVARPsiFull <- function(k,
                             idx,
                             statenames,
                             psi_values = NULL,
                             psi_lbound = NULL,
                             psi_ubound = NULL) {
  # Q
  # process noise
  if (is.null(psi_values)) {
    psi_values <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(psi_values),
      dim(psi_values) == c(k, k)
    )
  }
  psi_labels <- matrix(
    data = NA,
    nrow = k,
    ncol = k
  )
  for (j in idx) {
    for (i in idx) {
      psi_labels[i, j] <- paste0(
        "psi_",
        i,
        j
      )
    }
  }
  if (is.null(psi_lbound)) {
    psi_lbound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
    diag(psi_lbound) <- .Machine$double.xmin
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
  # make sure matrices are symmetric
  psi_values[
    upper.tri(psi_values)
  ] <- t(psi_values)[
    upper.tri(psi_values)
  ]
  psi_labels[
    upper.tri(psi_labels)
  ] <- t(psi_labels)[
    upper.tri(psi_labels)
  ]
  psi_lbound[
    upper.tri(psi_lbound)
  ] <- t(psi_lbound)[
    upper.tri(psi_lbound)
  ]
  psi_ubound[
    upper.tri(psi_ubound)
  ] <- t(psi_ubound)[
    upper.tri(psi_ubound)
  ]
  return(
    OpenMx::mxMatrix(
      type = "Symm",
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
