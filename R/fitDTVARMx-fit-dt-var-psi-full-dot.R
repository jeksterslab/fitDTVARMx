.FitDTVARPsiFull <- function(k,
                             idx,
                             statenames,
                             psi_values = NULL,
                             psi_free = NULL,
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
  psi_labels <- outer(
    X = idx,
    Y = idx,
    FUN = function(x, y) {
      paste0(
        "psi",
        "_",
        x,
        y
      )
    }
  )
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
  if (is.null(psi_free)) {
    psi_free <- matrix(
      data = TRUE,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(psi_free),
      dim(psi_free) == c(k, k)
    )
    for (i in idx) {
      for (j in idx) {
        if (!psi_free[i, j]) {
          psi_labels[i, j] <- NA
          psi_lbound[i, j] <- NA
          psi_ubound[i, j] <- NA
        }
      }
    }
  }
  # make sure matrices are symmetric
  psi_free[
    upper.tri(psi_free)
  ] <- t(psi_free)[
    upper.tri(psi_free)
  ]
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
      free = psi_free,
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
