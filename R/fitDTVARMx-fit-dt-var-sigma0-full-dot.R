.FitDTVARSigma0Full <- function(k,
                                idx,
                                sigma0_values = NULL,
                                sigma0_free = NULL,
                                sigma0_lbound = NULL,
                                sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  if (is.null(sigma0_values)) {
    sigma0_values <- diag(k)
  } else {
    stopifnot(
      is.matrix(sigma0_values),
      dim(sigma0_values) == c(k, k)
    )
  }
  sigma0_labels <- outer(
    X = idx,
    Y = idx,
    FUN = function(x, y) {
      paste0(
        "sigma0",
        "_",
        x,
        y
      )
    }
  )
  if (is.null(sigma0_lbound)) {
    sigma0_lbound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
    diag(sigma0_lbound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(sigma0_lbound),
      dim(sigma0_lbound) == c(k, k)
    )
  }
  if (is.null(sigma0_ubound)) {
    sigma0_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(sigma0_ubound),
      dim(sigma0_ubound) == c(k, k)
    )
  }
  if (is.null(sigma0_free)) {
    sigma0_free <- matrix(
      data = TRUE,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(sigma0_free),
      dim(sigma0_free) == c(k, k)
    )
    for (i in idx) {
      for (j in idx) {
        if (!sigma0_free[i, j]) {
          sigma0_labels[i, j] <- NA
          sigma0_lbound[i, j] <- NA
          sigma0_ubound[i, j] <- NA
        }
      }
    }
  }
  # make sure matrices are symmetric
  sigma0_free[
    upper.tri(sigma0_free)
  ] <- t(sigma0_free)[
    upper.tri(sigma0_free)
  ]
  sigma0_values[
    upper.tri(sigma0_values)
  ] <- t(sigma0_values)[
    upper.tri(sigma0_values)
  ]
  sigma0_labels[
    upper.tri(sigma0_labels)
  ] <- t(sigma0_labels)[
    upper.tri(sigma0_labels)
  ]
  sigma0_lbound[
    upper.tri(sigma0_lbound)
  ] <- t(sigma0_lbound)[
    upper.tri(sigma0_lbound)
  ]
  sigma0_ubound[
    upper.tri(sigma0_ubound)
  ] <- t(sigma0_ubound)[
    upper.tri(sigma0_ubound)
  ]
  return(
    OpenMx::mxMatrix(
      type = "Symm",
      nrow = k,
      ncol = k,
      free = sigma0_free,
      values = sigma0_values,
      labels = sigma0_labels,
      lbound = sigma0_lbound,
      ubound = sigma0_ubound,
      byrow = FALSE,
      name = "sigma0"
    )
  )
}
