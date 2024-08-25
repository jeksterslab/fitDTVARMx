.FitDTVARMu0Vec <- function(k,
                            idx,
                            statenames,
                            mu0_values = NULL,
                            mu0_free = NULL,
                            mu0_lbound = NULL,
                            mu0_ubound = NULL) {
  # x0
  # initial condition
  # mean
  if (is.null(mu0_values)) {
    mu0_values <- matrix(
      data = 0,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_values)) {
      mu0_values <- matrix(
        data = mu0_values,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_values),
        dim(mu0_values) == c(k, 1)
      )
    }
  }
  if (is.null(mu0_lbound)) {
    mu0_lbound <- matrix(
      data = -1,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_lbound)) {
      mu0_lbound <- matrix(
        data = mu0_lbound,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_lbound),
        dim(mu0_lbound) == c(k, 1)
      )
    }
  }
  if (is.null(mu0_ubound)) {
    mu0_ubound <- matrix(
      data = +1,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_ubound)) {
      mu0_ubound <- matrix(
        data = mu0_ubound,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_ubound),
        dim(mu0_ubound) == c(k, 1)
      )
    }
  }
  mu0_labels <- matrix(
    data = paste0(
      "mu0_",
      idx
    ),
    ncol = 1
  )
  if (is.null(mu0_free)) {
    mu0_free <- matrix(
      data = TRUE,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_free)) {
      mu0_free <- matrix(
        data = mu0_free,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(mu0_free),
      dim(mu0_free) == c(k, 1)
    )
    for (i in idx) {
      if (!mu0_free[i, 1]) {
        mu0_labels[i, 1] <- NA
        mu0_lbound[i, 1] <- NA
        mu0_ubound[i, 1] <- NA
      }
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = mu0_free,
      values = mu0_values,
      labels = mu0_labels,
      lbound = mu0_lbound,
      ubound = mu0_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        "mu0"
      ),
      name = "mu0"
    )
  )
}
