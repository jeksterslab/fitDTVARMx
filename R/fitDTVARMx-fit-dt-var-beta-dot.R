.FitDTVARBeta <- function(k,
                          idx,
                          statenames,
                          beta_values = NULL,
                          beta_free = NULL,
                          beta_lbound = NULL,
                          beta_ubound = NULL) {
  # A
  # auto regression and cross regression coefficients
  if (is.null(beta_values)) {
    beta_values <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(beta_values),
      dim(beta_values) == c(k, k)
    )
  }
  beta_labels <- outer(
    X = idx,
    Y = idx,
    FUN = function(x, y) {
      paste0(
        "beta",
        "_",
        x,
        y
      )
    }
  )
  if (is.null(beta_lbound)) {
    beta_lbound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(beta_lbound),
      dim(beta_lbound) == c(k, k)
    )
  }
  if (is.null(beta_ubound)) {
    beta_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(beta_ubound),
      dim(beta_ubound) == c(k, k)
    )
  }
  if (is.null(beta_free)) {
    beta_free <- matrix(
      data = TRUE,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(beta_free),
      dim(beta_free) == c(k, k)
    )
    idx <- seq_len(k)
    for (i in idx) {
      for (j in idx) {
        if (!beta_free[i, j]) {
          beta_labels[i, j] <- NA
          beta_lbound[i, j] <- NA
          beta_ubound[i, j] <- NA
        }
      }
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = k,
      free = beta_free,
      values = beta_values,
      labels = beta_labels,
      lbound = beta_lbound,
      ubound = beta_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "beta"
    )
  )
}
