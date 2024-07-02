.FitDTVARBeta <- function(k,
                          statenames,
                          beta_start = NULL,
                          beta_lbound = NULL,
                          beta_ubound = NULL) {
  idx <- seq_len(k)
  # A
  # auto regression and cross regression coefficients
  if (is.null(beta_start)) {
    beta_start <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(beta_start),
      dim(beta_start) == c(k, k)
    )
  }
  beta_labels <- matrix(
    data = "0",
    nrow = k,
    ncol = k
  )
  for (i in idx) {
    for (j in idx) {
      beta_labels[i, j] <- paste0(
        "beta",
        "_",
        idx[i],
        idx[j]
      )
    }
  }
  if (is.null(beta_lbound)) {
    beta_lbound <- matrix(
      data = -10,
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
      data = 10,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(beta_ubound),
      dim(beta_ubound) == c(k, k)
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = beta_start,
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
