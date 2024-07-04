.FitDTVARThetaDiag <- function(k,
                               observed,
                               theta_start = NULL,
                               theta_lbound = NULL,
                               theta_ubound = NULL) {
  # R
  # measurement error
  if (is.null(theta_start)) {
    theta_start <- rep(
      x = 0.10,
      times = k
    )
  }
  theta_labels <- paste0(
    "theta_",
    seq_len(k),
    seq_len(k)
  )
  if (is.null(theta_lbound)) {
    theta_lbound <- rep(
      x = .Machine$double.xmin,
      times = k
    )
  }
  if (is.null(theta_ubound)) {
    theta_ubound <- rep(
      x = NA,
      times = k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = theta_start,
      labels = theta_labels,
      lbound = theta_lbound,
      ubound = theta_ubound,
      byrow = FALSE,
      dimnames = list(
        observed,
        observed
      ),
      name = "theta"
    )
  )
}
