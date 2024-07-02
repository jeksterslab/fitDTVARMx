.FitDTVARSigma0 <- function(k) {
  # R0
  # initial condition
  # covariance
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = diag(k),
      labels = NA,
      lbound = 10,
      ubound = -10,
      byrow = FALSE,
      name = "sigma0"
    )
  )
}
