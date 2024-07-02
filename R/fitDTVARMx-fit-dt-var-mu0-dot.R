.FitDTVARMu0 <- function(k) {
  # x0
  # initial condition
  # mean
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = rep(x = 0, times = k),
      labels = NA,
      lbound = 10,
      ubound = -10,
      byrow = FALSE,
      name = "mu0"
    )
  )
}
