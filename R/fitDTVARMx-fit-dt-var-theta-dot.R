.FitDTVARTheta <- function(k,
                           observed) {
  # R
  # measurement error
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = .Machine$double.xmin,
      labels = NA,
      lbound = .Machine$double.xmin,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        observed,
        observed
      ),
      name = "theta"
    )
  )
}
