.FitDTVARX <- function(k,
                       alpha_fixed = TRUE) {
  # u
  # covariates
  if (alpha_fixed) {
    return(
      OpenMx::mxMatrix(
        type = "Zero",
        nrow = 1,
        ncol = 1,
        name = "covariate"
      )
    )
  } else {
    return(
      OpenMx::mxMatrix(
        type = "Unit",
        nrow = 1,
        ncol = 1,
        name = "covariate"
      )
    )
  }
}
