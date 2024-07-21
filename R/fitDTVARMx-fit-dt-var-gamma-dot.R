.FitDTVARGamma <- function(k,
                           idx,
                           alpha_fixed = TRUE,
                           alpha_start = NULL,
                           alpha_lbound = NULL,
                           alpha_ubound = NULL) {
  # B
  # latent variables on covariates
  if (alpha_fixed) {
    if (is.null(alpha_start)) {
      return(
        OpenMx::mxMatrix(
          type = "Zero",
          nrow = k,
          ncol = 1,
          name = "gamma"
        )
      )
    } else {
      return(
        OpenMx::mxMatrix(
          type = "Full",
          nrow = k,
          ncol = 1,
          free = FALSE,
          values = alpha_start,
          byrow = FALSE,
          name = "gamma"
        )
      )
    }
  } else {
    if (is.null(alpha_start)) {
      alpha_start <- matrix(
        data = 0,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(alpha_start)) {
        alpha_start <- matrix(
          data = alpha_start,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(alpha_start),
          dim(alpha_start) == c(k, 1)
        )
      }
    }
    if (is.null(alpha_lbound)) {
      alpha_lbound <- matrix(
        data = NA,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(alpha_lbound)) {
        alpha_lbound <- matrix(
          data = alpha_lbound,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(alpha_lbound),
          dim(alpha_lbound) == c(k, 1)
        )
      }
    }
    if (is.null(alpha_ubound)) {
      alpha_ubound <- matrix(
        data = NA,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(alpha_ubound)) {
        alpha_ubound <- matrix(
          data = alpha_ubound,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(alpha_ubound),
          dim(alpha_ubound) == c(k, 1)
        )
      }
    }
    return(
      OpenMx::mxMatrix(
        type = "Full",
        nrow = k,
        ncol = 1,
        free = TRUE,
        values = alpha_start,
        labels = paste0("alpha_", idx),
        lbound = alpha_lbound,
        ubound = alpha_ubound,
        byrow = FALSE,
        name = "gamma"
      )
    )
  }
}
