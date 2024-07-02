.FitDTVARLambda <- function(k,
                            observed) {
  # C
  # measurement model factor loadings
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = 1,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        observed,
        paste0("eta", seq_len(k))
      ),
      name = "lambda"
    )
  )
}
