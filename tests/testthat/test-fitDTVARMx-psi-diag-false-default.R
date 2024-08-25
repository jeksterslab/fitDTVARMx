## ---- test-fitDTVARMx-psi-diag-false-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    psi <- fitDTVARMx:::.FitDTVARPsi(
      k = k,
      idx = idx,
      statenames = statenames,
      psi_diag = FALSE,
      psi_values = NULL,
      psi_free = NULL,
      psi_lbound = NULL,
      psi_ubound = NULL
    )
    psi_name <- psi@name
    psi_values <- psi@values
    psi_labels <- psi@labels
    psi_free <- psi@free
    psi_lbound <- psi@lbound
    psi_ubound <- psi@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(psi) == "SymmMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          psi_name == "psi"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            psi_values == diag(
              x = 0.1,
              nrow = k,
              ncol = k
            )
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            fitDTVARMx:::.Vech(psi_labels) == fitDTVARMx:::.Vech(
              outer(
                X = idx,
                Y = idx,
                FUN = function(x, y) {
                  paste0(
                    "psi",
                    "_",
                    x,
                    y
                  )
                }
              )
            )
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(
            psi_free == matrix(
              data = TRUE,
              nrow = k,
              ncol = k
            )
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            diag(psi_lbound) == .Machine$double.xmin
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(psi_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-psi-diag-false-default"
)
