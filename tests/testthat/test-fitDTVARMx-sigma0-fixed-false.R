## ---- test-fitDTVARMx-sigma0-fixed-false
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    sigma0 <- fitDTVARMx:::.FitDTVARSigma0(
      k = k,
      idx = idx,
      sigma0_fixed = FALSE,
      sigma0_diag = FALSE,
      sigma0_values = matrix(
        data = 1,
        nrow = k,
        ncol = k
      ),
      sigma0_free = matrix(
        data = TRUE,
        nrow = k,
        ncol = k
      ),
      sigma0_lbound = matrix(
        data = -1,
        nrow = k,
        ncol = k
      ),
      sigma0_ubound = matrix(
        data = +1,
        nrow = k,
        ncol = k
      )
    )
    sigma0_name <- sigma0@name
    sigma0_values <- sigma0@values
    sigma0_labels <- sigma0@labels
    sigma0_free <- sigma0@free
    sigma0_lbound <- sigma0@lbound
    sigma0_ubound <- sigma0@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(sigma0) == "SymmMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          sigma0_name == "sigma0"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            sigma0_values == 1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            fitDTVARMx:::.Vech(sigma0_labels) == fitDTVARMx:::.Vech(
              outer(
                X = idx,
                Y = idx,
                FUN = function(x, y) {
                  paste0(
                    "sigma0",
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
            sigma0_free == matrix(
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
            sigma0_lbound == matrix(
              data = -1,
              nrow = k,
              ncol = k
            )
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            sigma0_ubound == matrix(
              data = +1,
              nrow = k,
              ncol = k
            )
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-sigma0-fixed-false"
)
