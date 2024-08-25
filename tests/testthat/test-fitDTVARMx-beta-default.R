## ---- test-fitDTVARMx-beta-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    beta <- fitDTVARMx:::.FitDTVARBeta(
      k = k,
      idx = idx,
      statenames = statenames,
      beta_values = NULL,
      beta_free = NULL,
      beta_lbound = NULL,
      beta_ubound = NULL
    )
    beta_name <- beta@name
    beta_values <- beta@values
    beta_labels <- beta@labels
    beta_free <- beta@free
    beta_lbound <- beta@lbound
    beta_ubound <- beta@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(beta) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          beta_name == "beta"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            beta_values == diag(
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
            c(beta_labels) == c(
              outer(
                X = idx,
                Y = idx,
                FUN = function(x, y) {
                  paste0(
                    "beta",
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
          all(beta_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            is.na(beta_lbound)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(beta_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-beta-default"
)
