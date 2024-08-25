## ---- test-fitDTVARMx-mu0-fixed-false
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    mu0 <- fitDTVARMx:::.FitDTVARMu0(
      k = k,
      idx = idx,
      statenames = statenames,
      mu0_fixed = FALSE,
      mu0_values = rep(x = 1, times = k),
      mu0_free = rep(x = TRUE, times = k),
      mu0_lbound = rep(x = -1, times = k),
      mu0_ubound = rep(x = +1, times = k)
    )
    mu0_name <- mu0@name
    mu0_values <- mu0@values
    mu0_labels <- mu0@labels
    mu0_free <- mu0@free
    mu0_lbound <- mu0@lbound
    mu0_ubound <- mu0@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(mu0) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          mu0_name == "mu0"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            c(mu0_values) == 1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            c(mu0_labels) == paste0("mu0_", idx)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(mu0_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            c(mu0_lbound) == -1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            c(mu0_ubound) == 1
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-mu0-fixed-false"
)
