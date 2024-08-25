## ---- test-fitDTVARMx-alpha-fixed-false
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    alpha <- fitDTVARMx:::.FitDTVARGamma(
      k = k,
      idx = idx,
      alpha_fixed = FALSE,
      alpha_values = rep(x = 1, times = k),
      alpha_free = rep(x = TRUE, times = k),
      alpha_lbound = rep(x = -1, times = k),
      alpha_ubound = rep(x = +1, times = k)
    )
    alpha_name <- alpha@name
    alpha_values <- alpha@values
    alpha_labels <- alpha@labels
    alpha_free <- alpha@free
    alpha_lbound <- alpha@lbound
    alpha_ubound <- alpha@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(alpha) == "FullMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          alpha_name == "gamma"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            c(alpha_values) == 1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            c(alpha_labels) == paste0("alpha_", idx)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(alpha_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            c(alpha_lbound) == -1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            c(alpha_ubound) == 1
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-alpha-fixed-false"
)
