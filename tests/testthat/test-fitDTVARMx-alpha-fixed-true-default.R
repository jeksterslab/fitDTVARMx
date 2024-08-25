## ---- test-fitDTVARMx-alpha-fixed-true-default
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
      alpha_fixed = TRUE,
      alpha_values = NULL,
      alpha_free = NULL,
      alpha_lbound = NULL,
      alpha_ubound = NULL
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
          class(alpha) == "ZeroMatrix"
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
            c(alpha_values) == 0
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            is.na(alpha_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          !all(alpha_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            is.na(alpha_lbound)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(alpha_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-alpha-fixed-true-default"
)
