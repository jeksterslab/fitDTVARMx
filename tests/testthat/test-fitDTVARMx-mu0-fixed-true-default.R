## ---- test-fitDTVARMx-mu0-fixed-true-default
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
      mu0_fixed = TRUE,
      mu0_values = NULL,
      mu0_free = NULL,
      mu0_lbound = NULL,
      mu0_ubound = NULL
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
            c(mu0_values) == 0
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        testthat::expect_true(
          all(
            is.na(mu0_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          !all(mu0_free)
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            is.na(mu0_lbound)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(mu0_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-mu0-fixed-true-default"
)
