## ---- test-fitDTVARMx-theta-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    observed <- paste0("y", idx)
    theta <- fitDTVARMx:::.FitDTVARTheta(
      k = k,
      idx = idx,
      observed = observed,
      theta_fixed = TRUE,
      theta_values = NULL,
      theta_free = NULL,
      theta_lbound = NULL,
      theta_ubound = NULL
    )
    theta_name <- theta@name
    theta_values <- theta@values
    theta_labels <- theta@labels
    theta_free <- theta@free
    theta_lbound <- theta@lbound
    theta_ubound <- theta@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(theta) == "ZeroMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          theta_name == "theta"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            theta_values == matrix(
              data = 0,
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
            is.na(theta_labels)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          !all(
            theta_free
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "lbound"),
      {
        testthat::expect_true(
          all(
            is.na(theta_lbound)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(theta_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-theta-default"
)
