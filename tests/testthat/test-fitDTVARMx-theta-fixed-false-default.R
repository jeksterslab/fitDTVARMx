## ---- test-fitDTVARMx-theta-fixed-false-default
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
      theta_fixed = FALSE,
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
          class(theta) == "DiagMatrix"
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
            theta_values == diag(
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
        theta_labels_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(theta_labels_diag) <- paste0("theta_", idx, idx)
        theta_labels_diag_vec <- na.omit(c(theta_labels_diag))
        theta_labels_vec <- na.omit(c(theta_labels))
        testthat::expect_true(
          all(
            theta_labels_vec == theta_labels_diag_vec
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(
            theta_free == matrix(
              data = as.logical(diag(k)),
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
        theta_lbound_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(theta_lbound_diag) <- .Machine$double.xmin
        theta_lbound_diag_vec <- na.omit(c(theta_lbound_diag))
        theta_lbound_vec <- na.omit(c(theta_lbound))
        testthat::expect_true(
          all(
            theta_lbound_vec == theta_lbound_diag_vec
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
  text = "test-fitDTVARMx-theta-fixed-false-default"
)
