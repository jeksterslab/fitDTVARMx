## ---- test-fitDTVARMx-beta
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
      beta_values = matrix(
        data = 1,
        nrow = k,
        ncol = k
      ),
      beta_free = diag(
        x = TRUE,
        nrow = k,
        ncol = k
      ),
      beta_lbound = matrix(
        data = -1,
        nrow = k,
        ncol = k
      ),
      beta_ubound = matrix(
        data = +1,
        nrow = k,
        ncol = k
      )
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
            beta_values == matrix(
              data = 1,
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
        beta_labels_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(beta_labels_diag) <- paste0("beta_", idx, idx)
        beta_labels_diag_vec <- na.omit(c(beta_labels_diag))
        beta_labels_vec <- na.omit(c(beta_labels))
        testthat::expect_true(
          all(
            beta_labels_vec == beta_labels_diag_vec
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(
            beta_free == matrix(
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
        testthat::expect_true(
          all(
            diag(beta_lbound) == -1
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            diag(beta_ubound) == +1
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-beta"
)
