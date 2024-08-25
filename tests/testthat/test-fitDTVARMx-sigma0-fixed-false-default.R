## ---- test-fitDTVARMx-sigma0-fixed-false-default
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
      sigma0_diag = TRUE,
      sigma0_values = NULL,
      sigma0_free = NULL,
      sigma0_lbound = NULL,
      sigma0_ubound = NULL
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
          class(sigma0) == "DiagMatrix"
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
            sigma0_values == diag(k)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "labels"),
      {
        sigma0_labels_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(sigma0_labels_diag) <- paste0("sigma0_", idx, idx)
        sigma0_labels_diag_vec <- na.omit(c(sigma0_labels_diag))
        sigma0_labels_vec <- na.omit(c(sigma0_labels))
        testthat::expect_true(
          all(
            sigma0_labels_vec == sigma0_labels_diag_vec
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
        sigma0_lbound_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(sigma0_lbound_diag) <- .Machine$double.xmin
        sigma0_lbound_diag_vec <- na.omit(c(sigma0_lbound_diag))
        sigma0_lbound_vec <- na.omit(c(sigma0_lbound))
        testthat::expect_true(
          all(
            sigma0_lbound_vec == sigma0_lbound_diag_vec
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(sigma0_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-sigma0-fixed-false-default"
)
