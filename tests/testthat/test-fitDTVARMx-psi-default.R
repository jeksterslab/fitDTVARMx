## ---- test-fitDTVARMx-psi-default
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    k <- 3
    idx <- seq_len(k)
    statenames <- paste0("eta", idx)
    psi <- fitDTVARMx:::.FitDTVARPsi(
      k = k,
      idx = idx,
      statenames = statenames,
      psi_diag = TRUE,
      psi_values = NULL,
      psi_free = NULL,
      psi_lbound = NULL,
      psi_ubound = NULL
    )
    psi_name <- psi@name
    psi_values <- psi@values
    psi_labels <- psi@labels
    psi_free <- psi@free
    psi_lbound <- psi@lbound
    psi_ubound <- psi@ubound
    testthat::test_that(
      paste(text, "class"),
      {
        testthat::expect_true(
          class(psi) == "DiagMatrix"
        )
      }
    )
    testthat::test_that(
      paste(text, "name"),
      {
        testthat::expect_true(
          psi_name == "psi"
        )
      }
    )
    testthat::test_that(
      paste(text, "values"),
      {
        testthat::expect_true(
          all(
            psi_values == diag(
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
        psi_labels_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(psi_labels_diag) <- paste0("psi_", idx, idx)
        psi_labels_diag_vec <- na.omit(c(psi_labels_diag))
        psi_labels_vec <- na.omit(c(psi_labels))
        testthat::expect_true(
          all(
            psi_labels_vec == psi_labels_diag_vec
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "free"),
      {
        testthat::expect_true(
          all(
            psi_free == matrix(
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
        psi_lbound_diag <- matrix(
          data = NA,
          nrow = k,
          ncol = k
        )
        diag(psi_lbound_diag) <- .Machine$double.xmin
        psi_lbound_diag_vec <- na.omit(c(psi_lbound_diag))
        psi_lbound_vec <- na.omit(c(psi_lbound))
        testthat::expect_true(
          all(
            psi_lbound_vec == psi_lbound_diag_vec
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ubound"),
      {
        testthat::expect_true(
          all(
            is.na(psi_ubound)
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-psi-default"
)
