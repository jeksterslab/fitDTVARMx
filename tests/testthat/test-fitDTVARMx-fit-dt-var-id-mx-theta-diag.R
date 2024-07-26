## ---- test-fitDTVARMx-fit-dt-var-id-mx-theta-diag
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 100
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    mu0 <- list(
      null_vec
    )
    sigma0 <- diag(p)
    sigma0_l <- list(
      t(chol(sigma0))
    )
    alpha <- list(
      null_vec
    )
    psi <- 0.1 * iden
    psi_l <- list(
      t(chol(psi))
    )
    beta_mu <- matrix(
      data = c(
        0.7, 0.5, -0.1,
        0.0, 0.6, 0.4,
        0, 0, 0.5
      ),
      nrow = p
    )
    beta_sigma <- 0.00001 * diag(p * p)
    beta <- simStateSpace::SimBetaN(
      n = n,
      beta = beta_mu,
      vcov_beta_vec_l = t(chol(beta_sigma))
    )
    sim <- simStateSpace::SimSSMVARIVary(
      n = n,
      time = time,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l
    )
    data <- as.data.frame(sim)
    fit <- fitDTVARMx::FitDTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      psi_diag = TRUE,
      theta_fixed = FALSE,
      ncores = NULL
    )
    print.fitdtvaridmx(fit)
    summary.fitdtvaridmx(fit)
    print.fitdtvaridmx(fit, means = FALSE, theta = TRUE)
    summary.fitdtvaridmx(fit, means = FALSE, theta = TRUE)
    coef.fitdtvaridmx(fit, psi = TRUE, theta = TRUE)
    vcov.fitdtvaridmx(fit, psi = TRUE, theta = TRUE)
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                beta_mu,
                diag(psi),
                rep(x = 0, times = p)
              ) - summary.fitdtvaridmx(fit)
            ) <= tol
          )
        )
      }
    )
    beta_ubound <- beta_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    theta_lbound <- psi_lbound <- beta_lbound
    theta_ubound <- psi_ubound <- beta_ubound
    diag(theta_lbound) <- .Machine$double.xmin
    fit <- fitDTVARMx::FitDTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      beta_start = beta_mu,
      beta_lbound = beta_lbound,
      beta_ubound = beta_ubound,
      psi_diag = TRUE,
      psi_start = psi,
      psi_lbound = psi_lbound,
      psi_ubound = psi_ubound,
      theta_fixed = FALSE,
      theta_start = 0.10 * diag(p),
      theta_lbound = theta_lbound,
      theta_ubound = theta_ubound,
      mu0_fixed = TRUE,
      mu0_start = NULL,
      mu0_lbound = NULL,
      mu0_ubound = NULL,
      sigma0_fixed = TRUE,
      sigma0_diag = TRUE,
      sigma0_start = NULL,
      sigma0_lbound = NULL,
      sigma0_ubound = NULL,
      try = 1000,
      ncores = NULL
    )
    testthat::test_that(
      paste(text, 2),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                beta_mu,
                diag(psi),
                rep(x = 0, times = p)
              ) - summary.fitdtvaridmx(fit)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-fit-dt-var-id-mx-theta-diag",
  tol = 1
)
