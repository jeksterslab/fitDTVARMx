## ---- test-psi-full
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 5
    time <- 1000
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
    beta_sigma <- 0.001 * diag(p * p)
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
    fit <- FitDTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      psi_diag = FALSE,
      ncores = NULL
    )
    print(fit)
    summary(fit)
    print(fit, means = FALSE)
    summary(fit, means = FALSE)
    coef(fit, psi = TRUE)
    vcov(fit, psi = TRUE)
    testthat::test_that(
      text,
      {
        testthat::expect_true(
          all(
            abs(
              c(
                c(beta_mu),
                psi[
                  lower.tri(
                    x = psi,
                    diag = TRUE
                  )
                ]
              ) - summary(fit)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-psi-full",
  tol = 0.1
)
