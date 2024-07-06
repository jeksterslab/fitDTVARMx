## ---- test-fitDTVARMx-fit-dt-var-mx-theta-null
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 500
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    mu0 <- null_vec
    sigma0 <- diag(p)
    sigma0_l <- t(chol(sigma0))
    alpha <- null_vec
    psi <- 0.1 * iden
    psi_l <- t(chol(psi))
    beta <- matrix(
      data = c(
        0.7, 0.5, -0.1,
        0.0, 0.6, 0.4,
        0, 0, 0.5
      ),
      nrow = p
    )
    sim <- simStateSpace::SimSSMVARFixed(
      n = n,
      time = time,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l
    )
    data <- as.data.frame(sim)
    fit <- FitDTVARMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      psi_diag = TRUE,
      ncores = NULL
    )
    print(fit)
    summary(fit)
    print(fit, means = FALSE)
    summary(fit, means = FALSE)
    coef(fit, psi = TRUE)
    vcov(fit, psi = TRUE)
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              round(
                c(
                  beta,
                  diag(psi)
                ),
                digits = 1
              ) - round(
                coef(fit, psi = TRUE),
                digits = 1
              )
            ) <= tol
          )
        )
      }
    )
    psi_ubound <- psi_lbound <- beta_ubound <- beta_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    fit2 <- FitDTVARMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      beta_start = beta,
      beta_lbound = beta_lbound,
      beta_ubound = beta_ubound,
      psi_diag = TRUE,
      psi_start = psi,
      psi_lbound = psi_lbound,
      psi_ubound = psi_ubound,
      theta_fixed = TRUE,
      theta_start = NULL,
      theta_lbound = NULL,
      theta_ubound = NULL,
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
    print(fit2)
    summary(fit2)
    print(fit2, means = FALSE)
    summary(fit2, means = FALSE)
    coef(fit2, psi = TRUE)
    vcov(fit2, psi = TRUE)
    testthat::test_that(
      paste(text, 2),
      {
        testthat::expect_true(
          all(
            abs(
              round(
                coef(fit),
                digits = 1
              ) - round(
                coef(fit2),
                digits = 1
              )
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-fitDTVARMx-fit-dt-var-mx-theta-null",
  tol = 0.1
)
