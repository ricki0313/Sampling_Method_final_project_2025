library(MASS)

# -------------------- function --------------------
# posterior parameter for normal + inverse-gamma
posterior_norm_inv_gamma <- function(y, m0 = 0, k0 = 0.01, a0 = 0.001, b0 = 0.001) {
  n   <- length(y)
  ybar <- mean(y)
  s2   <- var(y)
  
  k_n <- k0 + n
  m_n <- (k0 * m0 + n * ybar) / k_n
  a_n <- a0 + n / 2
  b_n <- b0 + 0.5 * ((n - 1) * s2 + (k0 * n / k_n) * (ybar - m0)^2)
  
  list(m_n = m_n, k_n = k_n, a_n = a_n, b_n = b_n)
}

# draw R_post sets of (mu, sigma2) from posterior 
draw_mu_sigma2 <- function(post_par, R_post = 5000) {
  mu   <- numeric(R_post)
  sig2 <- numeric(R_post)
  
  for (r in 1:R_post) {
    sig2[r] <- 1 / rgamma(1, shape = post_par$a_n, rate = post_par$b_n)
    mu[r] <- rnorm(1, mean = post_par$m_n, sd = sqrt(sig2[r] / post_par$k_n))
  }
  list(mu = mu, sigma2 = sig2)
}

# model 1 & 3: different means
bayes_effect_size <- function(yA, yB, R_post = 5000,
                              m0_a = 0, k0_a = 0.01,
                              a0_a = 0.001, b0_a = 0.001,
                              m0_b = 0, k0_b = 0.01,
                              a0_b = 0.001, b0_b = 0.001) {
  
  # posterior for mode A
  postA <- posterior_norm_inv_gamma(yA, m0_a, k0_a, a0_a, b0_a)
  drawA <- draw_mu_sigma2(postA, R_post)
  
  # posterior for mode B
  postB <- posterior_norm_inv_gamma(yB, m0_b, k0_b, a0_b, b0_b)
  drawB <- draw_mu_sigma2(postB, R_post)
  
  muA   <- drawA$mu
  sig2A <- drawA$sigma2
  muB   <- drawB$mu
  sig2B <- drawB$sigma2
  
  # posterior draws of effect size g
  g <- (muA - muB) / sqrt((sig2A + sig2B) / 2)
  
  list(muA = muA, sig2A = sig2A,
       muB = muB, sig2B = sig2B,
       g   = g)
}

# model 2 & 4: same means
bayes_common_mean <- function(yA, yB, R_post  = 5000, burn_in = 1000,
                              m0      = 0,      # prior mean of mu
                              tau0    = 1/100,  # prior precision = 1 / w^2 (w^2 = 100)
                              a0      = 0.001,
                              b0      = 0.001) {
  
  nA  <- length(yA); nB <- length(yB)
  mA  <- mean(yA);   mB <- mean(yB)
  s2A <- var(yA);    s2B <- var(yB)
  
  # init value
  mu    <- (mA + mB) / 2
  sig2A <- s2A
  sig2B <- s2B
  
  n_iter   <- R_post + burn_in
  mu_keep  <- numeric(R_post)
  
  for (it in 1:n_iter) {
    
    ## 1. update mu | sig2A, sig2B, data
    prec_mu <- tau0 + nA / sig2A + nB / sig2B
    var_mu  <- 1 / prec_mu
    mean_mu <- var_mu * (tau0 * m0 + nA * mA / sig2A + nB * mB / sig2B)
    mu      <- rnorm(1, mean = mean_mu, sd = sqrt(var_mu))
    
    ## 2. update sig2A | mu, dataA
    rssA     <- sum((yA - mu)^2)
    aA_post  <- a0 + nA / 2
    bA_post  <- b0 + 0.5 * rssA
    sig2A    <- 1 / rgamma(1, shape = aA_post, rate = bA_post)
    
    ## 3. update sig2B | mu, dataB
    rssB     <- sum((yB - mu)^2)
    aB_post  <- a0 + nB / 2
    bB_post  <- b0 + 0.5 * rssB
    sig2B    <- 1 / rgamma(1, shape = aB_post, rate = bB_post)
    
    ## keep posterior draws (after burn-in)
    if (it > burn_in) {
      mu_keep[it - burn_in] <- mu
    }
  }
  
  list(mu = mu_keep)
}

# marginal likelihood: Normal + Inverse Gamma
log_marginal_norminv_one <- function(y, m0 = 0, k0 = 0.01, a0 = 0.001, b0 = 0.001) {
  n    <- length(y)
  ybar <- mean(y)
  s2   <- var(y)
  
  k_n <- k0 + n
  a_n <- a0 + n / 2
  b_n <- b0 + 0.5 * ((n - 1) * s2 +
                       (k0 * n / k_n) * (ybar - m0)^2)
  
  # textbook formula for Normal-InvGamma evidence
  log_val <-
    lgamma(a_n) - lgamma(a0) +
    a0 * log(b0) - a_n * log(b_n) +
    0.5 * (log(k0) - log(k_n)) -
    n / 2 * log(2 * pi)
  
  return(log_val)
}

# marginal M1 (different means different variances)
log_marginal_M1 <- function(yA, yB,
                            m0_a = 0, k0_a = 0.01,
                            a0_a = 0.001, b0_a = 0.001,
                            m0_b = 0, k0_b = 0.01,
                            a0_b = 0.001, b0_b = 0.001) {
  
  log_mA <- log_marginal_norminv_one(yA, m0_a, k0_a, a0_a, b0_a)
  log_mB <- log_marginal_norminv_one(yB, m0_b, k0_b, a0_b, b0_b)
  log_mA + log_mB
}

# marginal M3 (different means same variances)
log_marginal_M3 <- function(yA, yB,
                            m0_a = 0, k0_a = 0.01,
                            m0_b = 0, k0_b = 0.01,
                            a0   = 0.001, b0   = 0.001) {
  
  nA <- length(yA); nB <- length(yB)
  mA <- mean(yA);   mB <- mean(yB)
  s2A <- var(yA);   s2B <- var(yB)
  N  <- nA + nB
  
  # adjust parameter
  kA_n <- k0_a + nA
  kB_n <- k0_b + nB
  
  # sum-of-squares + prior
  QA <- (nA - 1) * s2A + (k0_a * nA / kA_n) * (mA - m0_a)^2
  QB <- (nB - 1) * s2B + (k0_b * nB / kB_n) * (mB - m0_b)^2
  
  a_n <- a0 + (N / 2)
  b_n <- b0 + 0.5 * (QA + QB)
  
  log_val <-
    # Normal constant
    - N / 2 * log(2 * pi) +
    # means of Normal prior + likelihood
    0.5 * (log(k0_a) - log(kA_n) + log(k0_b) - log(kB_n)) +
    # sigma2 of Inv-Gamma 
    (lgamma(a_n) - lgamma(a0) +
       a0 * log(b0) - a_n * log(b_n))
  
  return(log_val)
}

# marginal M4 (same means same variances)
log_marginal_M4 <- function(yA, yB,
                            m0 = 0, k0 = 0.01,
                            a0 = 0.001, b0 = 0.001) {
  
  y_all <- c(yA, yB)
  n  <- length(y_all)
  m  <- mean(y_all)
  s2 <- var(y_all)
  
  k_n <- k0 + n
  a_n <- a0 + n / 2
  b_n <- b0 + 0.5 * ((n - 1) * s2 +
                       (k0 * n / k_n) * (m - m0)^2)
  
  log_val <-
    lgamma(a_n) - lgamma(a0) +
    a0 * log(b0) - a_n * log(b_n) +
    0.5 * (log(k0) - log(k_n)) -
    n / 2 * log(2 * pi)
  
  return(log_val)
}

# marginal M2 (same means different variances)
log_marginal_y_given_mu_one <- function(y, mu, a0 = 0.001, b0 = 0.001) {
  n <- length(y)
  
  sapply(mu, function(m) {
    ss  <- sum((y - m)^2)
    a_n <- a0 + n / 2
    b_n <- b0 + 0.5 * ss
    
    log_val <-
      - n / 2 * log(2 * pi) +
      lgamma(a_n) - lgamma(a0) +
      a0 * log(b0) - a_n * log(b_n)
    
    log_val
  })
}

log_marginal_M2 <- function(yA, yB,
                            beta0_mu = 0,  # μ 的 prior mean
                            phi2_mu  = 100, # μ 的 prior variance
                            a0_a = 0.001, b0_a = 0.001,
                            a0_b = 0.001, b0_b = 0.001) {
  
  nA <- length(yA); nB <- length(yB)
  mA <- mean(yA);   mB <- mean(yB)
  N  <- nA + nB
  m_all <- (nA * mA + nB * mB) / N
  
  # integrand: exp( log p(yA|mu) + log p(yB|mu) + log prior(mu) )
  log_f <- function(mu) {
    log_mA <- log_marginal_y_given_mu_one(yA, mu, a0_a, b0_a)
    log_mB <- log_marginal_y_given_mu_one(yB, mu, a0_b, b0_b)
    log_prior <- dnorm(mu,
                       mean = beta0_mu,
                       sd   = sqrt(phi2_mu),
                       log  = TRUE)
    log_mA + log_mB + log_prior
  }
  
  # 在 posterior mass 最大附近做穩定化，避免 underflow
  mu0 <- m_all
  C   <- log_f(mu0)
  
  integrand <- function(mu) {
    exp(log_f(mu) - C)
  }
  
  # 积分範圍用 prior 的 ±10 sd（幾乎包含所有 mass）
  L <- 10 * sqrt(phi2_mu)
  res <- integrate(integrand,
                   lower = mu0 - L,
                   upper = mu0 + L,
                   rel.tol = 1e-6)
  
  log_m2 <- log(res$value) + C
  return(log_m2)
}

# model averaging
model_averaging_mixedmode <- function(
    yA, yB,
    prefer = c("smaller", "larger", "none"),
    R_post = 5000,
    # hyper-parameters
    m0 = 0, k0 = 0.01, a0 = 0.001, b0 = 0.001,
    m0_common   = 0,
    tau0_common = 1/100,
    a0_common   = 0.001,
    b0_common   = 0.001,
    beta0_mu = 0,   # Model 2: μ ~ N(beta0_mu, phi2_mu)
    phi2_mu  = 100,
    ci_level = 0.95
) {
  
  prefer <- match.arg(prefer)
  
  ## ----- Step 1: 四個模型的 marginal likelihood (log scale) -----
  log_m1 <- log_marginal_M1(
    yA, yB,
    m0_a = m0, k0_a = k0, a0_a = a0, b0_a = b0,
    m0_b = m0, k0_b = k0, a0_b = a0, b0_b = b0
  )
  
  log_m2 <- log_marginal_M2(
    yA, yB,
    beta0_mu = beta0_mu,
    phi2_mu  = phi2_mu,
    a0_a = a0, b0_a = b0,
    a0_b = a0, b0_b = b0
  )
  
  log_m3 <- log_marginal_M3(
    yA, yB,
    m0_a = m0, k0_a = k0,
    m0_b = m0, k0_b = k0,
    a0   = a0, b0   = b0
  )
  
  log_m4 <- log_marginal_M4(
    yA, yB,
    m0 = m0, k0 = k0,
    a0 = a0, b0 = b0
  )
  
  log_m_vec <- c(log_m1, log_m2, log_m3, log_m4)
  
  # 為了數值穩定：減掉最大 log，再 exponentiate
  log_m_shift <- log_m_vec - max(log_m_vec)
  w_raw <- exp(log_m_shift)
  w <- w_raw / sum(w_raw)
  names(w) <- paste0("M", 1:4)
  
  ## ----- Step 2: 在「不同 mean 模型」與「共同 mean 模型」下抽 posterior -----
  # 不同 mean 模型的 posterior draws (M1 & M3 共用這一組)
  eff <- bayes_effect_size(
    yA, yB,
    R_post = R_post,
    m0_a = m0, k0_a = k0, a0_a = a0, b0_a = b0,
    m0_b = m0, k0_b = k0, a0_b = a0, b0_b = b0
  )
  muA_post <- eff$muA
  muB_post <- eff$muB
  
  # 共同 mean 模型的 posterior draws (M2 & M4 共用)
  cm <- bayes_common_mean(
    yA, yB,
    R_post  = R_post, burn_in = 1000,
    m0      = m0_common,
    tau0    = tau0_common,
    a0      = a0_common,
    b0      = b0_common
  )
  mu_common <- cm$mu
  
  ## ----- Step 3: 依照模型權重抽 M，再決定每一次的 θ_draw -----
  theta_draw <- numeric(R_post)
  model_draw <- integer(R_post)
  
  for (r in 1:R_post) {
    
    # 抽 model index: 1~4
    m_id <- sample(1:4, size = 1, prob = w)
    model_draw[r] <- m_id
    
    if (m_id %in% c(1, 3)) {
      # M1 or M3: 不同 mean 模型
      if (prefer == "none") {
        # 沒有偏好，A/B 各 0.5 機率
        chooseA <- rbinom(1, size = 1, prob = 0.5)
        theta_r <- if (chooseA == 1) muA_post[r] else muB_post[r]
      } else if (prefer == "smaller") {
        theta_r <- min(muA_post[r], muB_post[r])
      } else { # prefer == "larger"
        theta_r <- max(muA_post[r], muB_post[r])
      }
    } else {
      # M2 or M4: 共同 mean 模型
      theta_r <- mu_common[r]
    }
    
    theta_draw[r] <- theta_r
  }
  
  ## ----- Step 4: 用 θ_draw 做推論 -----
  
  theta_hat <- mean(theta_draw)
  alpha_ci  <- 1 - ci_level
  ci        <- quantile(theta_draw, c(alpha_ci/2, 1 - alpha_ci/2))
  
  list(
    weights    = w,           # M1–M4 權重
    model_draw = model_draw,  # 每次抽到哪個 model
    est        = theta_hat,
    ci_lower   = ci[1],
    ci_upper   = ci[2],
    theta_draw = theta_draw
  )
}

# -------------------- main --------------------
theta_true <- 0
sims_1 <- readRDS("data2/scen01_rho95.rds")
sims_2 <- readRDS("data2/scen02_rho95.rds")
sims_3 <- readRDS("data2/scen03_rho95.rds")
sims_4 <- readRDS("data2/scen04_rho95.rds")
sims_5 <- readRDS("data2/scen05_rho95.rds")
sims_6 <- readRDS("data2/scen06_rho95.rds")
sims_7 <- readRDS("data2/scen07_rho95.rds")
sims_8 <- readRDS("data2/scen08_rho95.rds")
sims_9 <- readRDS("data2/scen09_rho95.rds")

# we have to draw R_post times from posterior
set.seed(123)

## ---------- scenario 1 (mu_B=0, sigma2_B=1) ----------
R <- length(sims_1)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_1[[r]]$yA,
    sims_1[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 2 (mu_B=0, sigma2_B=2) ----------
R <- length(sims_2)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_2[[r]]$yA,
    sims_2[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 3 (mu_B=0.3, sigma2_B=2) ----------
R <- length(sims_3)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_3[[r]]$yA,
    sims_3[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 4 (mu_B=0.3, sigma2_B=1) ----------
R <- length(sims_4)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_4[[r]]$yA,
    sims_4[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 5 (mu_B=0.5, sigma2_B=2) ----------
R <- length(sims_5)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_5[[r]]$yA,
    sims_5[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 6 (mu_B=0.5, sigma2_B=1) ----------
R <- length(sims_6)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_6[[r]]$yA,
    sims_6[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 7 (mu_B=0.7, sigma2_B=2) ----------
R <- length(sims_7)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_7[[r]]$yA,
    sims_7[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 8 (mu_B=0.7, sigma2_B=1) ----------
R <- length(sims_8)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_8[[r]]$yA,
    sims_8[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 9 (mu_B=0.7, sigma2_B=0.5) ----------
R <- length(sims_9)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)
w_mat <- matrix(NA, nrow = R, ncol = 4)
colnames(w_mat) <- paste0("M", 1:4)

for (r in 1:R) {
  fit <- model_averaging_mixedmode(
    sims_9[[r]]$yA,
    sims_9[[r]]$yB,
    prefer = "smaller",
    R_post = 5000
  )
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
  w_mat[r, ] <- fit$weights
}

round(colMeans(w_mat),3)
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

