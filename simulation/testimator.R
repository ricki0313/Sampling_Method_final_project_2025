library(MASS)
# -------------------- function --------------------
# F-test + t-test
testimator_step1 <- function(yA, yB, alpha1 = 0.05, alpha2 = 0.05){
  nA  <- length(yA)
  nB  <- length(yB)
  mA  <- mean(yA)
  mB  <- mean(yB)
  sA2 <- var(yA)
  sB2 <- var(yB)
  
  ## 1. F-test (H0: sigma_A^2 = sigma_B^2)
  Fstat <- sA2 / sB2
  
  # two tail p-value
  pF <- 2 * min(
    pf(Fstat, df1 = nA - 1, df2 = nB - 1),
    1 - pf(Fstat, df1 = nA - 1, df2 = nB - 1)
  )
  equal_var <- (pF > alpha1)   # TRUE: sigma_A^2 = sigma_B^2
  
  ## 2. t-test: H0: mu_A = mu_B
  if (equal_var) {
    # pooled t-test
    sp2 <- ((nA - 1) * sA2 + (nB - 1) * sB2) / (nA + nB - 2)
    se_diff <- sqrt(sp2 * (1/nA + 1/nB))
    df <- nA + nB - 2
  } else {
    # Welch t-test
    se_diff <- sqrt(sA2/nA + sB2/nB)
    df <- (sA2/nA + sB2/nB)^2 /
      ((sA2/nA)^2/(nA - 1) + (sB2/nB)^2/(nB - 1))
  }
  
  tstat <- (mA - mB) / se_diff
  pT <- 2 * (1 - pt(abs(tstat), df))
  equal_mean <- (pT > alpha2)  # TRUE: mu_A = mu_B
  
  # return
  list(
    nA = nA, nB = nB,
    mA = mA, mB = mB,
    sA2 = sA2, sB2 = sB2,
    Fstat = Fstat, pF = pF, equal_var = equal_var,
    tstat = tstat, df = df, pT = pT, equal_mean = equal_mean
  )
}

testimator <- function(yA, yB,
                       prefer = c("smaller", "larger", "none"),
                       alpha1 = 0.05,   # F-test
                       alpha2 = 0.05,   # t-test 
                       gamma  = 0.05    # CI level when mode effect exists
) {
  prefer <- match.arg(prefer)
  
  # F-test + t-test
  base <- testimator_step1(yA, yB, alpha1 = alpha1, alpha2 = alpha2)
  
  nA  <- base$nA
  nB  <- base$nB
  mA  <- base$mA
  mB  <- base$mB
  sA2 <- base$sA2
  sB2 <- base$sB2
  
  if (base$equal_mean) {
    if (base$equal_var) {
      ## ---------- Case 1: equal variances equal means ----------
      alpha_ci <- alpha2
      
      # pooled variance
      sp2 <- ((nA - 1) * sA2 + (nB - 1) * sB2) / (nA + nB - 2)
      sp  <- sqrt(sp2)
      
      est <- (nA * mA + nB * mB) / (nA + nB)
      se_est <- sp / sqrt(nA + nB - 2)
      
      df_ci  <- nA + nB - 2
      t_crit <- qt(1 - alpha_ci/2, df_ci)
      
      lower <- est - t_crit * se_est
      upper <- est + t_crit * se_est
      
      chosen_mode <- "pooled_equal_variance"
    } else {
      ## ---------- Case 2: unequal variances equal means ----------
      alpha_ci <- alpha2
      
      wA <- nA / sA2
      wB <- nB / sB2
      w_sum <- wA + wB
      
      est <- (wA * mA + wB * mB) / w_sum
      
      # Var(theta_hat) ≈ 1 / (wA + wB)
      se_est <- sqrt(1 / w_sum)
      
      # df 用 step1 的 Welch df
      df_ci  <- base$df
      t_crit <- qt(1 - alpha_ci/2, df_ci)
      
      lower <- est - t_crit * se_est
      upper <- est + t_crit * se_est
      
      chosen_mode <- "pooled_unequal_variance"
    }
  } else {
    alpha_ci <- gamma
    if (prefer == "smaller" || prefer == "larger") {
      ## ---------- Case 3: unequal means (preferred direction known) ----------
      if (prefer == "smaller") {
        if (mA <= mB) {
          est <- mA; s2 <- sA2; n_use <- nA; chosen_mode <- "A"
        } else {
          est <- mB; s2 <- sB2; n_use <- nB; chosen_mode <- "B"
        }
      } else { # prefer == "larger"
        if (mA >= mB) {
          est <- mA; s2 <- sA2; n_use <- nA; chosen_mode <- "A"
        } else {
          est <- mB; s2 <- sB2; n_use <- nB; chosen_mode <- "B"
        }
      }
      
      se_est <- sqrt(s2 / n_use)
      df_ci  <- n_use - 1
      t_crit <- qt(1 - alpha_ci/2, df_ci)
      
      lower <- est - t_crit * se_est
      upper <- est + t_crit * se_est
      
    } else if (prefer == "none") {
      ## ---------- Case 4: unequal means (preferred direction unknown) ----------
      # mode A 的 CI
      seA   <- sqrt(sA2 / nA)
      dfA   <- nA - 1
      tA    <- qt(1 - alpha_ci/2, dfA)
      La    <- mA - tA * seA
      Ua    <- mA + tA * seA
      
      # mode B 的 CI
      seB   <- sqrt(sB2 / nB)
      dfB   <- nB - 1
      tB    <- qt(1 - alpha_ci/2, dfB)
      Lb    <- mB - tB * seB
      Ub    <- mB + tB * seB
      
      # 合併
      L <- min(La, Lb)
      U <- max(Ua, Ub)
      
      est   <- (L + U) / 2
      lower <- L
      upper <- U
      chosen_mode <- "interval"
    }
  } 
  
  c(base,
    list(
      prefer      = prefer,
      alpha1      = alpha1,
      alpha2      = alpha2,
      gamma       = gamma,
      chosen_mode = chosen_mode,
      est         = est,
      ci_lower    = lower,
      ci_upper    = upper
    ))
}

# -------------------- main --------------------
theta_true <- 0
sims_1 <- readRDS("data/scen01_rho95.rds")
sims_2 <- readRDS("data/scen02_rho95.rds")
sims_3 <- readRDS("data/scen03_rho95.rds")
sims_4 <- readRDS("data/scen04_rho95.rds")
sims_5 <- readRDS("data/scen05_rho95.rds")
sims_6 <- readRDS("data/scen06_rho95.rds")
sims_7 <- readRDS("data/scen07_rho95.rds")
sims_8 <- readRDS("data/scen08_rho95.rds")
sims_9 <- readRDS("data/scen09_rho95.rds")

## ---------- scenario 1 (mu_B=0, sigma2_B=1) ----------
R <- length(sims_1)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_1[[r]]$yA, sims_1[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 2 (mu_B=0, sigma2_B=2) ----------

R <- length(sims_2)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_2[[r]]$yA, sims_2[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 3 (mu_B=0.3, sigma2_B=2) ----------

R <- length(sims_3)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_3[[r]]$yA, sims_3[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 4 (mu_B=0.3, sigma2_B=1) ----------
R <- length(sims_4)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_4[[r]]$yA, sims_4[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 5 (mu_B=0.5, sigma2_B=2) ----------
R <- length(sims_5)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_5[[r]]$yA, sims_5[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 6 (mu_B=0.5, sigma2_B=1) ----------
R <- length(sims_6)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_6[[r]]$yA, sims_6[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 7 (mu_B=0.7, sigma2_B=2) ----------
R <- length(sims_7)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_7[[r]]$yA, sims_7[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

## ---------- scenario 8 (mu_B=0.7, sigma2_B=1) ----------
R <- length(sims_8)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_8[[r]]$yA, sims_8[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)


## ---------- scenario 9 (mu_B=0.7, sigma2_B=0.5) ----------
R <- length(sims_9)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- testimator(sims_9[[r]]$yA, sims_9[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- tmp$est
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}

summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

