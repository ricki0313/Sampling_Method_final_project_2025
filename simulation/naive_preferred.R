library(MASS)

# -------------------- function --------------------
naive_preferred <- function(yA, yB, prefer = c("smaller", "larger"),
                            conf_level = 0.95) {
  prefer <- match.arg(prefer)
  
  nA  <- length(yA)
  nB  <- length(yB)
  mA  <- mean(yA)
  mB  <- mean(yB)
  sA2 <- var(yA)
  sB2 <- var(yB)
  
  # ------ Step 1: point estimator ------
  if (prefer == "smaller") {
    est <- min(mA, mB)
    chosen_mode <- ifelse(mA <= mB, "A", "B")
  } else {
    est <- max(mA, mB)
    chosen_mode <- ifelse(mA >= mB, "A", "B")
  }

  # ------ Step 2: plug-in variance ------
  sd_xy <- sqrt(sA2/nA + sB2/nB)
  Delta <- (mB - mA) / sd_xy
  
  Phi_D  <- pnorm(Delta)
  Phi_mD <- pnorm(-Delta)
  phi_D  <- dnorm(Delta)
  
  # plug-in expected value of min(X,Y)
  Em1 <- mA * Phi_D + mB * Phi_mD - sd_xy * phi_D
  
  # plug-in expected value of min(X,Y)^2
  Em2 <- (mA^2 + sA2/nA) * Phi_D +
    (mB^2 + sB2/nB) * Phi_mD -
    (mA + mB) * sd_xy * phi_D
  
  var_est <- Em2 - Em1^2
  se_est  <- sqrt(var_est)
  
  # ------ Step 3: Confidence interval ------
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha/2)
  
  lower <- est - z_crit * se_est
  upper <- est + z_crit * se_est
  
  # ------ Output style matches your testimator() ------
  list(
    nA = nA, nB = nB,
    mA = mA, mB = mB,
    sA2 = sA2, sB2 = sB2,
    prefer = prefer,
    chosen_mode = chosen_mode,
    est = est,
    se = se_est,
    ci_lower = lower,
    ci_upper = upper
  )
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

# ---------- scenario 1 (mu_B=0, sigma2_B=1) ----------
R <- length(sims_1)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  fit <- naive_preferred(sims_1[[r]]$yA, sims_1[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
}
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

# ---------- scenario 2 (mu_B=0, sigma2_B=0.5) ----------
R <- length(sims_2)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  fit <- naive_preferred(sims_2[[r]]$yA, sims_2[[r]]$yB, prefer = "smaller")
  est_vec[r]   <- fit$est
  lower_vec[r] <- fit$ci_lower
  upper_vec[r] <- fit$ci_upper
}
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)
