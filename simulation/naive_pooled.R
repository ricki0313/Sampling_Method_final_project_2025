library(MASS)

# -------------------- function --------------------
naive_pooled <- function(yA, yB, conf_level = 0.95) {
  
  # ------ combine samples ------
  z <- c(yA, yB)
  nA <- length(yA)
  nB <- length(yB)
  n  <- nA + nB
  
  # ------ sample mean ------
  mA <- mean(yA)
  mB <- mean(yB)
  m  <- mean(z)
  
  # ------ sample variance (one-sample) ------
  s2  <- var(z)    # denominator (n - 1)
  sd_z <- sqrt(s2)
  
  # ------ standard error ------
  SE <- sd_z / sqrt(n)
  
  # ------ confidence interval ------
  alpha <- 1 - conf_level
  df    <- n - 1
  t_crit <- qt(1 - alpha/2, df)
  
  lower <- m - t_crit * SE
  upper <- m + t_crit * SE
  
  # ------ output ------
  list(
    nA = nA, nB = nB, n_total = n,
    mA = mA, mB = mB, mean_pooled = m,
    var_pooled = s2,
    sd_pooled  = sd_z,
    SE = SE,
    df = df,
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
  tmp <- naive_pooled(sims_1[[r]]$yA, sims_1[[r]]$yB)
  est_vec[r]   <- tmp$mean_pooled
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)

# ---------- scenario 9 (mu_B=7, sigma2_B=0.5) ----------
R <- length(sims_9)
est_vec   <- numeric(R)
lower_vec <- numeric(R)
upper_vec <- numeric(R)

for (r in 1:R) {
  tmp <- naive_pooled(sims_9[[r]]$yA, sims_9[[r]]$yB)
  est_vec[r]   <- tmp$mean_pooled
  lower_vec[r] <- tmp$ci_lower
  upper_vec[r] <- tmp$ci_upper
}
summary_sim(est_vec, lower_vec, upper_vec, theta_true, conf_level = 0.95)
