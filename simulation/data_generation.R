library(MASS)

# -------------------- function --------------------
generate_data <- function(n = 250,
                          muA = 0, sigmaA2 = 1,
                          muB = 0, sigmaB2 = 1,
                          rho = 0.95, R = 500) {
  
  # covariance matrix
  Sigma <- matrix(c(
    sigmaA2, rho * sqrt(sigmaA2 * sigmaB2),
    rho * sqrt(sigmaA2 * sigmaB2), sigmaB2
  ), nrow = 2, byrow = TRUE)
  
  # list to store replications
  sims <- vector("list", R)
  
  for (r in 1:R) {
    Y <- mvrnorm(n = n, mu = c(muA, muB), Sigma = Sigma)
    sims[[r]] <- list(
      yA = Y[, 1],
      yB = Y[, 2]
    )
  }
  
  return(sims)
}

# ------------------- main ------------------------
set.seed(123)
scen1_rho95 <- generate_data(muB=0, sigmaB2=1)
scen2_rho95 <- generate_data(muB=0, sigmaB2=2)
scen3_rho95 <- generate_data(muB=0.3, sigmaB2=2)
scen4_rho95 <- generate_data(muB=0.3, sigmaB2=1)
scen5_rho95 <- generate_data(muB=0.5, sigmaB2=2)
scen6_rho95 <- generate_data(muB=0.5, sigmaB2=1)
scen7_rho95 <- generate_data(muB=0.7, sigmaB2=2)
scen8_rho95 <- generate_data(muB=0.7, sigmaB2=1)
scen9_rho95 <- generate_data(muB=0.7, sigmaB2=0.5)

saveRDS(scen1_rho95, "data/scen01_rho95.rds")
saveRDS(scen2_rho95, "data/scen02_rho95.rds")
saveRDS(scen3_rho95, "data/scen03_rho95.rds")
saveRDS(scen4_rho95, "data/scen04_rho95.rds")
saveRDS(scen5_rho95, "data/scen05_rho95.rds")
saveRDS(scen6_rho95, "data/scen06_rho95.rds")
saveRDS(scen7_rho95, "data/scen07_rho95.rds")
saveRDS(scen8_rho95, "data/scen08_rho95.rds")
saveRDS(scen9_rho95, "data/scen09_rho95.rds")
