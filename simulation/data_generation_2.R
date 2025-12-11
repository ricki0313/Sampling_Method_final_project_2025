library(MASS)

generate_data <- function(muA=0, sigmaA2=1,
                                 muB=0, sigmaB2=1,
                                 rho=0.95, nA=250, nB=250, R=500) {
  
  Sigma <- matrix(c(
    sigmaA2, rho*sqrt(sigmaA2*sigmaB2),
    rho*sqrt(sigmaA2*sigmaB2), sigmaB2
  ), 2, 2)
  
  sims <- vector("list", R)
  
  for(r in 1:R){
    
    # A sample (first batch)
    Y_A <- mvrnorm(nA, c(muA, muB), Sigma)
    yA <- Y_A[,1]   # only YA is observed
    
    # B sample (second batch)
    Y_B <- mvrnorm(nB, c(muA, muB), Sigma)
    yB <- Y_B[,2]   # only YB is observed
    
    sims[[r]] <- list(yA=yA, yB=yB)
  }
  
  sims
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

saveRDS(scen1_rho95, "data2/scen01_rho95.rds")
saveRDS(scen2_rho95, "data2/scen02_rho95.rds")
saveRDS(scen3_rho95, "data2/scen03_rho95.rds")
saveRDS(scen4_rho95, "data2/scen04_rho95.rds")
saveRDS(scen5_rho95, "data2/scen05_rho95.rds")
saveRDS(scen6_rho95, "data2/scen06_rho95.rds")
saveRDS(scen7_rho95, "data2/scen07_rho95.rds")
saveRDS(scen8_rho95, "data2/scen08_rho95.rds")
saveRDS(scen9_rho95, "data2/scen09_rho95.rds")
