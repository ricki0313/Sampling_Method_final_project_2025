library(MASS)

generate_data <- function(Npop = 1e6, nA = 250, nB = 250,
                          muA = 0, sigmaA2 = 1,
                          muB = 0, sigmaB2 = 1,
                          rho = 0.95,
                          R = 500) {
  
  Sigma <- matrix(c(
    sigmaA2, rho * sqrt(sigmaA2 * sigmaB2),
    rho * sqrt(sigmaA2 * sigmaB2), sigmaB2
  ), 2, 2)
  
  # generate super-population
  POP <- mvrnorm(Npop, c(muA, muB), Sigma)
  
  sims <- vector("list", R)
  
  for (r in 1:R) {
    idA <- sample(Npop, nA)
    idB <- sample(setdiff(1:Npop, idA), nB)
    
    yA <- POP[idA, 1]
    yB <- POP[idB, 2]
    
    sims[[r]] <- list(yA = yA, yB = yB)
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

saveRDS(scen1_rho95, "data3/scen01_rho95.rds")
saveRDS(scen2_rho95, "data3/scen02_rho95.rds")
saveRDS(scen3_rho95, "data3/scen03_rho95.rds")
saveRDS(scen4_rho95, "data3/scen04_rho95.rds")
saveRDS(scen5_rho95, "data3/scen05_rho95.rds")
saveRDS(scen6_rho95, "data3/scen06_rho95.rds")
saveRDS(scen7_rho95, "data3/scen07_rho95.rds")
saveRDS(scen8_rho95, "data3/scen08_rho95.rds")
saveRDS(scen9_rho95, "data3/scen09_rho95.rds")
