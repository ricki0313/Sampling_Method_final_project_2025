testimator_step1 <- function(yA, yB, alpha1 = 0.05, alpha2 = 0.05){
  nA  <- length(yA)
  nB  <- length(yB)
  mA  <- mean(yA)
  mB  <- mean(yB)
  sA2 <- var(yA)
  sB2 <- var(yB)
  
  ## 1. F-test (H0: sigma_A^2 = sigma_B^2)
  Fstat <- sA2 / sB2
  
  # 2 tail p-value
  pF <- 2 * min(
    pf(Fstat, df1 = nA - 1, df2 = nB - 1),
    1 - pf(Fstat, df1 = nA - 1, df2 = nB - 1)
  )
  equal_var <- (pF > alpha1)   # TRUE: don't reject H0
  
  ## 2. t-test: H0: mu_A = mu_B ----
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
  equal_mean <- (pT > alpha2)  # TRUE = 接受平均相等
  
  # 先把這些資訊整理回傳
  list(
    nA = nA, nB = nB,
    mA = mA, mB = mB,
    sA2 = sA2, sB2 = sB2,
    Fstat = Fstat, pF = pF, equal_var = equal_var,
    tstat = tstat, df = df, pT = pT, equal_mean = equal_mean
  )
}