summary_sim <- function(est, lower, upper, theta_true, conf_level = 0.95) {
  # est:    (theta_hat_1, ..., theta_hat_R)
  # lower:  (lower_1, ... , lower_1)
  # upper:  (upper_1, ... , upper_1)
  
  if (length(est) != length(lower) ||
      length(est) != length(upper)) {
    stop("the length of est, lower, upper must be same")
  }
  
  R <- length(est)
  alpha <- 1 - conf_level
  
  ## 1. Bias
  bias <- mean(est) - theta_true
  
  ## 2. RMSE
  rmse <- sqrt(mean((est - theta_true)^2))
  
  ## 3. Coverage rate
  covered <- (lower <= theta_true) & (theta_true <= upper)
  coverage <- mean(covered)
  
  ## 4. Actual interval length (平均的 upper - lower)
  actual_len <- mean(upper - lower)
  
  ## 5. Expected interval length
  # 用模擬出來的 estimator 分佈的 sd，乘上理論 z critical
  sd_est <- sd(est)
  z_crit <- qnorm(1 - alpha/2)
  expected_len <- 2 * z_crit * sd_est
  
  data.frame(
    R               = R,
    theta_true      = theta_true,
    conf_level      = conf_level,
    bias            = bias,
    rmse            = rmse,
    coverage        = coverage,
    expected_len    = expected_len,
    actual_len      = actual_len
  )
}
