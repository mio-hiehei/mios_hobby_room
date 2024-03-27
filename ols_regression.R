ols_function <- function(y, X){
  
  b_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Residuals
  e <- y - X %*% b_hat
  
  norm_dist_e <- cbind(e = sort(e),
                       zdist = sort(rnorm(nrow(e), 0, 1)))
  
  # plot(norm_dist_e)
  # Variance Estimates
  n <- nrow(X)
  k <- ncol(X)
  sigma_sq <- t(e) %*% e / (n - k) # constant is already in
  sigma_sq <- as.numeric(sigma_sq)
  
  # Variance-Covariance matrix
  var_cov <- sigma_sq * solve(t(X) %*% X)
  
  # Standard Errors
  std_err <- sqrt(diag(var_cov))
  
  t_value <- b_hat / std_err
  
  p_value <- 2*pt(-abs(t_value), df=length(X)-1)
  
  sqe <- sum((e + y) - mean(y)^2)
  sqt <- sum(y - mean(y)^2)
  
  R2 <- sqe / sqt
  adj_R2 <- 1 - ( (1- R2) * (nrow(X) - 1) / (nrow(X) - k - 1) )
  
  output <- list(reg_out = cbind.data.frame(Variables = colnames(X),
                               Coefficients = round(b_hat, 4),
                               SE = round(std_err, 4),
                               `t-value` = round(t_value, 4),
                               `p-value` = round(p_value, 4)),
                 VarCov = var_cov,
                 Diag = paste0("R2: ", round(R2, 3), 
                               "\n adjusted R2: ", round(adj_R2, 3)),
                 Residuals = norm_dist_e)
  
  return(output)
  
  
}

