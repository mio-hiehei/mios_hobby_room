library(MASS)


ols_qoi_sim <- function(X,
                        coef, 
                        vcov,
                        nsim = 1000,
                        scenario1_min,
                        scenario1_max,
                        scenario1_step,
                        name_var1,
                        df){
  
  require(MASS)
  
  coef <- model$reg_out$Coefficients 
  vcov <- model$VarCov
  nsim = 1000
  scenario1_min <- 7
  scenario1_max <- 19
  scenario1_step  <- 2
  name_var1 <- "Surname Length"
  df <- plot_data
  
  
  S <- mvrnorm(nsim, coef, vcov)
  
  k <- ncol(X)
  
  scenario1 <- seq(from = scenario1_min, to = scenario1_max, length.out = scenario1_step)
  
  cases <- array(NA, dim = c(nrow(X), k, length(scenario1)))
  
  cases[,,] <- X
  
  scen_df <- data.frame(mean = numeric(),
                        lwr = numeric(),
                        upr = numeric(),
                        scenario = numeric())
  
  for(scenario in 1:length(scenario1)){
    
    sel <- which(colnames(X) == name_var1)
    
    cases[,sel,scenario] <- scenario1[scenario]
    
    ev <- cases[,, scenario] %*% coef
    
    mean <- mean(ev)
    
    ### There must be an error because the confidence intervals are always the same size, regardless of the scenarios... Whats going on there?
    
    lwr <- quantile(ev, 0.025)
      mean - 1.96 * sd(ev)
    upr <- mean + 1.96 * sd(ev)
    
    scen_df[scenario,] <- cbind.data.frame(mean, lwr, upr, scenario1[scenario])
    
  }
  
  return(scen_df)
  
}

ggplot(data = scen_df,
       aes(x = scenario,
           ymin = lwr,
           ymax = upr))+
  geom_errorbar()+
  geom_point(data = df,
             aes_string(x = paste0("`", name_var1, "`"),
                        "`Productivity Index`"))

