## Analysis: Why sometimes adress and sometimes no adress?

schools_nrw_df$isna <- as.numeric(is.na(schools_nrw_df$lat))
schools_nrw_df$id <- 1:nrow(schools_nrw_df)
model <- glm(data = schools_nrw_df,
             isna ~ id + einwohner,
             family = binomial(link = "logit"))

summary(model)

scenarios_df <- data.frame(intercept = 1,
                           id = c(quantile(schools_nrw_df$id, 0.250, na.rm = TRUE),
                                  quantile(schools_nrw_df$id, 0.250, na.rm = TRUE),
                                  quantile(schools_nrw_df$id, 0.750, na.rm = TRUE),
                                  quantile(schools_nrw_df$id, 0.750, na.rm = TRUE)),
                           einwohner = c(quantile(schools_nrw_df$einwohner, 0.250, na.rm = TRUE),
                                         quantile(schools_nrw_df$einwohner, 0.750, na.rm = TRUE),
                                         quantile(schools_nrw_df$einwohner, 0.250, na.rm = TRUE),
                                         quantile(schools_nrw_df$einwohner, 0.750, na.rm = TRUE)))

predictions <- predict(model, scenarios_df, type = "response", se.fit = TRUE)
scenarios_df$fit <- predictions$fit
scenarios_df$lower <-  predictions$fit - predictions$se.fit * 1.96
scenarios_df$upper <-  predictions$fit + predictions$se.fit * 1.96

ggplot(data = scenarios_df,
       aes(x = as.character(id),
           color = as.character(einwohner)))+
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                position = "dodge")