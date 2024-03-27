## This file generates data on the surname analysis on the first tab.


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n <- sample(c(400:800), 1)

gender <- sample(c("male", "female"), n, replace = TRUE)

mean_coffee <- 3.3

plot_data <- data.frame(Name = randomNames(n = n,
                                           gender = gender,
                                           which.names = "both",
                                           name.order = "last.first")) %>%
  mutate(Gender = gender) %>%
  add_row(Name = "Hienstorfer-Heitmann, Mio", Gender = "male") %>%
  mutate(`Surname Length` = str_count(gsub("[- ]", "", str_extract(Name, "^.+?(?=,)"))),
         Age = c(round(rnorm(n, 45, 10), 0), 29),
         `Av. Coffees per day` = c(rbinom(n, 1, 0.8 ) *  rtruncnorm(n, mean = mean_coffee, sd = 2, a = 0 ), 3.2),
         `Delayed Gratification` = rbinom(n + 1, 1, 0.3),
         IQ = c(rnorm(n, 100, 15), 99 ),
         `eats Breakfast` = c(rbinom(n, 1, 0.75), 1),
         `Productivity Index` = 2.5 + 
           (0.3 * `Surname Length`)^1.0699+ 
           0.05 * (- (`Av. Coffees per day` - mean_coffee) ^ 2 + ( ( max(`Av. Coffees per day`) - mean_coffee ) ^2 ) ) +
           0.0001585 * `Delayed Gratification`+
           0.00000245 * IQ +
           0.25 * `eats Breakfast` +
           0.00048 * Age + 
           rnorm(n + 1, 0, 1),# error term
           `Productivity Index` = case_when(Name == "Hienstorfer-Heitmann, Mio"~ 9,
                                            TRUE ~ `Productivity Index`)
  )


plot_data$`Productivity Index` <- ifelse(plot_data$`Productivity Index` >= 10, 9.99, plot_data$`Productivity Index`)
plot_data$`Productivity Index` <- ifelse(plot_data$`Productivity Index` <= 0, 0.01, plot_data$`Productivity Index`)

var_desc_table <- data.frame(Variable = c("Age",
                                          "Av.Coffees per day",
                                          "Delayed Gratification",
                                          "IQ",
                                          "eats Breakfast"),
                             Description = c("Age of the respondent",
                                             "Av. no of coffees a respondent drinks on a day",
                                             "Unique data based on the Stanford marshmallow experiment. All respondents were tested on whether they were able to postpone the consumption of a sweet of their choice in the age of 4. Successful postponement was rewarded with another portion of the related sweet.",
                                             "IQ of respondent",
                                             "whether the respondent usually has breakfast in the moring"),
                             Expectation = c("With increasing age, employees may be more experienced and thus productive.",
                                             "Caffein is known to be a booster for concentration. Anyhow, it also increases anxiety and may distract in large doses.",
                                             "According to the described experiment, being able to delay gratification may also enable an individual to work more focussed and concientiously.",
                                             "A higher IQ may lead to more thoughtful ideas how to efficiently do tasks.",
                                             "Nutritionists point out the importance of a healthy and extensive breakfast to provide the body with enough energy throughout the day, which will increase productivity."))

X <- cbind(Intercept = 1,
           `Surname Length` = plot_data$`Surname Length`,
           Age = plot_data$Age,
           `Av. Coffees per day` = plot_data$`Av. Coffees per day`,
           `Delayed Gratification` = plot_data$`Delayed Gratification`,
           IQ = plot_data$IQ,
           `eats Breakfast` = plot_data$`eats Breakfast`)

y <- plot_data$`Productivity Index`

source("ols_regression.R")

model <- ols_function(X = X, y = y)

reg_data <- model$reg_out

rownames(reg_data) <- NULL

source("ols_qoi_sim.R")






# ggplot(data = plot_data,
#        aes(x = `Av. Coffees per day`,
#        y = `Productivity Index`))+
#   geom_point()



