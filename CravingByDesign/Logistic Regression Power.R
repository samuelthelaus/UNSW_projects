library(brglm2)
library(simglm)

set.seed(46) 

power = c()
for(i in 1:10000) {
  sim_arguments <- list(
    formula = y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
    fixed = list(treat = list(var_type = 'factor', levels = c('treat', 'control')),
                 craving_score = list(var_type = 'continuous', mean = 0, sd = 1),
                 accuracy = list(var_type = 'continuous', mean = 0, sd = 1),
                 sex = list(var_type = 'factor', levels = c('male', 'female')),
                 age = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl1 = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl2 = list(var_type = 'continuous', mean = 0, sd = 1)),
    error = list(variance = 1),
    sample_size = 200,
    reg_weights = c(1, exp(3.47), exp(3.47), exp(3.47), 
                    exp(3.47), exp(3.47), exp(3.47), exp(3.47)),
    outcome_type = 'binary'
  )
  
  data = simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  log_mod = glm(y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
                data = data, family = binomial('logit'),
                method = 'brglmFit')
  
  s = summary(log_mod)
  p = s$coefficients[2, 4]
  
  if(p < .05) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)

