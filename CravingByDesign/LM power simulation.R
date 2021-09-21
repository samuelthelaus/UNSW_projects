library(tidyr)
library(simglm)

pow_list = c()

for(i in 1:1000) {
  my_seed = sample(1:1000000, size = 1, replace = TRUE)
  set.seed(my_seed)
  
  sim_arguments <- list(
    formula = y ~ 1 + weight + age + weight:age,
    fixed = list(weight = list(var_type = 'continuous', mean = 0, sd = 1),
                 age = list(var_type = 'continuous', mean = 0, sd = 1)),
    error = list(variance = 1),
    sample_size = 200,
    reg_weights = c(0, 0.1, 0.1, 0.1)
  )
  
  data = simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  s = summary(lm(y ~ weight * age, data = data))
  
  if(s[[4]][4,3] > 1.96 | s[[4]][4,3] < -1.96) {
    pow_list[length(pow_list) + 1] = 1
  } else {
    pow_list[length(pow_list) + 1] = 0
  }
}

mean(pow_list)




