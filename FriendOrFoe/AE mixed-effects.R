library(lme4)
library(Rmisc)
library(ggplot2)
library(comprehenr)
library(simr)
library(MuMIn)

# More complex models on the after-effect, controlling for multiple covariates

## Exp 1
model_exp1 = lmer(adj_low ~ condition + 
                    scale(bcRT) + scale(Age) + Gender +
                    (1 + scale(bcRT) + scale(Age) | participant),
                  data = data_exp[data_exp$experiment == 'first_experiment' &
                                    data_exp$volatility == 'Post-High', ],
                  REML = FALSE,
                  lmerControl(optCtrl = list(maxfun = 2e5)))

summary(model_exp1)
r.squaredGLMM(model_exp1)

# Interaction plot
sum_frame_1 <- summarySE(data_exp[data_exp$experiment == 'first_experiment', ], 
                         measurevar = 'adjusted', groupvars = c('volatility', 'condition'))

ggplot(sum_frame_1, aes(x = condition, y = adjusted, fill = volatility)) +
  geom_bar(stat='identity', position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = adjusted - se, ymax = adjusted + se), 
                position=position_dodge(width = 0.9), width=0.25) +
  theme_minimal() +
  scale_x_discrete(breaks=c('no_sound', 'sound'),
                   labels=c('No Sound', 'Sound')) +
  labs(x = 'Condition', y = 'Mean baseline corrected result') +
  scale_fill_grey(name = 'Volatility')

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/exp_1_ae_mm.png',
       width = 7, height = 5)

# Exp 2
model_exp2 = lmer(adj_low ~ condition + 
                    scale(bcRT) + scale(Age) + Gender +
                    (1 + scale(bcRT) + scale(Age) | participant),
                  data = data_exp[data_exp$experiment == 'white_noise' &
                                    data_exp$volatility == 'Post-High', ],
                  REML = FALSE,
                  lmerControl(optCtrl = list(maxfun = 2e5)))

summary(model_exp2)
r.squaredGLMM(model_exp2)

# Interaction plot
sum_frame_1 <- summarySE(data_exp[data_exp$experiment == 'white_noise', ], 
                         measurevar = 'adjusted', groupvars = c('volatility', 'condition'))

ggplot(sum_frame_1, aes(x = condition, y = adjusted, fill = volatility)) +
  geom_bar(stat='identity', position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = adjusted - se, ymax = adjusted + se), 
                position=position_dodge(width = 0.9), width=0.25) +
  theme_minimal() +
  scale_x_discrete(breaks=c('no_sound', 'sound'),
                   labels=c('No Sound', 'Sound')) +
  labs(x = 'Condition', y = 'Mean baseline corrected result') +
  scale_fill_grey(name = 'Volatility')

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/exp_2_ae_mm.png',
       width = 7, height = 5)

# Exp 1 & 2
model_exp12 = lmer(adjusted ~ volatility * condition + 
                     scale(bcRT) + scale(Age) + Gender +
                     (1 + scale(bcRT) + scale(Age) | participant),
                   data = data_exp[data_exp$experiment %in% c('first_experiment', 'white_noise'), ],
                   REML = FALSE,
                   lmerControl(optCtrl = list(method = 'nlminb', 
                                              maxfun = 2e5)))

summary(model_exp12)

# Interaction plot
sum_frame_1 <- summarySE(data_exp[data_exp$experiment %in% c('first_experiment', 'white_noise'), ], 
                         measurevar = 'adjusted', groupvars = c('volatility', 'condition'))

ggplot(sum_frame_1, aes(x = condition, y = adjusted, fill = volatility)) +
  geom_bar(stat='identity', position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = adjusted - se, ymax = adjusted + se), 
                position=position_dodge(width = 0.9), width=0.25) +
  theme_minimal() +
  scale_x_discrete(breaks=c('no_sound', 'sound'),
                   labels=c('No Sound', 'Sound')) +
  labs(x = 'Condition', y = 'Mean baseline corrected result') +
  scale_fill_grey(name = 'Volatility')

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/exp_12_ae_mm.png',
       width = 7, height = 5)

# Exp 3
model_exp3 = lmer(adjusted ~ volatility * condition + 
                    scale(bcRT) + scale(Age) + Gender +
                    (1 + scale(bcRT) + scale(Age) | participant),
                  data = data_exp[data_exp$experiment ==  'crossover_test', ],
                  REML = FALSE,
                  lmerControl(optCtrl = list(method = 'nlminb', 
                                             maxfun = 2e5)))

summary(model_exp3)

# Interaction plot
sum_frame_1 <- summarySE(data_exp[data_exp$experiment == 'crossover_test', ], 
                         measurevar = 'adjusted', groupvars = c('volatility', 'condition'))

ggplot(sum_frame_1, aes(x = condition, y = adjusted, fill = volatility)) +
  geom_bar(stat='identity', position=position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = adjusted - se, ymax = adjusted + se), 
                position=position_dodge(width = 0.9), width=0.25) +
  theme_minimal() +
  scale_x_discrete(breaks=c('no_sound', 'sound'),
                   labels=c('No Sound', 'Sound')) +
  labs(x = 'Condition', y = 'Mean baseline corrected result') +
  scale_fill_grey(name = 'Volatility')

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/exp_3_ae_mm.png',
       width = 7, height = 5)


# 3-way interaction between exp 1&2 and 3
data_exp$ExpGroup = as.factor(to_vec(for(item in data_exp$experiment)
  if(item %in% c('first_experiment', 'white_noise')) 'Exp 1 or 2'
  else if(item == 'crossover_test') 'Exp 3'))


model_exp123 = lmer(adjusted ~ condition * volatility * ExpGroup + 
                      scale(bcRT) + scale(Age) + Gender +
                      (1 + scale(bcRT) + scale(Age) | participant),
                    data = na.omit(data_exp),
                    REML = FALSE,
                    lmerControl(optCtrl = list(method = 'nlminb',
                                               maxfun = 2e5)))

model_exp123 = lmer(adj_low ~ condition * experiment + 
                      scale(bcRT) + scale(Age) + Gender +
                      (1 + scale(bcRT) + scale(Age) | participant),
                    data = data_exp[data_exp$experiment %in% c('first_experiment',
                                                               'crossover_test') &
                                      data_exp$volatility == 'Post-High', ],
                    REML = FALSE,
                    lmerControl(optCtrl = list(maxfun = 2e5)))

summary(model_exp123)

fixef(model_exp123)['conditionsound:volatilityPost-Low:ExpGroupExp 3'] = 10

powerSim(model_exp123, 
         test = fixed('condition:volatility:ExpGroup'), nsim = 1)



## Power

## Power for linear mixed model

# Set up simulated data for modeling
# Defining coefficients for the Intercept, Loss, Reward, Commitment and 3-way interactions
# 2-way interactions can be added with for example 'Loss:Reward' or 'Loss:Commit'
# Adding a 2-way interaction will require adding a coefficient size to the 'fixed_param' vector
fixed <- ~1 + Sound + Volatility + ExpGroup + RT + Age + Gender + Sound:Volatility + Sound:ExpGroup + Volatility:ExpGroup + Sound:Volatility:ExpGroup
random <- ~1 + RT + Age
fixed_param <- c(-0.3454, -0.1143, 0.5314, 0.04598, -0.009987, 
                 -0.09468, 0.004846, 0.1672, 0.02331, -0.0331, 0.5)
random_param <- list(random_var = c(0.10193, 0.03013, 0.44264), rand_gen = 'rnorm')
cov_param <- list(dist_fun = c('rnorm', 'rnorm'),
                  var_type = c("level1", "level2"),
                  opts = list(list(mean = 0, sd = 1),
                              list(mean = 0, sd = 1)))

error_var <- 1
with_err_gen <- 'rnorm'
data_str <- "long"
pow_param <- c('Sound', 'Volatility', 'ExpGroup', 'Sound:Volatility:ExpGroup') # parameters to test power for
alpha <- .05 # significance level
pow_dist <- "t"
pow_tail <- 2 # one or two-tailed (takes values 1 or 2)
replicates <- 1000 # change this to adjust how many simulations to run


# n is number of participants
# p is number of sessions
n = 51
p = 78
power = as.numeric(NA)

lmm_power_df = expand_grid(n, p, power)


for(row in 1:nrow(lmm_power_df)) {
  
  n = lmm_power_df$n[row]
  p = lmm_power_df$p[row]
  
  # Runs a loop a certain number of times (replicates)
  # Simulates a dataset where the 3-way interaction takes an average of beta = 0.5
  # Looks at the t-statistic of that beta value (note that no p-value is obtained as df is unknown for linear mixed models)
  # If | t | > 1.96, count the interaction as significant
  # If the interaction is significant, count that as one significant simulation
  # Take the proportion of significant simulations as power
  power = c()
  for(i in 1:replicates) {
    power_out = sim_pow_nested(fixed = fixed, random = random,
                               fixed_param = fixed_param,
                               random_param = random_param,
                               cov_param = cov_param,
                               n = n, p = p,
                               data_str = data_str, 
                               unbal = list('level2' = FALSE),
                               error_var = error_var, 
                               with_err_gen = with_err_gen,
                               pow_param = pow_param, alpha = alpha,
                               pow_dist = pow_dist, pow_tail = pow_tail)
    
    # The 4 in power_out$statistic[4] means that we're looking at the 4th item from 'pow_param'
    # If we wish to look at the first item, we change it to power_out$statistic[1]
    if(power_out$statistic[4] > 1.96 | power_out$statistic[4] < -1.96) {
      power[length(power) + 1] = 1
    } else {
      power[length(power) + 1] = 0
    }
  }
  
  # This is power for the 3-way interaction
  lmm_power_df$power[row] = mean(power)
  
  print(row)
}

View(lmm_power_df)
