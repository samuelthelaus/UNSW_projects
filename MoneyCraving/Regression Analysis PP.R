
library(brglm2)
library(lme4)
library(lmerTest)
library(Rmisc)
library(EnvStats)
library(ggplot2)
library(pscl)
library(MuMIn)
library(blme)

# Mode function
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


# GLM for penny picking

# Aggregate by participant
agg_glm = aggregate(PP ~ Out_Total + ID + Experiment,
                    data = comp_all_9mm[comp_all_9mm$Experiment != 'sessions_with_MBA_students', ], 
                    FUN = mean)

# Drop unused level
agg_glm$Experiment = droplevels(agg_glm$Experiment)

# Add first bet
for(id in unique(agg_glm$ID)) {
  x = Modes(comp_all_9mm$First_Bet[comp_all_9mm$ID == id])
  agg_glm$First_Bet[agg_glm$ID == id] = x[1]
}

# set up model
brglm_mod = glm(PP ~ Experiment + Out_Total + First_Bet, 
                data = agg_glm, family = binomial(link = 'logit'), 
                method = 'brglmFit')

summary(brglm_mod)

pR2(brglm_mod)

# GLMM by trial

# Pick out relevant trials
glm_trial = comp_all_8mm[
  comp_all_8mm$Apprentice == 1 & 
    comp_all_8mm$Time == 'After' &
    comp_all_8mm$PP == 1 &
    comp_all_8mm$Experiment != 'sessions_with_MBA_students', ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

# Create binary first bet variable
glm_trial$Trial_From_BS_binary = 0
glm_trial$Trial_From_BS_binary[glm_trial$Trial_From_BS > 2] = 1

# Set up model, ShotBelow8 varaible only for comp_all_7mm
glmm_1 = bglmer(Bet ~ Experiment +
                  Trial_From_BS_binary +
                  Last_Bet +
                  scale(Wealth) +
                  scale(Last_5_Outcomes) +
                  scale(Session_Number) +
                  #ShotBelow8 +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

summary(glmm_1)

# Theoretical Conditional R^2
r.squaredGLMM(glmm_1)

# GLMM by trial on everyone
glm_trial = comp_all_9mm[comp_all_9mm$Experiment != 'sessions_with_MBA_students', ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

glmm_1 = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP * 
                  (Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number)) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

summary(glmm_1)

r.squaredGLMM(glmm_1)

# RT split by Bowman
# GLMM by trial on everyone
glm_trial = comp_all_8mm[comp_all_8mm$Experiment %in% c('Experiment3',
                                                        'Experiment5') &
                           comp_all_8mm$Time == 'After' &
                           comp_all_8mm$Apprentice == 1, ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

glmm_rt = bglmer(Bet ~ Experiment +
                  PP * 
                  (Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number) +
                     scale(bcRT)) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

summary(glmm_rt)

r.squaredGLMM(glmm_rt)

summary_summary = summarySE(glm_trial, measurevar = 'bcRT', 
                            groupvars = c('Bet', 'PP'))
summary_summary$Bet = as.factor(summary_summary$Bet)
summary_summary$PP = as.factor(summary_summary$PP)

ggplot(summary_summary, aes(x = PP, y = bcRT, color = Bet)) +
  geom_point() +
  geom_line(aes(group = Bet)) +
  geom_errorbar(aes(ymin = bcRT - se, ymax = bcRT + se), 
                width = .1) +
  labs(x = 'Group', y = 'Box-Cox Response Time') +
  scale_x_discrete(breaks = c(0, 1), 
                   labels = c('Others', 'Penny Pickers')) +
  scale_color_discrete(name = 'Decision', labels = c('Skip', 'Bet')) +
  theme_classic()


#### RT analysis + extras ####

# Predicting PP using accuracy (exp 3 + 5)
# GLM for penny picking
# Aggregate RT by participant
agg_glm = aggregate(RT ~ PP + Out_Total + ID + Experiment,
                    data = comp_all_9mm[comp_all_9mm$Experiment != 'Experiment2', ], 
                    FUN = mean)

agg_glm$Experiment = droplevels(agg_glm$Experiment)

# Add first bet and accuracy
for(id in unique(agg_glm$ID)) {
  Correct = mean(comp_all_9mm$Correct[comp_all_9mm$ID == id], na.rm = TRUE)
  x = Modes(comp_all_9mm$First_Bet[comp_all_9mm$ID == id])
  agg_glm$First_Bet[agg_glm$ID == id] = x[1]
  agg_glm$Correct_Guess[agg_glm$ID == id] = Correct
}

# Set up model usng brglm package
brglm_mod = glm(PP ~ Experiment + Out_Total + First_Bet + Correct_Guess, 
                data = agg_glm, family = binomial(link = 'logit'), 
                method = 'brglmFit')

summary(brglm_mod)

# McFadden R^2
pR2(brglm_mod)

# GLMM

glm_trial = comp_all_8mm[
  comp_all_8mm$Apprentice == 1 & comp_all_8mm$Time == 'After' &
    comp_all_8mm$PP == 1 & 
    comp_all_8mm$Experiment == 'Experiment2', ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

glm_trial$Trial_From_BS_binary = 0
glm_trial$Trial_From_BS_binary[glm_trial$Trial_From_BS > 2] = 1

glmm_app = bglmer(Bet ~ 
                    #Experiment +
                    Trial_From_BS_binary +
                    Last_Bet +
                    scale(Last_5_Outcomes) +
                    scale(Wealth) +
                    scale(Session_Number) +
                    #Correct +
                    scale(RT) +
                    (1 | ID), 
                  data = glm_trial,
                  fixef.prior = t,
                  family = binomial(link = 'logit'),
                  glmerControl(optCtrl = list(maxfun = 2e5)))

summary(glmm_app)

r.squaredGLMM(glmm_app)


# GLMM others too

# Set up data, comment in/out experiment 2 or 3+5
# Sessions with apprentice, set apprentice to 1, else 0
glm_trial = comp_all_8mm[
  comp_all_8mm$Experiment %in% c('Experiment3',
                                          'Experiment5') &
    #comp_all_8mm$Experiment == 'Experiment2' &
    comp_all_8mm$Apprentice == 1, ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

# Set up model using brglmer (bayesian glmer)
glmm_1 = bglmer(Bet ~ 
                  Experiment +
                  PP * 
                  (Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number) +
                     scale(bcRT)) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

summary(glmm_1)

# Conditional R^2 in top right corner of matrix
r.squaredGLMM(glmm_1)



#### Descriptive statistics ####
# Check number of PP and number of sessions PP

# Create agg_glmm, which gets data by trial
# For main text descriptives, use comp_all_8mm, for appendix A1, use comp_all_7
agg_glmm = aggregate(PP_By_Session ~ ID + Session_Number + Experiment,
                     data = comp_all_9mm[comp_all_9mm$PP_By_Session == 1, ], FUN = mean)

# Create variables for number of times bet (bet2-4) and save to times_list
bet2 = 0
bet3 = 0
bet4 = 0
times_list = c()
for(sub in unique(agg_glmm$ID[agg_glmm$Experiment == 'Experiment2'])) {
  
  # Get data for individual participant
  x = agg_glmm[agg_glmm$ID == sub, ]
  
  # times = number of PP session
  times = sum(x$PP_By_Session)
  
  if(x$PP[1] == 1) {
    times_list[length(times_list) + 1] = times
  }
  
  # Add 1 to number of times bet
  if(times > 3) {
    bet4 = bet4 + 1
    bet3 = bet3 + 1
    bet2 = bet2 + 1
  } else if(times > 2) {
    bet3 = bet3 + 1
    bet2 = bet2 + 1
  } else if(times > 1) {
    bet2 = bet2 + 1
  }
}

# Check results
psych::describe(times_list)
Modes(times_list)
mean(comp_all_9mm$PP[comp_all_9mm$Experiment == 'Experiment2'])
bet2/length(unique(comp_all_9mm$ID[comp_all_9mm$Experiment == 'Experiment2' & comp_all_7$PP == 1]))
bet3/length(unique(comp_all_9mm$ID[comp_all_9mm$Experiment == 'Experiment2' & comp_all_7$PP == 1]))
bet4/length(unique(comp_all_9mm$ID[comp_all_9mm$Experiment == 'Experiment2' & comp_all_7$PP == 1]))

# Check number of black swans
agg_glm = aggregate(PP ~ Out_Total + ID + Experiment,
                    data = comp_all_9mm[comp_all_9mm$Experiment != 'sessions_with_MBA_students', ], 
                    FUN = mean)

# Print number by exp
for(exp in unique(agg_glm$Experiment)) {
  
  exp_data = agg_glm[agg_glm$Experiment == exp, ]
  
  print(exp)
  print(paste('PP: ', mean(exp_data$Out_Total[exp_data$PP == 1]),
              ' Others: ',
              mean(exp_data$Out_Total[exp_data$PP == 0])))
  
}

# Print average across experiments
print(paste('PP: ', mean(agg_glm$Out_Total[agg_glm$PP == 1]),
            ' Others: ',
            mean(agg_glm$Out_Total[agg_glm$PP == 0])))


# Model comparison model 3
glm_trial = comp_all_9mm[comp_all_9mm$Experiment != 'sessions_with_MBA_students', ]

glm_trial$Experiment = droplevels(glm_trial$Experiment)

glmm_1 = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP + 
                  Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

glmm_2 = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP * 
                     Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))


glmm_3 = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP * 
                  (Last_Bet +
                     scale(Wealth)) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

glmm_4 = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP * 
                  (Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes)) +
                     scale(Session_Number) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

glmm_full = bglmer(Bet ~ Experiment +
                  Apprentice +
                  PP * 
                  (Last_Bet +
                     scale(Wealth) +
                     scale(Last_5_Outcomes) +
                     scale(Session_Number)) +
                  (1 | ID), 
                data = glm_trial,
                fixef.prior = t,
                family = binomial(link = 'logit'),
                glmerControl(optCtrl = list(maxfun = 2e5)))

anova(glmm_1, glmm_2)
anova(glmm_2, glmm_3)
anova(glmm_3, glmm_4)
anova(glmm_4, glmm_full)

AIC(glmm_1, glmm_2, glmm_3, glmm_4, glmm_full)

summary(glmm_full)

r.squaredGLMM(glmm_full)





