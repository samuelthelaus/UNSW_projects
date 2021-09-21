library(lme4)
library(lmerTest)
library(simr)

# this script contains tests for RT using mixed models

set.seed(26)

#### Difference sound/no sound ####

# Exp 1 (SIGNIFICANT)
mm_exp1 = lmer(bcRT ~ condition + (1 | participant),
               data = first_exp, REML = FALSE)

summary(mm_exp1)

powerSim(mm_exp1)

# Exp 2 (NOT SIGNIFICANT)
mm_exp2 = lmer(bcRT ~ condition + (1 | participant),
               data = white_noise, REML = FALSE)

summary(mm_exp2)

powerSim(mm_exp2)

# Exp 1 & 2 (SIGNIFICANT)
mm_exp12 = lmer(bcRT ~ condition + (1 | participant),
                data = first_white, REML = FALSE)

summary(mm_exp12)

powerSim(mm_exp12)

# Exp 3 (SIGNIFICANT)
mm_exp3 = lmer(bcRT ~ condition + (1 | participant),
               data = crossover, REML = FALSE)

summary(mm_exp3)

powerSim(mm_exp3)

#### Models with confounds ####

# Exp 1 (SIGNIFICANT)
mm2_exp1 = lmer(bcRT ~ condition + Age + Gender +
                  (1 | participant),
                data = first_exp, REML = FALSE)

summary(mm2_exp1)

powerSim(mm2_exp1)

# Exp 2 (SIGNIFICANT)
mm2_exp2 = lmer(bcRT ~ condition + Age + Gender +
                  (1 | participant),
                data = white_noise, REML = FALSE)

summary(mm2_exp2)

powerSim(mm2_exp2)

# Exp 1 & 2 (SIGNIFICANT)
mm2_exp12 = lmer(bcRT ~ condition + Age + Gender +
                   (1 | participant),
                 data = first_white, REML = FALSE)

summary(mm2_exp12)

powerSim(mm2_exp12)

# Exp 3 (SIGNIFICANT)
mm2_exp3 = lmer(bcRT ~ condition + Age + Gender +
                  (1 | participant),
                data = crossover, REML = FALSE)

summary(mm2_exp3)

powerSim(mm2_exp3)