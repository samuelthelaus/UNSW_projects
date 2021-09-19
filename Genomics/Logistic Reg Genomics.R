
library(MASS)
library(randomForest)
library(pscl)
library(pROC)

set.seed(26)

comp = gen_data_2[complete.cases(gen_data_2$all_pp), ]

# Impute data using random forest
gen_imputed = rfImpute(all_pp ~ X1800544 + X6277 + X4680 + X4354185 + X1076560 + 
                            X1800955 + X907094 + X242446, comp, ntree=500,
                          iter=50)

# Train test split
s_size = round(0.67 * nrow(gen_imputed), 0)
train_ind <- sample(seq_len(nrow(gen_imputed)), size = s_size)

train <- gen_imputed[train_ind, ]
test <- gen_imputed[-train_ind, ]

# Logistic model with all SNPs
log_mod = glm(all_pp ~ ., data = train,
              family = binomial(link = 'logit'))

summary(log_mod)

# Backward selection using AIC
mod_save = stepAIC(log_mod, scope=list(upper= ~X1800544 * X6277 * X4680 * X4354185 * X1076560 * 
                              X1800955 * X907094 * X242446, 
                            lower= ~1),
        direction = 'both')


mod_save$call

stepAIC(log_mod, direction = 'backward')

# Selected model with and without interactions
log_mod_2 = glm(all_pp ~ X6277 + X4680 + X1076560 + X1800955 + 
                  X1076560:X1800955 + X6277:X1800955, 
                family = binomial(link = "logit"), maxit = 500,
                data = train)


# Best model
summary(log_mod_2)

predictions <- predict(log_mod_2, newdata=test, type="response")

# For binary
roc(test$all_pp, predictions)

# For multiclass
multiclass.roc(test$Strategy, predictions)




