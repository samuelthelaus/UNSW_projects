
library(randomForest)

set.seed(26)

chi_tab_1 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                    'X1800955', 'X907094', 'X242446'),
                       Separate = rep(NA, 8),
                       Only_PP = rep(NA, 8),
                       Risk = rep(NA, 8),
                       Cog = rep(NA, 8),
                       All_PP = rep(NA, 8))

chi_tab_2 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                  'X1800955', 'X907094', 'X242446'),
                       Separate = rep(NA, 8),
                       Only_PP = rep(NA, 8),
                       Risk = rep(NA, 8),
                       Cog = rep(NA, 8),
                       All_PP = rep(NA, 8))

for(c in 10:14) {
  comp = gen_data[complete.cases(gen_data[, c]), ]
  # Impute data using random forest
  gen_imputed = rfImpute(comp[, c] ~ X1800544 + X6277 + X4680 + X4354185 + X1076560 + 
                           X1800955 + X907094 + X242446, comp, ntree=500,
                         iter=50)
  for(r in 2:9) {
    chi_mod = chisq.test(gen_imputed[, 1], gen_imputed[, r])
    stat = round(chi_mod$statistic, 3)
    df = chi_mod$parameter
    p = round(chi_mod$p.value, 3)
    chi_tab_1[r - 1, c - 8] = paste('Chi2(', df, ')=', stat, ', p=', p, sep = '')
  }
}

for(c in 10:14) {
  comp = gen_data_2[complete.cases(gen_data_2[, c]), ]
  # Impute data using random forest
  gen_imputed = rfImpute(comp[, c] ~ X1800544 + X6277 + X4680 + X4354185 + X1076560 + 
                           X1800955 + X907094 + X242446, comp, ntree=500,
                         iter=50)
  for(r in 2:9) {
    chi_mod = chisq.test(gen_imputed[, 1], gen_imputed[, r])
    stat = round(chi_mod$statistic, 3)
    df = chi_mod$parameter
    p = round(chi_mod$p.value, 3)
    chi_tab_2[r - 1, c - 8] = paste('Chi2(', df, ')=', stat, ', p=', p, sep = '')
  }
}

write.csv(chi_tab_1, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/chi_tab_1.csv')
write.csv(chi_tab_2, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/chi_tab_2.csv')





