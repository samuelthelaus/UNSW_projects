
## Set up data
new_gen_data = read.csv('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/data_new_split200518.csv')

new_gen_data[new_gen_data == 'Undetermined'] = NA

for(i in 2:9) {
  new_gen_data[, i] = as.factor(new_gen_data[, i])
  new_gen_data[, i] = droplevels(new_gen_data[, i])
}

# new_gen_data = new_gen_data[new_gen_data$excluded == 0, ]

write.csv(new_gen_data, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/gen_data_new_split.csv', row.names = FALSE)

new_gen_data_2 = new_gen_data
new_gen_data_2[, 2:9] = lapply(new_gen_data_2[, 2:9], as.character)
new_gen_data_2[new_gen_data_2 == 'Allele 2'] = 'A2/HZ'
new_gen_data_2[new_gen_data_2 == 'Heterozygote'] = 'A2/HZ'
new_gen_data_2[,2:9] = as.data.frame(apply(new_gen_data_2[,2:9], MARGIN = 2, FUN = factor))

write.csv(new_gen_data_2, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/gen_data_new_split_2.csv', row.names = FALSE)


## Monotonic tables
library(randomForest)

set.seed(26)

chi_tab_1 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                  'X1800955', 'X907094', 'X242446'),
                       X7m = rep(NA, 8),
                       X7m_m = rep(NA, 8),
                       X8m = rep(NA, 8),
                       X8m_m = rep(NA, 8),
                       X9m = rep(NA, 8),
                       X9m_m = rep(NA, 8))

chi_tab_2 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                  'X1800955', 'X907094', 'X242446'),
                       X7m = rep(NA, 8),
                       X7m_m = rep(NA, 8),
                       X8m = rep(NA, 8),
                       X8m_m = rep(NA, 8),
                       X9m = rep(NA, 8),
                       X9m_m = rep(NA, 8))

x = 2
for(c in seq(10, 20, 2)) {
  comp = new_gen_data[complete.cases(new_gen_data[, c]), ]
  comp[,c] = as.factor(comp[,c])
  # Impute data using random forest
  gen_imputed = rfImpute(comp[, c] ~ X1800544 + X6277 + X4680 + X4354185 + X1076560 + 
                           X1800955 + X907094 + X242446, comp, ntree=500,
                         iter=50)
  for(r in 2:9) {
    chi_mod = chisq.test(gen_imputed[, 1], gen_imputed[, r])
    stat = round(chi_mod$statistic, 3)
    df = chi_mod$parameter
    p = round(chi_mod$p.value, 3)
    chi_tab_1[r - 1, x] = paste('Chi2(', df, ')=', stat, ', p=', p, sep = '')
  }
  x = x + 1
}

x = 2
for(c in seq(10, 20, 2)) {
  comp = new_gen_data_2[complete.cases(new_gen_data_2[, c]), ]
  comp[,c] = as.factor(comp[,c])
  # Impute data using random forest
  gen_imputed = rfImpute(comp[, c] ~ X1800544 + X6277 + X4680 + X4354185 + X1076560 + 
                           X1800955 + X907094 + X242446, comp, ntree=500,
                         iter=50)
  for(r in 2:9) {
    chi_mod = chisq.test(gen_imputed[, 1], gen_imputed[, r])
    stat = round(chi_mod$statistic, 3)
    df = chi_mod$parameter
    p = round(chi_mod$p.value, 3)
    chi_tab_2[r - 1, x] = paste('Chi2(', df, ')=', stat, ', p=', p, sep = '')
  }
  x = x + 1
}

write.csv(chi_tab_1, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/chi_tab_1_new.csv')
write.csv(chi_tab_2, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/chi_tab_2_new.csv')


## T-tables
aov_tab_1 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                  'X1800955', 'X907094', 'X242446'),
                       X7m = rep(NA, 8),
                       X7m_m = rep(NA, 8),
                       X8m = rep(NA, 8),
                       X8m_m = rep(NA, 8),
                       X9m = rep(NA, 8),
                       X9m_m = rep(NA, 8))

t_tab_2 = data.frame(Column = c('X1800544', 'X6277', 'X4680', 'X4354185', 'X1076560', 
                                  'X1800955', 'X907094', 'X242446'),
                       X7m = rep(NA, 8),
                       X7m_m = rep(NA, 8),
                       X8m = rep(NA, 8),
                       X8m_m = rep(NA, 8),
                       X9m = rep(NA, 8),
                       X9m_m = rep(NA, 8))

x = 2
for(c in seq(11, 21, 2)) {
  comp = new_gen_data[complete.cases(new_gen_data[, c]), ]

  for(r in 2:9) {
    aov_mod = summary(aov(comp[, c] ~ comp[, r], data = comp))
    df1 = aov_mod[[1]][1,1]
    df2 = aov_mod[[1]][2,1]
    f = round(aov_mod[[1]][1,4], 3)
    p = round(aov_mod[[1]][1,5], 3)
    aov_tab_1[r - 1, x] = paste('F(', df1, ', ', df2, ') = ', f, ', p = ', p, sep = '')
  }
  x = x + 1
}

x = 2
for(c in seq(11, 21, 2)) {
  comp = new_gen_data_2[complete.cases(new_gen_data_2[, c]), ]
  
  for(r in 2:9) {
    t_mod = t.test(comp[comp[, r] == 'Allele 1', c], comp[comp[, r] == 'A2/HZ', c],
                   var.equal = TRUE)
    df = t_mod$parameter
    t = abs(round(t_mod$statistic, 3))
    p = round(t_mod$p.value, 3)
    t_tab_2[r - 1, x] = paste('t(', df, ')=', t, ', p=', p, sep = '')
  }
  x = x + 1
}

write.csv(aov_tab_1, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/aov_tab.csv')
write.csv(t_tab_2, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Dump/t_tab.csv')

