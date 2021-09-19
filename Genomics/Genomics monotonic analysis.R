
gen_data = read.csv('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/data_new_split200518.csv')

gen_data[gen_data == 'Undetermined'] = NA

gen_data$only_pp = NA
gen_data$pp_risk = NA
gen_data$pp_cog = NA
gen_data$all_pp = NA

# Create new vars
for(i in which(!is.na(gen_data$Strategy))) {
  if(gen_data$Strategy[i] == 'sophisticated') {
    gen_data$only_pp[i] = 'sophisticated'
    gen_data$pp_risk[i] = 'sophisticated'
    gen_data$pp_cog[i] = 'sophisticated'
    gen_data$all_pp[i] = 'sophisticated'
  } else if(gen_data$Strategy[i] == 'pp') {
    gen_data$only_pp[i] = 'pp'
    gen_data$pp_risk[i] = 'pp'
    gen_data$pp_cog[i] = 'pp'
    gen_data$all_pp[i] = 'pp'
  } else if(gen_data$Strategy[i] == 'pp_impaired_cognition') {
    gen_data$pp_cog[i] = 'pp'
    gen_data$all_pp[i] = 'pp'
  } else if(gen_data$Strategy[i] == 'pp_riskloving') {
    gen_data$pp_risk[i] = 'pp'
    gen_data$all_pp[i] = 'pp'
  } else if(gen_data$Strategy[i] == 'did_not_understand_task') {
    gen_data$all_pp[i] = 'pp'
  }
}


gen_data = as.data.frame(apply(gen_data, MARGIN = 2, FUN = factor))

for(i in 2:9) {
  gen_data[, i] = droplevels(gen_data[, i])
}

write.csv(gen_data, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/gen_data_new_split.csv', row.names = FALSE)

#### Allele 2 and HZ separate ####

## rs6277

# All separate
chi_1_1 = chisq.test(gen_data$X6277, gen_data$Strategy)
chi_1_1

con_tab = table(gen_data$X6277, gen_data$Strategy)
con_tab

cohen.ES(test = 'chisq', size = 'medium')

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_1_1$parameter, sig.level = .05)

# Only pure PP
chi_1_2 = chisq.test(gen_data$X6277, gen_data$only_pp)
chi_1_2

con_tab = table(gen_data$X6277, gen_data$only_pp)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_1_2$parameter, sig.level = .05)

# PP + riskloving
chi_1_3 = chisq.test(gen_data$X6277, gen_data$pp_risk)
chi_1_3

con_tab = table(gen_data$X6277, gen_data$pp_risk)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_1_3$parameter, sig.level = .05)

# PP + cog impair
chi_1_4 = chisq.test(gen_data$X6277, gen_data$pp_cog)
chi_1_4

con_tab = table(gen_data$X6277, gen_data$pp_cog)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_1_4$parameter, sig.level = .05)

# All together
chi_1_5 = chisq.test(gen_data$X6277, gen_data$all_pp)
chi_1_5

con_tab = table(gen_data$X6277, gen_data$all_pp)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_1_5$parameter, sig.level = .05)


## rs1076560

# All separate
chi_2_1 = chisq.test(gen_data$X1076560, gen_data$Strategy)
chi_2_1

con_tab = table(gen_data$X1076560, gen_data$Strategy)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_1$parameter, sig.level = .05)

# Only pure PP
chi_2_2 = chisq.test(gen_data$X1076560, gen_data$only_pp)
chi_2_2

con_tab = table(gen_data$X1076560, gen_data$only_pp)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_2_2$parameter, sig.level = .05)

# PP + riskloving
chi_2_3 = chisq.test(gen_data$X1076560, gen_data$pp_risk)
chi_2_3

con_tab = table(gen_data$X1076560, gen_data$pp_risk)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_3$parameter, sig.level = .05)

# PP + cog impair
chi_2_4 = chisq.test(gen_data$X1076560, gen_data$pp_cog)
chi_2_4

con_tab = table(gen_data$X1076560, gen_data$pp_cog)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_2_4$parameter, sig.level = .05)

# All together
chi_2_5 = chisq.test(gen_data$X1076560, gen_data$all_pp)
chi_2_5

con_tab = table(gen_data$X1076560, gen_data$all_pp)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_2_5$parameter, sig.level = .05)



#### Allele 2 and HZ together ####

gen_data_2 = gen_data
gen_data_2[, ] = lapply(gen_data_2[, ], as.character)
gen_data_2[gen_data_2 == 'Allele 2'] = 'A2/HZ'
gen_data_2[gen_data_2 == 'Heterozygote'] = 'A2/HZ'
gen_data_2 = as.data.frame(apply(gen_data_2, MARGIN = 2, FUN = factor))

write.csv(gen_data_2, 'C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/gen_data_2.csv', row.names = FALSE)

## rs6277

# All separate
chi_1_1x = chisq.test(gen_data_2$X6277, gen_data_2$Strategy)
chi_1_1x

con_tab = table(gen_data_2$X6277, gen_data_2$Strategy)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_1_1x$parameter, sig.level = .05)

# Only pure PP
chi_1_2x = chisq.test(gen_data_2$X6277, gen_data_2$only_pp)
chi_1_2x

con_tab = table(gen_data_2$X6277, gen_data_2$only_pp)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_1_2x$parameter, sig.level = .05)

# PP + riskloving
chi_1_3x = chisq.test(gen_data_2$X6277, gen_data_2$pp_risk)
chi_1_3x

con_tab = table(gen_data_2$X6277, gen_data_2$pp_risk)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_1_3x$parameter, sig.level = .05)

# PP + cog impair
chi_1_4x = chisq.test(gen_data_2$X6277, gen_data_2$pp_cog)
chi_1_4x

con_tab = table(gen_data_2$X6277, gen_data_2$pp_cog)
con_tab

pwr.chisq.test(w = .1, N = sum(con_tab), df = chi_1_4x$parameter, sig.level = .05)

# All together
chi_1_5x = chisq.test(gen_data_2$X6277, gen_data_2$all_pp)
chi_1_5x

con_tab = table(gen_data_2$X6277, gen_data_2$all_pp)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_1_5x$parameter, sig.level = .05)


## rs1076560

# All separate
chi_2_1x = chisq.test(gen_data_2$X1076560, gen_data_2$Strategy)
chi_2_1x

con_tab = table(gen_data_2$X1076560, gen_data_2$Strategy)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_1x$parameter, sig.level = .05)

# Only pure PP
chi_2_2x = chisq.test(gen_data_2$X1076560, gen_data_2$only_pp)
chi_2_2x

con_tab = table(gen_data_2$X1076560, gen_data_2$only_pp)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_2x$parameter, sig.level = .05)

# PP + riskloving
chi_2_3x = chisq.test(gen_data_2$X1076560, gen_data_2$pp_risk)
chi_2_3x

con_tab = table(gen_data_2$X1076560, gen_data_2$pp_risk)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_3x$parameter, sig.level = .05)

# PP + cog impair
chi_2_4x = chisq.test(gen_data_2$X1076560, gen_data_2$pp_cog)
chi_2_4x

con_tab = table(gen_data_2$X1076560, gen_data_2$pp_cog)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_4x$parameter, sig.level = .05)

# All together
chi_2_5x = chisq.test(gen_data_2$X1076560, gen_data_2$all_pp)
chi_2_5x

con_tab = table(gen_data_2$X1076560, gen_data_2$all_pp)
con_tab

pwr.chisq.test(w = .3, N = sum(con_tab), df = chi_2_5x$parameter, sig.level = .05)



