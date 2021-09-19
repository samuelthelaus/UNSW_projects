
library(lme4)
library(Rmisc)
library(EnvStats)

gen_rt_data = read.csv('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Data/RT_Data.csv')

gen_rt_data$Sample.ID = as.character(gen_rt_data$Sample.ID)
gen_rt_data$excluded = 0

for(i in 1:nrow(new_gen_data)) {
  if(new_gen_data$excluded[i] == 1) {
    gen_rt_data$excluded[gen_rt_data$Sample.ID == as.character(new_gen_data$Sample.ID[i])] = 1
  }
}

gen_rt_data = gen_rt_data[gen_rt_data$excluded == 0, ]

# Labeling
comp = gen_rt_data[1, ]
comp$Session_Number = NA
comp$Time = NA
comp$Trial = NA
comp$Trial_From_BS = NA
comp$First_Bet = NA
comp$PP_By_Session = NA
comp$Out_Session = NA
comp$Out_Total = NA
j = 1
for(sub in unique(gen_rt_data$Sample.ID)) {
    
    sub_data = gen_rt_data[gen_rt_data$Sample.ID == sub, ]
    j = 1
    appNum = 0
    masNum = 0
    Total_Out = 0
    df = data.frame(Session_Number = 1, Time = 1, Trial = 1, Trial_From_BS = 1, 
                    First_Bet = 1, PP_By_Session = 1, Out_Session = 1)
    
    for(i in seq(from=20, to=nrow(sub_data), by=20)) {
      
      x = sub_data[j:i, ]
      
      if(x$Apprentice[1] == 1) {
        appNum = appNum + 1
        x$Ses = rep(appNum, nrow(x))
      } else {
        masNum = masNum + 1
        x$Ses = rep(masNum, nrow(x))
      }
      
      x$Picked = 0
      x$Time = 'Before'
      x$Trial_From_BS = NA
      if(any(abs(x$Shot) > 9 & x$Outcome == -40)) {
        for(k in 1:nrow(x)) {
          if(abs(x$Shot[k]) > 9 & x$Outcome[k] == -40) {
            break
          }
        }
        
        x$Trial_From_BS = 1:20 - k
        if(k == 1) {
          x$Time[2:nrow(x)] = 'After'
        } else if(k != nrow(x)) {
          x$Time[(k + 1):nrow(x)] = 'After'
        }
        
          if(sum(x$Bet[x$Time == 'After']) > 0) {
            x$Picked = 1
          }
      }
      
      FirstBet = NA
      for(k in 1:nrow(x)) {
        if(x$Bet[k] == 1) {
          if(k < 3) {
            FirstBet = 'Early'
          } else {
            FirstBet = 'Late'
          }
          break
        }
        if(k == nrow(x)) {
          FirstBet = 'Never'
        }
      }
      
      outliers = sum(abs(x$Shot) > 9)
      Total_Out = Total_Out + outliers
      
      y = data.frame(Session_Number = x$Ses, Time = x$Time, 
                     Trial = 1:nrow(x), Trial_From_BS = x$Trial_From_BS,
                     First_Bet = FirstBet, 
                     PP_By_Session = x$Picked, 
                     Out_Session = rep(outliers, nrow(x)))
      
      df = rbind(df, y)
      j = i + 1
      
    }
    
    df = df[2:nrow(df), ]
    df$Out_Total = Total_Out
    
    sub_data = cbind(sub_data, df)
    
    comp = rbind(comp, sub_data)
  }

comp = comp[2:nrow(comp), ]


# Categorize penny pickers
for(j in unique(comp$Sample.ID)) {
  if(sum(comp$Bet[comp$Sample.ID == j & comp$Apprentice == 1]) > 1) {
    comp$PP[comp$Sample.ID == j] = 1
  } else {
    comp$PP[comp$Sample.ID == j] = 0
  }
}

comp = comp[comp$RT > 0, ]

# Categorize penny pickers
for(j in unique(comp$Sample.ID)) {
  if(sum(comp$Bet[comp$Sample.ID == j & comp$Apprentice == 1 &
                  comp$Time == 'After']) > 1) {
    comp$PP[comp$Sample.ID == j] = 1
  } else {
    comp$PP[comp$Sample.ID == j] = 0
  }
}

comp$logRT = log(comp$RT)
comp$sqrRT = sqrt(comp$RT)

# Box-cox transform
boxout = boxcox(comp$RT, objective.name = 'Log-Likelihood', lambda = seq(-5, 5, by = .1))
print(cbind(boxout$lambda, boxout$objective))

comp$bcRT = boxcoxTransform(comp$RT, lambda = -0.6)

# Test difference in RT between PP/Other
agg_rt = aggregate(bcRT ~ Sample.ID + PP + Apprentice, data = comp, FUN = mean)

agg_rt = agg_rt[agg_rt$Apprentice == 1, ]

t.test(agg_rt$logRT, agg_rt$X7m_pp)

sd(agg_rt$logRT[agg_rt$X7m_pp == 0], na.rm = T)


# Move PP over to new_gen_data
for(id in agg_rt$Sample.ID) {
  if(id %in% new_gen_data$Sample.ID) {
    new_gen_data$PP_9[new_gen_data$Sample.ID == id] = agg_rt$PP[agg_rt$Sample.ID == id]
  }
} 

by_session = data.frame(logRT = 1, Apprentice = 1, Bet = 1, PP = 1, ID = 'b')
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  x = comp[j:i, ]
  if(sum(x$Bet, na.rm = T) > 0) {
    didBet = 1
  } else {
    didBet = 0
  }
  
  y = data.frame(logRT = mean(x$logRT, na.rm = TRUE), Apprentice = x$Apprentice[1],
                 Bet = didBet, PP = x$X7m_pp[1],
                 ID = x$Sample.ID[1])
  
  by_session = rbind(by_session, y)
  
  j = i + 1
}

by_session = by_session[2:nrow(by_session), ]


ses_apprentice = by_session[by_session$Apprentice == 1, ]
ses_master = by_session[by_session$Apprentice == 0, ]

ses_app_agg = aggregate(logRT ~ Bet + PP + ID, data = ses_apprentice,
                        FUN = mean)

t.test(ses_app_agg$logRT[ses_app_agg$Bet == 1 & ses_app_agg$PP == 1],
       ses_app_agg$logRT[ses_app_agg$Bet == 0 & ses_app_agg$PP == 1])

rt_mod_ses_app = lmer(logRT ~ Bet * PP + (1 | ID), data = ses_apprentice)
rt_mod_ses_mas = lmer(logRT ~ Bet * PP + (1 | ID), data = ses_master)

summary(rt_mod_ses_mas)


rt_mod_trial = lmer(RT ~ Bet * X7m_pp + 
                      (1 | Sample.ID), 
                    data = gen_rt_data[gen_rt_data$Apprentice == 1,])

summary(rt_mod_trial)



# Plot interaction
summaryRT_app <- summarySE(ses_apprentice, measurevar = 'logRT', groupvars = c('PP', 'Bet'))
summaryRT_app$PP = factor(summaryRT_app$PP, levels = c(0, 1), 
                          labels = c('Others', 'Penny pickers'))
summaryRT_app$Bet = factor(summaryRT_app$Bet, levels = c(0, 1),
                                     labels = c('No', 'Yes'))

ggplot(summaryRT_app, aes(PP, logRT)) + 
  geom_point(aes(color = Bet), size = .75) + 
  geom_line(aes(color = Bet, group = Bet)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = Bet), 
                width = 0.15) + 
  labs(x = 'Group', y = 'Mean log RT') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 50, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_rt_app.png')


## For plots within sessions
plot_session_if_bet = data.frame(logRT = 1, Row = 1, Bet = 1, PP = 1, ID = 1)
plot_session_by_bet = data.frame(logRT = 1, Row = 1, Bet = 1, PP = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After) & comp$Apprentice == 1)) {
  x = comp[j:i, ]
  
  if(any(abs(x$Shot) > 7)) {
    for(k in 1:nrow(x)) {
      if(abs(x$Shot[k]) > 7) {
        j = k
        break
      }
    }
    if(j + 10 < nrow(x)) {
      x = x[j:(j + 10), ]
    } else {
      x = x[j:nrow(x), ]
    }
    if(sum(x$Bet, na.rm = T) > 0) {
      didBet = 1
    } else {
      didBet = 0
    }
    y = data.frame(logRT = x$logRT, Row = 1:nrow(x),
                   Bet = rep(didBet, nrow(x)), PP = x$X7m_pp,
                   ID = x$Sample.ID)
    z = data.frame(logRT = x$logRT, Row = 1:nrow(x),
                   Bet = x$Bet, PP = x$X7m_pp,
                   ID = x$Sample.ID)
    
    plot_session_if_bet = rbind(plot_session_if_bet, y)
    plot_session_by_bet = rbind(plot_session_by_bet, z)
  }
  j = i + 1
}

plot_session_if_bet = plot_session_if_bet[2:nrow(plot_session_if_bet), ]
plot_session_by_bet = plot_session_by_bet[2:nrow(plot_session_by_bet), ]

plot_session_by_bet = plot_session_by_bet[plot_session_by_bet$PP == 1, ]
plot_session_if_bet = plot_session_if_bet[plot_session_if_bet$PP == 1, ]


summary_after_out <- summarySE(plot_session_if_bet, 
                               measurevar = 'logRT', 
                               groupvars = c('Row', 'PP'))
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                           labels = c('Others', 'PP'))

ggplot(summary_after_out, aes(Row, logRT)) + 
  geom_point(aes(color = PP), size = .75) + 
  geom_line(aes(color = PP, group = PP)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = PP), 
                width = 0.15) + 
  #scale_y_continuous(limits = c(0.6, 0.85)) +
  labs(x = 'Group', y = 'Mean log RT', title = 'Comparison PP vs others') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('/Users/sam/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_rt_split_by_pp.png')



# Modeling with data after outlier
inter_df_with = data.frame(logRT = 1, Bet = 1, PP = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After) & comp$Apprentice == 1)) {
  x = comp[j:i, ]
  
  if(any(abs(x$Shot) > 7)) {
    for(k in 1:nrow(x)) {
      if(abs(x$Shot[k]) > 7) {
        j = k
        break
      }
    }
    if(j + 10 < nrow(x)) {
      x = x[j:(j + 10), ]
    } else {
      x = x[j:nrow(x), ]
    }
    if(sum(x$Bet, na.rm = T) > 0) {
      didBet = 1
    } else {
      didBet = 0
    }
    y = data.frame(logRT = x$logRT, Bet = didBet, PP = x$X7m_pp, ID = x$Sample.ID)
    
    inter_df_with = rbind(inter_df_with, y)
  } 
  j = i + 1
}

inter_df_with = inter_df_with[2:nrow(inter_df_with), ]

lmer_after_outlier = lmer(logRT ~ Bet * PP + (1 | ID), data = inter_df_with)

summary(lmer_after_outlier)

# Without outlier
inter_df_without = data.frame(logRT = 1, Bet = 1, PP = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  x = comp[j:i, ]
  
  if(!any(abs(x$Shot) > 7)) {
    
    if(sum(x$Bet, na.rm = T) > 0) {
      didBet = 1
    } else {
      didBet = 0
    }
    y = data.frame(logRT = x$logRT, Bet = didBet, PP = x$X7m_pp, ID = x$Sample.ID)
    
    inter_df_without = rbind(inter_df_without, y)
  } 
  j = i + 1
}

inter_df_without = inter_df_without[2:nrow(inter_df_without), ]

lmer_no_outlier = lmer(logRT ~ Bet * PP + (1 | ID), data = inter_df_without)

summary(lmer_after_outlier)


# Test differences from boxplots

# Master
master_boxplot = data.frame(logRT = 1, Bet = 1, PP = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  
  if(comp$Apprentice[i] == 0) {
    x = comp[j:i, ]
    
    if(sum(x$Bet, na.rm = T) > 0) {
      didBet = 'Yes'
    } else {
      didBet = 'No'
    }
    
    if(x$X7m_pp[1] == 1) {
      pp = 'Penny Picker'
    } else {
      pp = 'Other'
    }
    
    y = data.frame(logRT = x$logRT, 
                   Bet = didBet, PP = pp, ID = x$Sample.ID)
    
    master_boxplot = rbind(master_boxplot, y)
    
    j = i + 1
  } else {
    j = i + 1
  }
}

master_boxplot = master_boxplot[2:nrow(master_boxplot), ]

master_box_lmer = lmer(logRT ~ Bet * PP + (1 | ID), data = master_boxplot)

summary(master_box_lmer)

# Apprentice
before_after_boxplot = data.frame(logRT = 1, Bet = 1, PP = 1, Time = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After) & comp$Apprentice == 1)) {
  x = comp[j:i, ]
  
  if(any(abs(x$Shot) > 7)) {
    for(k in 1:nrow(x)) {
      if(abs(x$Shot[k]) > 7) {
        j = k
        break
      }
    }
    
    x1 = x[1:(j - 1), ]
    x2 = x[j:nrow(x), ]
    
    if(sum(x1$Bet, na.rm = T) > 0) {
      didBet1 = 'Yes'
    } else {
      didBet1 = 'No'
    }
    if(sum(x2$Bet, na.rm = T) > 0) {
      didBet2 = 'Yes'
    } else {
      didBet2 = 'No'
    }
    
    if(x$X7m_pp[1] == 1) {
      pp = 'Penny Picker'
    } else {
      pp = 'Other'
    }
    
    y = data.frame(logRT = c(mean(x1$logRT), mean(x2$logRT)), 
                   Bet = c(didBet1, didBet2), PP = rep(pp, 2),
                   Time = c('Before', 'After'), ID = rep(x1$Sample.ID[1], 2))
    
    before_after_boxplot = rbind(before_after_boxplot, y)
  } 
  j = i + 1
}

before_after_boxplot = before_after_boxplot[2:nrow(before_after_boxplot), ]

before_boxplot = before_after_boxplot[before_after_boxplot$Time == 'Before', ]
after_boxplot = before_after_boxplot[before_after_boxplot$Time == 'After', ]


before_box_lmer = lmer(logRT ~ Bet * PP + (1 | ID), data = before_boxplot)
after_box_lmer = lmer(logRT ~ Bet * PP + (1 | ID), data = after_boxplot)

summary(before_box_lmer)
summary(after_box_lmer)



# Test differences from boxplots BY TRIAL

# Master
master_boxplot = data.frame(logRT = 1, bcRT = 1, Bet = 1, PP = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  
  if(comp$Apprentice[i] == 0) {
    x = comp[j:i, ]
    
    x$Bet[x$Bet == 1] = 'Yes'
    x$Bet[x$Bet == 0] = 'No'
    
    if(x$X7m_pp[1] == 1) {
      pp = 'Penny Picker'
    } else {
      pp = 'Other'
    }
    
    y = data.frame(logRT = x$logRT, bcRT = x$bcRT,
                   Bet = x$Bet, PP = pp, ID = x$Sample.ID)
    
    master_boxplot = rbind(master_boxplot, y)
    
    j = i + 1
  } else {
    j = i + 1
  }
}

master_boxplot = master_boxplot[2:nrow(master_boxplot), ]

master_box_lmer = lmer(logRT ~ Bet * PP + (1 | ID), data = master_boxplot)
master_box_lmer = lmer(bcRT ~ Bet * PP + (1 | ID), data = master_boxplot)

summary(master_box_lmer)

# Apprentice
before_after_boxplot = data.frame(logRT = 1, bcRT = 1, Bet = 1, PP = 1, Time = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  
  if(comp$Apprentice[i] == 1){
    x = comp[j:i, ]
    
    if(any(abs(x$Shot) > 7)) {
      for(k in 1:nrow(x)) {
        if(abs(x$Shot[k]) > 7) {
          j = k
          break
        }
      }
      
      x$Time = 'Before'
      if(j == 1) {
        x$Time[2:nrow(x)] = 'After'
      } else if(j != nrow(x)) {
        x$Time[(j + 1):nrow(x)] = 'After'
      }
      
      x$Bet[x$Bet == 1] = 'Yes'
      x$Bet[x$Bet == 0] = 'No'
      
      if(x$X7m_pp[1] == 1) {
        pp = 'Penny Picker'
      } else if(x$X7m_pp[1] == 0){
        pp = 'Other'
      }
      
      y = data.frame(logRT = x$logRT, bcRT = x$bcRT, Bet = x$Bet, 
                     PP = rep(pp, nrow(x)), Time = x$Time, 
                     ID = x$Sample.ID)
      
      before_after_boxplot = rbind(before_after_boxplot, y)
    } 
    j = i + 1
  } else {
    j = i + 1
  }
}

before_after_boxplot = before_after_boxplot[2:nrow(before_after_boxplot), ]

before_boxplot = before_after_boxplot[before_after_boxplot$Time == 'Before', ]
after_boxplot = before_after_boxplot[before_after_boxplot$Time == 'After', ]


before_box_lmer = lmer(logRT ~ Bet * PP + (1 | ID), data = before_boxplot)
after_box_lmer = lmer(logRT ~ Bet + PP + (1 | ID), data = after_boxplot)

before_box_lmer = lmer(bcRT ~ Bet * PP + (1 | ID), data = before_boxplot)
after_box_lmer = lmer(bcRT ~ Bet + PP + (1 | ID), data = after_boxplot)

summary(before_box_lmer)
summary(after_box_lmer)


# Quantiles analysis

q_diff = data.frame(bcRT = 1, Bet = 1, PP = 1, Quant = 1, ID = 1)
j = 1
pp_q = quantile(comp$bcRT[comp$X7m_pp == 1 & comp$Apprentice == 0], probs = seq(0, 1, .2))
oth_q = quantile(comp$bcRT[comp$X7m_pp == 0 & comp$Apprentice == 0], probs = seq(0, 1, .2))
for(i in which(!is.na(comp$Answer.After))) {
  
  if(comp$Apprentice[i] == 0) {
    x = comp[j:i, ]
    
    x$Bet[x$Bet == 1] = 'Yes'
    x$Bet[x$Bet == 0] = 'No'
    
    if(x$X7m_pp[1] == 1) {
      quant_list = pp_q
      pp = 'Penny Picker'
    } else {
      quant_list = oth_q
      pp = 'Other'
    }
    
    q_list = c()
    for(val in x$bcRT) {
      if(val < quant_list[2]) {
        q_list[length(q_list) + 1] = 0.2
        next
      } else if(val < quant_list[3]) {
        q_list[length(q_list) + 1] = 0.4
        next
      } else if(val < quant_list[4]) {
        q_list[length(q_list) + 1] = 0.6
        next
      } else if(val < quant_list[5]) {
        q_list[length(q_list) + 1] = 0.8
        next
      } else {
        q_list[length(q_list) + 1] = 1
      }
    }
    
    y = data.frame(bcRT = x$bcRT, Bet = x$Bet, PP = pp, 
                   Quant = q_list, ID = x$Sample.ID)
    
    q_diff = rbind(q_diff, y)
    
    j = i + 1
  } else {
    j = i + 1
  }
}

q_diff = q_diff[2:nrow(q_diff), ]


q_diff_mod_1 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff[q_diff$Quant == 0.2, ])
q_diff_mod_2 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff[q_diff$Quant == 0.4, ])
q_diff_mod_3 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff[q_diff$Quant == 0.6, ])
q_diff_mod_4 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff[q_diff$Quant == 0.8, ])
q_diff_mod_5 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff[q_diff$Quant == 1, ])

summary(q_diff_mod_5)


# With apprentice
q_diff_ba = data.frame(bcRT = 1, Bet = 1, PP = 1, Time = 1, Quant = 1, ID = 1)
pp_q = quantile(comp$bcRT[comp$X7m_pp == 1 & comp$Apprentice == 1], probs = seq(0, 1, .2))
oth_q = quantile(comp$bcRT[comp$X7m_pp == 0 & comp$Apprentice == 1], probs = seq(0, 1, .2))
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  
  if(comp$Apprentice[i] == 1){
    x = comp[j:i, ]
    
    if(any(abs(x$Shot) > 7)) {
      for(k in 1:nrow(x)) {
        if(abs(x$Shot[k]) > 7) {
          j = k
          break
        }
      }
      
      x$Time = 'Before'
      if(j == 1) {
        x$Time[2:nrow(x)] = 'After'
      } else if(j != nrow(x)) {
        x$Time[(j + 1):nrow(x)] = 'After'
      }
      
      x$Bet[x$Bet == 1] = 'Yes'
      x$Bet[x$Bet == 0] = 'No'
      
      if(x$X7m_pp[1] == 1) {
        quant_list = pp_q
        pp = 'Penny Picker'
      } else {
        quant_list = oth_q
        pp = 'Other'
      }
      
      q_list = c()
      for(val in x$bcRT) {
        if(val < quant_list[2]) {
          q_list[length(q_list) + 1] = 0.2
          next
        } else if(val < quant_list[3]) {
          q_list[length(q_list) + 1] = 0.4
          next
        } else if(val < quant_list[4]) {
          q_list[length(q_list) + 1] = 0.6
          next
        } else if(val < quant_list[5]) {
          q_list[length(q_list) + 1] = 0.8
          next
        } else {
          q_list[length(q_list) + 1] = 1
        }
      }
      
      y = data.frame(bcRT = x$bcRT, Bet = x$Bet, 
                     PP = rep(pp, nrow(x)), Time = x$Time, 
                     Quant = q_list, ID = x$Sample.ID)
      
      q_diff_ba = rbind(q_diff_ba, y)
    } 
    j = i + 1
  } else {
    j = i + 1
  }
}

q_diff_ba = q_diff_ba[2:nrow(q_diff_ba), ]

q_diff_before = q_diff_ba[q_diff_ba$Time == 'Before', ]
q_diff_after = q_diff_ba[q_diff_ba$Time == 'After', ]


q_diff_mod_ba_1 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff_before[q_diff_before$Quant == 0.2, ])
q_diff_mod_ba_2 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff_before[q_diff_before$Quant == 0.4, ])
q_diff_mod_ba_3 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff_before[q_diff_before$Quant == 0.6, ])
q_diff_mod_ba_4 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff_before[q_diff_before$Quant == 0.8, ])
q_diff_mod_ba_5 = lmer(bcRT ~ Bet * PP + (1 | ID), data = q_diff_before[q_diff_before$Quant == 1, ])

summary(q_diff_mod_ba_5)

q_diff_mod_af_1 = lmer(bcRT ~ Bet + PP + (1 | ID), data = q_diff_after[q_diff_after$Quant == 0.2, ])
q_diff_mod_af_2 = lmer(bcRT ~ Bet + PP + (1 | ID), data = q_diff_after[q_diff_after$Quant == 0.4, ])
q_diff_mod_af_3 = lmer(bcRT ~ Bet + PP + (1 | ID), data = q_diff_after[q_diff_after$Quant == 0.6, ])
q_diff_mod_af_4 = lmer(bcRT ~ Bet + PP + (1 | ID), data = q_diff_after[q_diff_after$Quant == 0.8, ])
q_diff_mod_af_5 = lmer(bcRT ~ Bet + PP + (1 | ID), data = q_diff_after[q_diff_after$Quant == 1, ])

summary(q_diff_mod_af_5)


