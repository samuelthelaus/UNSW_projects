
library(Rmisc)
library(ggplot2)
library(dplyr)

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
                               groupvars = c('Row', 'Bet'))
summary_after_out$Bet = factor(summary_after_out$Bet, levels = c(0, 1),
                              labels = c('No', 'Yes'))

ggplot(summary_after_out, aes(Row, logRT)) + 
  geom_point(aes(color = Bet), size = .75) + 
  geom_line(aes(color = Bet, group = Bet)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = Bet), 
                width = 0.15) + 
  #scale_y_continuous(limits = c(0.6, 0.85)) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  labs(x = 'Trial', y = 'Mean log RT', title = 'If bet placed after outlier') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_logRT_if_bet.png',
       width = 6.5, height = 4.5)



## PP vs others before and after outlier

plot_session_by_pp = data.frame(logRT = 1, Row = 1, PP = 1)
plot_before_after = data.frame(logRT = 1, Time = 1, PP = 1)
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
      x1 = x[j:(j + 10), ]
    } else {
      x1 = x[j:nrow(x), ]
    }
    
    if(j - 5 > 1) {
      x2 = x[(j - 5):(j - 1), ]
    } else {
      x2 = x[1:(j - 1), ]
    }
    x = rbind(x2, x1)
    
    row = c(1:nrow(x2) - (nrow(x2) + 1), 1:nrow(x1) - 1)
      
    y = data.frame(logRT = x$logRT, Row = row, PP = x$X7m_pp)
    z = data.frame(logRT = c(mean(x2$logRT), mean(x1$logRT)), Time = c('Before', 'After'),
                   PP = rep(x$X7m_pp[1], 2))
    
    plot_session_by_pp = rbind(plot_session_by_pp, y)
    plot_before_after = rbind(plot_before_after, z)
  }
  j = i + 1
}

plot_session_by_pp = plot_session_by_pp[2:nrow(plot_session_by_pp), ]
plot_before_after = plot_before_after[2:nrow(plot_before_after), ]

# Continuous
summary_after_out <- summarySE(plot_session_by_pp, 
                               measurevar = 'logRT', 
                               groupvars = c('Row', 'PP'))
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                               labels = c('Others', 'Penny picker'))

ggplot(summary_after_out, aes(Row, logRT)) + 
  geom_point(aes(color = PP), size = .75) + 
  geom_line(aes(color = PP, group = PP)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = PP), 
                width = 0.15) +
  geom_vline(xintercept = 0, alpha = .5, linetype = 'dashed') +
  labs(x = 'Trial from outlier', y = 'Mean log RT', title = 'Comparison PP vs others') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_logRT_by_pp.png',
       width = 6.5, height = 4.5)


# Categorical
summary_after_out <- summarySE(plot_before_after, 
                               measurevar = 'logRT', 
                               groupvars = c('Time', 'PP'))
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                              labels = c('Others', 'Penny picker'))

ggplot(summary_after_out, aes(PP, logRT)) + 
  geom_point(aes(color = Time), size = .75) + 
  geom_line(aes(color = Time, group = Time)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = Time), 
                width = 0.15) +
  labs(x = 'Group', y = 'Mean log RT', title = 'Comparison PP vs others') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_logRT_by_pp_categorical.png',
       width = 6.5, height = 4.5)



## By bins
plot_by_bins_3 = data.frame(logRT = 1, Bin = 1, PP = 1)
plot_by_bins_2 = data.frame(logRT = 1, Bin = 1, PP = 1)
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
    
    if(j < nrow(x)) {
      before = mean(x$logRT[1:j])
      after = mean(x$logRT[(j + 1):nrow(x)])
      
      if(sum(x$Bet[(j + 1):nrow(x)]) < 4) {
        bet3 = 'Less than 4'
        bet2 = 'Less than 4'
      } else if(sum(x$Bet[(j + 1):nrow(x)]) > 3 & sum(x$Bet[(j + 1):nrow(x)]) < 9) {
        bet3 = '4 to 8'
        bet2 = '4 or more'
      } else {
        bet3 = '9 or more'
        bet2 = '4 or more'
      }
      
      y = data.frame(logRT = c(before, after), Bin = c('Before', bet3), PP = rep(x$X7m_pp[1], 2))
      z = data.frame(logRT = c(before, after), Bin = c('Before', bet2), PP = rep(x$X7m_pp[1], 2))
      
      plot_by_bins_3 = rbind(plot_by_bins_3, y)
      plot_by_bins_2 = rbind(plot_by_bins_2, z)
    }
  }
  j = i + 1
}

plot_by_bins_3 = plot_by_bins_3[2:nrow(plot_by_bins_3), ]
plot_by_bins_2 = plot_by_bins_2[2:nrow(plot_by_bins_2), ]



# 3 bins
summary_after_out <- summarySE(plot_by_bins_3, 
                               measurevar = 'logRT', 
                               groupvars = c('Bin', 'PP'))
summary_after_out$se[is.na(summary_after_out$se)] = 0
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                              labels = c('Others', 'Penny picker'))
summary_after_out$Bin = factor(summary_after_out$Bin, levels = c('Before', 'Less than 4',
                                                                 '4 to 8', '9 or more'))

ggplot(summary_after_out, aes(Bin, logRT)) + 
  geom_bar(stat = 'identity', aes(fill = PP), width = .8,
           position = 'dodge') +
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, fill = PP), 
                width = 0.25, position = position_dodge(0.8)) +
  labs(x = 'Bin', y = 'Mean log RT', title = 'Comparison PP vs others in 3 bins') +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 320, size = 10, vjust = 1.2, hjust = -.1)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/bar_plot_3_bins.png',
       width = 7, height = 4)


# 2 bins
summary_after_out <- summarySE(plot_by_bins_2, 
                               measurevar = 'logRT', 
                               groupvars = c('Bin', 'PP'))
summary_after_out$se[is.na(summary_after_out$se)] = 0
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                              labels = c('Others', 'Penny picker'))
summary_after_out$Bin = factor(summary_after_out$Bin, levels = c('Before', 'Less than 4',
                                                                 '4 or more'))

ggplot(summary_after_out, aes(Bin, logRT)) + 
  geom_bar(stat = 'identity', aes(fill = PP), width = .8,
           position = 'dodge') +
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, fill = PP), 
                width = 0.25, position = position_dodge(0.8)) +
  labs(x = 'Bin', y = 'Mean log RT', title = 'Comparison PP vs others in 2 bins') +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 320, size = 10, vjust = 1.2, hjust = -.1)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/bar_plot_2_bins.png',
       width = 7, height = 4)



## Histograms

# Bet vs skip
comp_plot = comp
comp_plot$PP = factor(comp_plot$X7m_pp, levels = c(0, 1),
                          labels = c('Others', 'Penny pickers'))
comp_plot$Betting = factor(comp_plot$Bet, levels = c(0, 1),
                           labels = c('No', 'Yes'))
comp_plot = comp_plot[
  #(abs(comp_plot$logRT) < 3 * sd(comp_plot$logRT)) &
                        (comp_plot$Apprentice == 1), ]

# Pooled
mu <- ddply(comp_plot, "Betting", summarise, grp.mean=mean(logRT))

ggplot(comp_plot, aes(x=logRT, color=Betting, fill=Betting)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha = 1) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Betting),
             linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Reaction Time', y = 'Density', title = 'Bet vs Skip Pooled') +
  theme(legend.position="right")

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/hist_betskip_pooled.png',
       width = 7, height = 4)

# PP
pp_comp_plot = comp_plot[comp_plot$PP == 'Penny pickers', ]

mu <- ddply(pp_comp_plot, "Betting", summarise, grp.mean=mean(logRT))

ggplot(pp_comp_plot, aes(x=logRT, color=Betting, fill=Betting)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha = 1) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Betting),
             linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Reaction Time', y = 'Density', title = 'Bet vs Skip only PP') +
  theme(legend.position="right")

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/hist_betskip_pp.png',
       width = 7, height = 4)

# Others
others_comp_plot = comp_plot[comp_plot$PP == 'Others', ]

mu <- ddply(others_comp_plot, "Betting", summarise, grp.mean=mean(logRT))

ggplot(others_comp_plot, aes(x=logRT, color=Betting, fill=Betting)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha = 1) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Betting),
             linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Reaction Time', y = 'Density', title = 'Bet vs Skip only Others') +
  theme(legend.position="right")

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/hist_betskip_others.png',
       width = 7, height = 4)


# Before and after

## By bins
before_after_dist = data.frame(logRT = 1, Time = 1, PP = 1)
j = 1
comp_use = comp[(comp$logRT > 0), ]
for(i in which(!is.na(comp_use$Answer.After) & comp_use$Apprentice == 1)) {
  x = comp_use[j:i, ]
  
  if(any(abs(x$Shot) > 7)) {
    for(k in 1:nrow(x)) {
      if(abs(x$Shot[k]) > 7) {
        j = k
        break
      }
    }
    
    if(j < nrow(x)) {
      before = mean(x$logRT[1:j])
      after = mean(x$logRT[(j + 1):nrow(x)])
      
      y = data.frame(logRT = c(before, after), Time = c('Before', 'After'), PP = rep(x$X7m_pp[1], 2))
      
      before_after_dist = rbind(before_after_dist, y)
    }
  }
  j = i + 1
}

before_after_dist = before_after_dist[2:nrow(before_after_dist), ]

ba_pp = before_after_dist[before_after_dist$PP == 1, ]
ba_others = before_after_dist[before_after_dist$PP == 0, ]

# PP
mu <- ddply(ba_pp, 'Time', summarise, grp.mean=mean(logRT))

ggplot(ba_pp, aes(x=logRT, color=Time, fill=Time)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha = .5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Time),
             linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Reaction Time', y = 'Density', title = 'Before vs After outlier for PP') +
  theme(legend.position="right")

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/before_after_pp.png',
       width = 7, height = 4)


# Others
mu <- ddply(ba_others, 'Time', summarise, grp.mean=mean(logRT))

ggplot(ba_others, aes(x=logRT, color=Time, fill=Time)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha = .5) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Time),
             linetype="dashed") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(x = 'Reaction Time', y = 'Density', title = 'Before vs After outlier for Others') +
  theme(legend.position="right")

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/before_after_others.png',
       width = 7, height = 4)




## Figure 1 restricted to trials after outlier
interaction_plot_with_outlier = data.frame(logRT = 1, Bet = 1, PP = 1)
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
    y = data.frame(logRT = mean(x$logRT), Bet = didBet, PP = x$X7m_pp[1])
    
    interaction_plot_with_outlier = rbind(interaction_plot_with_outlier, y)
  } 
  j = i + 1
}

interaction_plot_with_outlier = interaction_plot_with_outlier[2:nrow(interaction_plot_with_outlier), ]

summary_after_out <- summarySE(interaction_plot_with_outlier, 
                               measurevar = 'logRT', 
                               groupvars = c('PP', 'Bet'))
summary_after_out$Bet = factor(summary_after_out$Bet, levels = c(0, 1),
                               labels = c('No', 'Yes'))
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                              labels = c('Others', 'Penny picker'))

ggplot(summary_after_out, aes(PP, logRT)) + 
  geom_point(aes(color = Bet), size = .75) + 
  geom_line(aes(color = Bet, group = Bet)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = Bet), 
                width = 0.15) + 
  #scale_y_continuous(limits = c(0.6, 0.85)) +
  #scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  labs(x = 'Group', y = 'Mean log RT', title = 'Bet vs Skip after outlier') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_restricted.png',
       width = 6.5, height = 4.5)


# Without outlier
interaction_plot_no_outlier = data.frame(logRT = 1, Bet = 1, PP = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  x = comp[j:i, ]
  
  if(!any(abs(x$Shot) > 7)) {
    
    if(sum(x$Bet, na.rm = T) > 0) {
      didBet = 1
    } else {
      didBet = 0
    }
    y = data.frame(logRT = mean(x$logRT), Bet = didBet, PP = x$X7m_pp[1])
    
    interaction_plot_no_outlier = rbind(interaction_plot_no_outlier, y)
  } 
  j = i + 1
}

interaction_plot_no_outlier = interaction_plot_no_outlier[2:nrow(interaction_plot_no_outlier), ]

summary_after_out <- summarySE(interaction_plot_no_outlier, 
                               measurevar = 'logRT', 
                               groupvars = c('PP', 'Bet'))
summary_after_out$se[is.na(summary_after_out$se)] = 0
summary_after_out$Bet = factor(summary_after_out$Bet, levels = c(0, 1),
                               labels = c('No', 'Yes'))
summary_after_out$PP = factor(summary_after_out$PP, levels = c(0, 1),
                              labels = c('Others', 'Penny picker'))

ggplot(summary_after_out, aes(PP, logRT)) + 
  geom_point(aes(color = Bet), size = .75) + 
  geom_line(aes(color = Bet, group = Bet)) + 
  theme_classic() +
  geom_errorbar(aes(ymin = logRT - se, ymax = logRT + se, color = Bet), 
                width = 0.15) + 
  #scale_y_continuous(limits = c(0.6, 0.85)) +
  #scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  labs(x = 'Group', y = 'Mean log RT', title = 'Bet vs Skip after outlier') +
  theme(legend.position = 'right') + 
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = .6)) + 
  theme(axis.title.y = element_text(vjust = .4))

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/interaction_plot_restricted_nooutlier.png',
       width = 6.5, height = 4.5)




## Plot before after master vs apprentice

# Master
master_boxplot = data.frame(logRT = 1, bcRT = 1, Bet = 1, PP = 1, ID = 1)
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
    
    y = data.frame(logRT = x$logRT, bcRT = x$bcRT,
                   Bet = didBet, PP = pp, ID = x$Sample.ID)
    
    master_boxplot = rbind(master_boxplot, y)
    
    j = i + 1
  } else {
    j = i + 1
  }
}

master_boxplot = master_boxplot[2:nrow(master_boxplot), ]


ggplot(master_boxplot, aes(x = PP, y = bcRT)) +
  geom_boxplot(aes(color = Bet), width = 0.5, size = 0.4,
               position = position_dodge(0.8)) +
  labs(x = 'Group', y = 'Mean Box-Cox RT', title = 'With master') +
  scale_fill_manual(values = c("#FF3636", "#0057A0")) +
  scale_color_manual(values = c("#FF3636", "#0057A0")) +
  theme_classic()
  
ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/boxplot_master_no_cog.png',
       width = 7, height = 4)

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

# Before
ggplot(before_boxplot, aes(x = PP, y = bcRT)) +
  geom_boxplot(aes(color = Bet), width = 0.5, size = 0.4,
               position = position_dodge(0.8)) +
  labs(x = 'Group', y = 'Mean Box-Cox RT', title = 'Before outlier') +
  scale_fill_manual(values = c("#FF3636", "#0057A0")) +
  scale_color_manual(values = c("#FF3636", "#0057A0")) +
  theme_classic()

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/with excluded/boxplot_app_before_outlier.png',
       width = 7, height = 4)


# After
ggplot(after_boxplot, aes(x = PP, y = bcRT)) +
  geom_boxplot(aes(color = Bet), width = 0.5, size = 0.4,
               position = position_dodge(0.8)) +
  labs(x = 'Group', y = 'Mean Box-Cox RT', title = 'After outlier') +
  scale_fill_manual(values = c("#FF3636", "#0057A0")) +
  scale_color_manual(values = c("#FF3636", "#0057A0")) +
  theme_classic()

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/with excluded/boxplot_app_after_outlier.png',
       width = 7, height = 4)




## RT difference in difference

RT_diff_diff = data.frame(logRT = 1, bcRT = 1, Bet = 1, PP = 1, Time = 1, 
                                  Trial = 1, Apprentice = 1, ID = 1)
j = 1
for(i in which(!is.na(comp$Answer.After))) {
  
    x = comp[j:i, ]
    
    x$Time = 'Before'
    if(any(abs(x$Shot) > 7)) {
      for(k in 1:nrow(x)) {
        if(abs(x$Shot[k]) > 7) {
          break
        }
      }
      
      if(k == 1) {
        x$Time[2:nrow(x)] = 'After'
      } else if(k != nrow(x)) {
        x$Time[(k + 1):nrow(x)] = 'After'
      }
      
    } 
    
    x$Bet[x$Bet == 1] = 'Yes'
    x$Bet[x$Bet == 0] = 'No'
    
    if(x$X7m_pp[1] == 1) {
      pp = 'Penny Picker'
    } else if(x$X7m_pp[1] == 0){
      pp = 'Other'
    }
    
    y = data.frame(logRT = x$logRT, bcRT = x$bcRT, Bet = x$Bet, 
                   PP = rep(pp, nrow(x)), Time = x$Time, Trial = 1:nrow(x),
                   Apprentice = x$Apprentice, ID = x$Sample.ID)
    
    RT_diff_diff = rbind(RT_diff_diff, y)
    
    j = i + 1
}

RT_diff_diff = RT_diff_diff[2:nrow(RT_diff_diff), ]

mean_master = aggregate(bcRT ~ Trial + ID, data = RT_diff_diff[RT_diff_diff$Apprentice == 0, ],
                        FUN = mean)

Diff_app = RT_diff_diff[RT_diff_diff$Apprentice == 1, ]

Diff_app$Corrected = NA
for(sub in unique(Diff_app$ID)) {
  for(trial in 1:20) {
    
    Diff_app$Corrected[Diff_app$ID == sub & 
                         Diff_app$Trial == trial] = mean_master$bcRT[mean_master$ID == sub &
                                                                       mean_master$Trial == trial]
    
  }
}

Diff_app = Diff_app[Diff_app$PP == 'Penny Picker', ]

Diff_app$Time = factor(Diff_app$Time, levels = c('Before', 'After'))

# Before
ggplot(Diff_app, aes(x = Time, y = bcRT)) +
  geom_boxplot(aes(color = Bet), width = 0.7, size = 0.5,
               position = position_dodge(0.8)) +
  labs(x = '', y = 'Mean Box-Cox RT', 
       title = 'Before/After Outlier for Penny Pickers') +
  scale_fill_manual(values = c("#FF3636", "#0057A0")) +
  scale_color_manual(values = c("#FF3636", "#0057A0")) +
  theme_classic()

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Genomics/Plots/boxplot_app_pp_BA_YN_notcorrected.png',
       width = 7, height = 4)



# Testing differences
lmer_diff_app_raw = lmer(bcRT ~ Bet * Time + (1 | ID), data = Diff_app)
lmer_diff_app_cor = lmer(Corrected ~ Bet * Time + (1 | ID), data = Diff_app)

summary(lmer_diff_app_raw)
summary(lmer_diff_app_cor)





