
# This script conatins simple analyses using t-tests to get an overview of the data 

# N=no. participants

#### T-test sound/no sound ####

# Exp 1 (SIGNIFICANT)

# bcRT is the box-cox corrected reaction time
t.test(first_agg_sound$bcRT, first_agg_nosound$bcRT, 
       paired = TRUE, alternative = 'two.sided')

boxplot(bcRT ~ condition, data = first_agg)

# effect size
d = (mean(first_agg_sound$bcRT) - mean(first_agg_nosound$bcRT))/
  sqrt((sd(first_agg_sound$bcRT)^2 + sd(first_agg_nosound$bcRT)^2)/2)

d

# power for this test (change d to assume different underlying effect sizes)
pwr.t.test(n=nrow(first_agg_sound),
           d=.2, sig.level = .05, 
           type = 'paired', alternative = 'two.sided')

hist(first_agg_sound$bcRT, main = 'Exp 1 - Sound', xlab = 'bcRT')
hist(first_agg_nosound$bcRT, main = 'Exp 1 - No sound', xlab = 'bcRT')

plot_exp1_agg = data.frame(Condition = c('Sound', 'No sound'),
                           mean = c(mean(first_agg_sound$bcRT), mean(first_agg_nosound$bcRT)),
                           sd = c(sd(first_agg_sound$bcRT), sd(first_agg_nosound$bcRT)))

ggplot(plot_exp1_agg, aes(x=Condition, y=mean)) +
  geom_bar(stat = 'identity', col='black', fill='brown1', width=.6) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Exp 1 (N=subjects)') +
  theme_classic()

ggsave('Exp1 nIsSubjects.png', width = 5, height = 4)

# Exp 2 (NOT SIGNIFICANT)
t.test(wn_agg_sound$bcRT, wn_agg_nosound$bcRT, 
       paired = TRUE, alternative = 'two.sided')

boxplot(bcRT ~ condition, data = wn_agg)

# effect size
d = (mean(wn_agg_sound$bcRT) - mean(wn_agg_nosound$bcRT))/
  sqrt((sd(wn_agg_sound$bcRT)^2 + sd(wn_agg_nosound$bcRT)^2)/2)

d

# power
pwr.t.test(n=nrow(wn_agg_sound),
           d=.5, sig.level = .05, 
           type = 'paired', alternative = 'two.sided')

hist(wn_agg_sound$bcRT, main = 'Exp 2 - Sound', xlab = 'bcRT')
hist(wn_agg_nosound$bcRT, main = 'Exp 2 - No sound', xlab = 'bcRT')

wilcox.test(wn_agg_sound$bcRT, wn_agg_nosound$bcRT, 
            paired = TRUE, alternative = 'two.sided')

plot_exp2_agg = data.frame(Condition = c('Sound', 'No sound'),
                           mean = c(mean(wn_agg_sound$bcRT), mean(wn_agg_nosound$bcRT)),
                           sd = c(sd(wn_agg_sound$bcRT), sd(wn_agg_nosound$bcRT)))

ggplot(plot_exp2_agg, aes(x=Condition, y=mean)) +
  geom_bar(stat = 'identity', col='black', fill='brown1', width=.6) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Exp 2 (N=subjects)') +
  theme_classic()

ggsave('Exp2 nIsSubjects.png', width = 5, height = 4)


# Exp 1 & 2 pooled (SIGNIFICANT)
t.test(fw_agg_sound$bcRT, fw_agg_nosound$bcRT, 
       paired = TRUE, alternative = 'two.sided')

boxplot(bcRT ~ condition, data = fw_agg)

# effect size
d = (mean(fw_agg_sound$bcRT) - mean(fw_agg_nosound$bcRT))/
  sqrt((sd(fw_agg_sound$bcRT)^2 + sd(fw_agg_nosound$bcRT)^2)/2)

d

# power
pwr.t.test(n=nrow(fw_agg_sound),
           d=.2, sig.level = .05, 
           type = 'paired', alternative = 'two.sided')

hist(fw_agg_sound$bcRT, main = 'Exp 1&2 - Sound', xlab = 'bcRT')
hist(fw_agg_nosound$bcRT, main = 'Exp 1&2 - No sound', xlab = 'bcRT')

plot_exp12_agg = data.frame(Condition = c('Sound', 'No sound'),
                            mean = c(mean(fw_agg_sound$bcRT), mean(fw_agg_nosound$bcRT)),
                            sd = c(sd(fw_agg_sound$bcRT), sd(fw_agg_nosound$bcRT)))

ggplot(plot_exp12_agg, aes(x=Condition, y=mean)) +
  geom_bar(stat = 'identity', col='black', fill='brown1', width=.6) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Exp 1 & 2 (N=subjects)') +
  theme_classic()

ggsave('Exp1+2 nIsSubjects.png', width = 5, height = 4)

# Exp 3 (NOT SIGNIFICANT)
t.test(cross_agg_sound$bcRT, cross_agg_nosound$bcRT, 
       paired = TRUE, alternative = 'two.sided')

boxplot(bcRT ~ condition, data = cross_agg)

# effect size
d = (mean(cross_agg_sound$bcRT) - mean(cross_agg_nosound$bcRT))/
  sqrt((sd(cross_agg_sound$bcRT)^2 + sd(cross_agg_nosound$bcRT)^2)/2)

d

# power
pwr.t.test(n=nrow(cross_agg_sound),
           d=.2, sig.level = .05, 
           type = 'paired', alternative = 'two.sided')

hist(cross_agg_sound$bcRT, main = 'Exp 3 - Sound', xlab = 'bcRT')
hist(cross_agg_nosound$bcRT, main = 'Exp 3 - No sound', xlab = 'bcRT')

plot_exp3_agg = data.frame(Condition = c('Sound', 'No sound'),
                           mean = c(mean(cross_agg_sound$bcRT), mean(cross_agg_nosound$bcRT)),
                           sd = c(sd(cross_agg_sound$bcRT), sd(cross_agg_nosound$bcRT)))

ggplot(plot_exp3_agg, aes(x=Condition, y=mean)) +
  geom_bar(stat = 'identity', col='black', fill='brown1', width=.6) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Exp 3 (N=subjects)') +
  theme_classic()

ggsave('Exp3 nIsSubjects.png', width = 5, height = 4)
