library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)

# Some plots for the after-effect

ggplot(ae_plot_data, aes(x = no_sound, y = sound)) +
  geom_hline(yintercept = 0, size = 1, linetype = 'dashed', alpha = 0.9) +
  geom_vline(xintercept = 0, size = 1, linetype = 'dashed', alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, col = '#E21A1A', size = 1, alpha = 0.9) +
  geom_errorbar(aes(ymin = sound - SE, ymax = sound + SE),
                col = '#0A61C9') +
  geom_errorbarh(aes(xmin = no_sound - SE, xmax = no_sound + SE),
                col = '#0A61C9') +
  geom_point(size = 3.5, col = '#0A61C9') +
  theme_minimal() +
  scale_x_continuous(breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5), 
                     expand = c(0.05, 0.05), limits = c(-0.35, 1.5)) +
  scale_y_continuous(breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5), 
                     expand = c(0.05, 0.05), limits = c(-0.35, 1.5)) +
  labs(x = 'After effect (no sound)', y = 'After-effect (sound)')

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/AE_effect_botherrors.png', 
       width = 7, height = 5)


write.csv(ae_plot_data, 
          'C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_data/ae_plot_data.csv',
          row.names = FALSE)


## Other version
ae_plot_data$diff = ae_plot_data$sound - ae_plot_data$no_sound

ggplot(ae_plot_data, aes(x = 1:47, y = diff)) +
  geom_hline(yintercept = 0, size = 1, alpha = 0.9, col = '#E21A1A') +
  geom_errorbar(aes(ymin = diff - SE, ymax = diff + SE),
                col = 'black', alpha = 0.9) +
  geom_point(size = 3.5, col = '#0A61C9') +
  theme_minimal() +
  labs(x = 'Participant #', y = 'After-effect difference \n(sound - no sound)')


ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/AE_effect_second_option.png', 
       width = 7, height = 5)



## Exp 2 and 3
ae_plot_exp2 = data.frame(diff = 1, SE = 2)
ae_plot_exp3 = data.frame(diff = 1, SE = 2)

for(sub in unique(data_exp$participant[data_exp$experiment %in%
                                       c('white_noise', 'crossover_test')])) {
    
  if(data_exp$experiment[data_exp$participant == sub][1] == 'white_noise') {
    diff = (mean(data_exp$Results[data_exp$participant == sub & 
                                    data_exp$experiment == 'white_noise' &
                                    data_exp$volatility == 'Post-Low' &
                                    data_exp$condition == 'sound']) -
              mean(data_exp$Results[data_exp$participant == sub & 
                                      data_exp$experiment == 'white_noise' &
                                      data_exp$volatility == 'Post-High' &
                                      data_exp$condition == 'sound'])) -
      (mean(data_exp$Results[data_exp$participant == sub & 
                               data_exp$experiment == 'white_noise' &
                               data_exp$volatility == 'Post-Low' &
                               data_exp$condition == 'no_sound']) -
         mean(data_exp$Results[data_exp$participant == sub & 
                                 data_exp$experiment == 'white_noise' &
                                 data_exp$volatility == 'Post-High' &
                                 data_exp$condition == 'no_sound']))
    
    SE = sd(data_exp$Results[data_exp$participant == sub])/
      sqrt(length(data_exp$Results[data_exp$participant == sub]))
    
    ae_plot_exp2 = rbind(ae_plot_exp2, c(diff, SE))
  } else {
    diff = (mean(data_exp$Results[data_exp$participant == sub & 
                                    data_exp$experiment == 'crossover_test' &
                                    data_exp$volatility == 'Post-Low' &
                                    data_exp$condition == 'sound']) -
              mean(data_exp$Results[data_exp$participant == sub & 
                                      data_exp$experiment == 'crossover_test' &
                                      data_exp$volatility == 'Post-High' &
                                      data_exp$condition == 'sound'])) -
      (mean(data_exp$Results[data_exp$participant == sub & 
                               data_exp$experiment == 'crossover_test' &
                               data_exp$volatility == 'Post-Low' &
                               data_exp$condition == 'no_sound']) -
         mean(data_exp$Results[data_exp$participant == sub & 
                                 data_exp$experiment == 'crossover_test' &
                                 data_exp$volatility == 'Post-High' &
                                 data_exp$condition == 'no_sound']))
    
    SE = sd(data_exp$Results[data_exp$participant == sub])/
      sqrt(length(data_exp$Results[data_exp$participant == sub]))
    
    ae_plot_exp3 = rbind(ae_plot_exp3, c(diff, SE))
  }
}

ae_plot_exp2 = ae_plot_exp2[2:nrow(ae_plot_exp2), ]
ae_plot_exp3 = ae_plot_exp3[2:nrow(ae_plot_exp3), ]

ae_plot_exp2$ID = 1:nrow(ae_plot_exp2)
ae_plot_exp3$ID = 1:nrow(ae_plot_exp3)

# Exp 2
  
ggplot(ae_plot_exp2, aes(x = 1:54, y = diff)) +
  geom_hline(yintercept = 0, size = 1, alpha = 0.9, col = '#E21A1A') +
  geom_errorbar(aes(ymin = diff - SE, ymax = diff + SE),
                col = 'black', alpha = 0.9) +
  geom_point(size = 3.5, col = '#0A61C9') +
  theme_minimal() +
  labs(x = 'Participant #', y = 'After-effect difference \n(sound - no sound)')


ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/AE_effect_second_option_exp2.png', 
       width = 7, height = 5)



# Exp 2

ggplot(ae_plot_exp3, aes(x = 1:53, y = diff)) +
  geom_hline(yintercept = 0, size = 1, alpha = 0.9, col = '#E21A1A') +
  geom_errorbar(aes(ymin = diff - SE, ymax = diff + SE),
                col = 'black', alpha = 0.9) +
  geom_point(size = 3.5, col = '#0A61C9') +
  theme_minimal() +
  labs(x = 'Participant #', y = 'After-effect difference \n(sound - no sound)')


ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/AE_effect_second_option_exp3.png', 
       width = 7, height = 5)


# As one plot
plot_list = list()
frame_list = list(ae_plot_data, ae_plot_exp2, ae_plot_exp3)
title_list = c('A', 'B', 'C')
for(i in 1:3) {
  
  plot_list[[i]] = ggplot(frame_list[[i]], 
                          aes(x = ID, y = diff)) +
    geom_hline(yintercept = 0, size = 1, alpha = 0.9, col = '#E21A1A') +
    geom_errorbar(aes(ymin = diff - SE, ymax = diff + SE),
                  col = 'black', alpha = 0.9) +
    geom_point(size = 3.5, col = '#0A61C9') +
    theme_minimal() +
    labs(x = 'Participant #', 
         y = 'After-effect difference \n(sound - no sound)',
         title = title_list[i])
  
}


plot_grid = grid.arrange(plot_list[[1]], plot_list[[2]],
                      plot_list[[3]], 
                      ncol=1, nrow = 3)

ggsave('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_analysis/Plots/AE_effect_second_option_all.png',
       plot = plot_grid,
       height = 8, width = 5.6, dpi=1200)


