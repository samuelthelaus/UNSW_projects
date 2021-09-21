
library(EnvStats)
library(comprehenr)

# Read in data
data = read.csv('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_data/data.csv')
after_effect = read.csv('C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_data/AF raw data.csv')

# Make background all lower case
data$Background = as.character(data$Background)
for(i in 1:nrow(data)) {
  data$Background[i] = tolower(data$Background[i])
}

data$Background = as.factor(data$Background)

data = data[data$time != 0, ]

boxcox(data$time, optimize = TRUE, lambda = c(-10, 10))

data$bcRT = boxcoxTransform(data$time, lambda = 0.1811776)

# Separate data by experiment and include only experimental trials
first_exp = data[data$experiment == 'first_experiment' &
                   data$Type == 'Experiment' &
                   data$time != 0, ]
white_noise = data[data$experiment == 'white_noise' &
                     data$Type == 'Experiment' &
                     data$time != 0, ]
crossover = data[data$experiment == 'crossover_test' &
                   data$Type == 'Experiment' &
                   data$time != 0, ]
first_white = data[(data$experiment == 'first_experiment' | 
                      data$experiment == 'white_noise') &
                     data$Type == 'Experiment' &
                     data$time != 0, ]


# Aggregate by participant ID and condition
first_agg = aggregate(bcRT ~ participant + condition + Age + Gender + Smoker + Background,
                   data = first_exp, FUN = mean)
wn_agg = aggregate(bcRT ~ participant + condition + Age + Gender + Smoker + Background,
                   data = white_noise, FUN = mean)
cross_agg = aggregate(bcRT ~ participant + condition + Age + Gender + Smoker + Background,
                   data = crossover, FUN = mean)
fw_agg = aggregate(bcRT ~ participant + condition + Age + Gender + Smoker + Background,
                   data = first_white, FUN = mean)


# Split into sound and no sound
first_agg_sound = first_agg[first_agg$condition == 'sound', ]
first_agg_sound = first_agg_sound[order(first_agg_sound$participant), ]
first_agg_nosound = first_agg[first_agg$condition == 'no_sound', ]
first_agg_nosound = first_agg_nosound[order(first_agg_nosound$participant), ]

wn_agg_sound = wn_agg[wn_agg$condition == 'sound', ]
wn_agg_sound = wn_agg_sound[order(wn_agg_sound$participant), ]
wn_agg_nosound = wn_agg[wn_agg$condition == 'no_sound', ]
wn_agg_nosound = wn_agg_nosound[order(wn_agg_nosound$participant), ]

cross_agg_sound = cross_agg[cross_agg$condition == 'sound', ]
cross_agg_sound = cross_agg_sound[order(cross_agg_sound$participant), ]
cross_agg_nosound = cross_agg[cross_agg$condition == 'no_sound', ]
cross_agg_nosound = cross_agg_nosound[order(cross_agg_nosound$participant), ]

fw_agg_sound = fw_agg[fw_agg$condition == 'sound', ]
fw_agg_sound = fw_agg_sound[order(fw_agg_sound$participant), ]
fw_agg_nosound = fw_agg[fw_agg$condition == 'no_sound', ]
fw_agg_nosound = fw_agg_nosound[order(fw_agg_nosound$participant), ]


#### After effect organize ####

# Create difference term
after_effect$diff = after_effect$sound - after_effect$no_sound


## Make new data for AE mixed-effects

# Control
for(sub in unique(data$participant)) {
  data$adjusted[data$participant == sub] = data$Results[data$participant == sub] - 
    mean(data$Results[data$participant == sub & data$Type == 'Control'])
  data$adjusted[data$participant == sub] = scale(data$adjusted[data$participant == sub])
}

# Low
for(sub in unique(data$participant)) {
  for(con in c('sound', 'no_sound')) {
    data$adj_low[data$participant == sub &
                   data$condition == con] = 
      data$Results[data$participant == sub &
                     data$condition == con] - 
      mean(data$Results[data$participant == sub &
                          data$StdDev_1 %in% c(0.25, 0.3) &
                          data$condition == con])
    
    data$adj_low[data$participant == sub &
                   data$condition == con] = 
      scale(data$adj_low[data$participant == sub &
                           data$condition == con])
  }
}

# High
for(sub in unique(data$participant)) {
  for(con in c('sound', 'no_sound')) {
    data$adj_high[data$participant == sub &
                   data$condition == con] =
      mean(data$Results[data$participant == sub &
                          data$StdDev_1 == 0.04 &
                          data$condition == con]) -
      data$Results[data$participant == sub &
                     data$condition == con]
      
    
    data$adj_high[data$participant == sub &
                   data$condition == con] = 
      scale(data$adj_high[data$participant == sub &
                           data$condition == con])
  }
}

# Baseline divide
for(sub in unique(data$participant)) {
  for(con in c('sound', 'no_sound')) {
    data$adj_div[data$participant == sub &
                    data$condition == con] =
      (data$adj_high[data$participant == sub &
                       data$condition == con] =
         mean(data$Results[data$participant == sub &
                             data$StdDev_1 == 0.04 &
                             data$condition == con]) -
         data$Results[data$participant == sub &
                        data$condition == con])/
      mean(data$Results[data$participant == sub & data$Type == 'Control'])
    
    
    data$adj_div[data$participant == sub &
                    data$condition == con] = 
      scale(data$adj_high[data$participant == sub &
                            data$condition == con])
  }
}

data_exp = data[data$Type == 'Experiment', ]

data_exp$volatility[data_exp$StdDev_1 %in% c(0.25, 0.3)] = 'Post-High'
data_exp$volatility[data_exp$StdDev_1 == 0.04] = 'Post-Low'

data_exp$ExpGroup = as.factor(to_vec(for(item in data_exp$experiment)
  if(item %in% c('first_experiment', 'white_noise')) 'Exp 1 or 2'
  else if(item == 'crossover_test') 'Exp 3'))

# For ae plot
ae_plot_data = data.frame(ID = 1, sound = 2, no_sound = 3, SE = 3)
for(sub in unique(data_exp$participant[data_exp$experiment == 'first_experiment'])) {
  
  mean_sound = mean(data_exp$Results[data_exp$participant == sub &
                                       data_exp$condition == 'sound' &
                                       data_exp$volatility == 'Post-Low']) -
    mean(data_exp$Results[data_exp$participant == sub &
                            data_exp$condition == 'sound' &
                            data_exp$volatility == 'Post-High'])
  
  mean_no_sound = mean(data_exp$Results[data_exp$participant == sub &
                                          data_exp$condition == 'no_sound' &
                                          data_exp$volatility == 'Post-Low']) -
    mean(data_exp$Results[data_exp$participant == sub &
                            data_exp$condition == 'no_sound' &
                            data_exp$volatility == 'Post-High'])
  
  se_both = sd(data_exp$Results[data_exp$participant == sub])/
    sqrt(length(data_exp$Results[data_exp$participant == sub]))
  
  ae_plot_data = rbind(ae_plot_data, 
                       c(data_exp$participant[data_exp$participant == sub][1],
                         mean_sound, mean_no_sound, se_both))
}

ae_plot_data = ae_plot_data[2:nrow(ae_plot_data), ]

ae_plot_data$ID_long = ae_plot_data$ID

ae_plot_data$ID = as.character(ae_plot_data$ID)

ae_plot_data$ID = as.numeric(to_vec(for(item in ae_plot_data$ID) 
  if(substr(item, start=2, stop=2) == '2') 
    as.numeric(substring(item, first=3)) + 24 else
      substring(item, first=3)))

ae_plot_data = ae_plot_data[order(ae_plot_data$ID), ]

