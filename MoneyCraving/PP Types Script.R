
# Checking the proportions of penny pickers in experiments
# And checking how often penny pickers picked pennies

data_list = list(comp_all_7, comp_all_7mm, 
                 comp_all_8, comp_all_8mm,
                 comp_all_9, comp_all_9mm)

out_list = list()
for(data in data_list) {
  temp = data.frame()
  data_use = data[data$PP == 1 &
                    data$Time == 'After' &
                    data$Apprentice == 1 &
                    #(!is.na(data$Trial_From_BS) & data$Trial_From_BS > 0) &
                    data$Experiment %in% c('Experiment1',
                                           'Experiment3',
                                           'Experiment5'), ]
  
  for(ID in unique(data_use$ID)) {
    t1 = 0
    t2 = 0
    t3 = 0
    other = 0
    data_ID = data_use[data_use$ID == ID, ]
    for(sesh in unique(data_ID$Session_Number)) {
      data_sesh = data_ID[data_ID$Session_Number == sesh, ]
      if(nrow(data_sesh) > 3) {
        first_2 = data_sesh[1:2, ]
        last = data_sesh[3:nrow(data_sesh), ]
        
        if(sum(first_2$Bet) > 0 & mean(last$Bet) < 0.5) {
          t1 = t1 + 1
        } else if(sum(first_2$Bet) == 0 & sum(last$Bet) > 0) {
          t2 = t2 + 1
        } else if(mean(data_sesh$Bet) > 0.8) {
          t3 = t3 + 1
        } else {
          other = other + 1
        }
      }
    }
    
    total = t1+t2+t3
    
    temp = rbind(temp, c(ID, data_ID$Experiment[1],
                         t1/total,
                         t2/total,
                         t3/total,
                         other/total))
    
  }
  
  colnames(temp) = c('ID', 'Experiment', 'Type1', 'Type2',
                     'Type3', 'Other')
  
  for(i in 3:6) {
    temp[,i] = as.numeric(temp[,i])
  }
  
  out_list[[length(out_list) + 1]] = na.omit(temp)
  
}

apply(out_list[[1]][, 3:6], 2, mean)




