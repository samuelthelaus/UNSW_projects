

df_list = list(comp_all_7, comp_all_7mm, 
               comp_all_8, comp_all_8mm,
               comp_all_9, comp_all_9mm)

index_df = data.frame(Participant_ID = 1, 
                      Experiment = 'Experiment1',
                      seven = 0, seven_memorable = 0,
                      eight = 0, eight_memorable = 0,
                      nine = 0, nine_memorable = 0)

for(exp in c('Experiment1', 'Experiment3', 'Experiment4', 'Experiment5')) {
  data = comp_all_7[comp_all_7$Experiment == exp, ]
  for(sub in unique(data$Sample.ID)) {
    my_vec = c(sub, exp)
    for(df in df_list) {
      
      my_vec = append(my_vec, df$PP[df$Sample.ID == sub &
                                      df$Experiment == exp][1])
    }
    index_df = rbind(index_df, my_vec)
  }
}

index_df = index_df[2:nrow(index_df), ]

cols_num <- c('Participant_ID', 'seven', 'seven_memorable',
              'eight', 'eight_memorable',
              'nine', 'nine_memorable')
index_df[cols_num] <- sapply(index_df[cols_num], as.numeric)

index_df = index_df[with(index_df, order(Experiment, Participant_ID)), ]

# Create folders and move data
dir.create('C:/Users/samuel/Desktop/Data for Elise')


for(exp in unique(index_df$Experiment)) {
  dir.create(paste('C:/Users/samuel/Desktop/Data for Elise', '/',
                   exp, sep = ''))
}

dir_list = list.dirs(path = 'C:/Users/samuel/Desktop/Data for Elise', 
                     full.names = TRUE, recursive = FALSE)

for(folder in dir_list) {
  for(col in colnames(index_df[,3:ncol(index_df)])) {
    dir.create(paste(folder, '/', col, sep = ''))
  }
}

for(folder in dir_list) {
  
  criteria_dirs = list.dirs(path = folder, 
                            full.names = TRUE, recursive = FALSE)
  
  for(sub_folder in criteria_dirs) {
    dir.create(paste(sub_folder, '/', 'Penny Pickers', sep = ''))
    dir.create(paste(sub_folder, '/', 'Others', sep = ''))
  }
  
}

# Move files over
for(row in 1:nrow(index_df)) {
  
  dir_list = list.dirs(path = 'C:/Users/samuel/Google Drive/Elise Projects/Picking pennies/Data/DATA_all_experiments', 
                       full.names = TRUE, recursive = FALSE)
  
  for(item in dir_list) {
    if(grepl(index_df$Experiment[row], item)) {
      exp_dir = item
      break
    }
  }
  
  data_list = list.files(exp_dir)
  
  for(item in data_list) {
    if(grepl(paste('^', index_df$Participant_ID[row], '.xls', '$', sep = ''), item)) {
      sub_dir = paste(exp_dir, '/', item, sep = '')
      break
    }
  }
  
  for(col in colnames(index_df[,3:ncol(index_df)])) {
    if(index_df[row, col] == 1) {
      file.copy(sub_dir, 
                paste('C:/Users/samuel/Desktop/Data for Elise', '/',
                      index_df$Experiment[row], '/',
                      col, '/',
                      'Penny Pickers',
                      sep = ''))
    } else if(index_df[row, col] == 0){
      file.copy(sub_dir, 
                paste('C:/Users/samuel/Desktop/Data for Elise', '/',
                      index_df$Experiment[row], '/',
                      col, '/',
                      'Others',
                      sep = ''))
    }
  }
}




