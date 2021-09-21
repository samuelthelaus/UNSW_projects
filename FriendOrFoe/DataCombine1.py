import pandas as pd
import os
import glob

# Create emtpy data frame
column_names = ['Type', 'Mean_1', 'StdDev_1', 'Mean_2', 'StdDev_2', 'Results', 'time', 'Age', 'Gender', 'Smoker',
               'Background', 'date', 'experiment', 'condition', 'participant']
df = pd.DataFrame(columns = column_names)

data_path = 'C:/Users/samuel/Google Drive/Elise Projects/Friend or Foe/FoF_data'
experiments = [f.name for f in os.scandir(data_path) if f.is_dir()]

# Go through all experiments from the data directory
# Then iterate through conditions (sound/no sound) and sessions

for exp in experiments:
    if exp == 'first_experiment':
        exp_num = '1'
    elif exp == 'white_noise':
        exp_num = '2'
    elif exp == 'crossover_test':
        exp_num = '3'
    
    con_path = os.path.join(data_path + '/', exp)
    conditions = [p.name for p in os.scandir(con_path) if p.is_dir()]
    for con in conditions:
        path = os.path.join(con_path + '/', con)
        for filename in glob.glob(os.path.join(path, '*.txt')):
            
            data = pd.read_csv(filename, sep='	')
            
            # Add experiment and condition to df
            data['experiment'] = exp
            if 'no_sound' in con:
                data['condition'] = 'no_sound'
            else:
                data['condition'] = 'sound'
            
            # Remove (s) from RT column 
            split_data = data['time'].str.split('(')
            data['time'] = split_data.apply(lambda x: x[0])
            
            # Add participant ID (create unique ID for each participant)
            p_num = exp_num + con.split('d_s')[1] + filename.split('guest', 1)[1].split('.txt', 1)[0]
            data['participant'] = p_num
            
            # Join data
            df = pd.concat([df, data])

           
df.to_csv(data_path + '/data.csv', index = False)
