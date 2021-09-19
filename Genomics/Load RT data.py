import numpy as np
import pandas as pd
import os
import glob

# Adjust these for whatever you call your files (df is the output of Data Load)
data_path = 'C:/Users/samuel/Google Drive/Elise Projects/Genomics'
df = pd.read_csv(os.path.join(data_path + '/Data/' + 'gen_data_new_split.csv'))
data_path_2 = os.path.join(data_path + '/' + 'data_brown_cleaned')

# Old and new columns
column_names = ['Trial Number', 'Shot', 'SD', 'Apprentice', 'Missed', 'Bet', 'Outcome', 'Wealth', 'RT', 'Answer After',
               'Confidence', 'Outcome From Answer', 'Unimportant', 'Sample ID']

column_add = ['X7m_pp', 'X7m_con', 'X7m_m_pp', 'X7m_m_con', 'X8m_pp', 'X8m_con', 'X8m_m_pp', 'X8m_m_con', 'X9m_pp', 
              'X9m_con', 'X9m_m_pp', 'X9m_m_con']

empty = pd.DataFrame(columns = column_names + column_add)

# Iterate through group names (betting style)
for group in [f.name for f in os.scandir(data_path_2) if f.is_dir()]:
    # Go through every participant's data and elongate data from df based on length of data for each participant. Then save to 'empty'
    for sub in os.listdir(os.path.join(data_path_2 + '/' + group)):
        x = os.path.join(data_path_2 + '/' + group + '/' + sub)
        temp = pd.read_excel(x, names = column_names, sheet_name = 'results', header = None)
        temp['Sample ID'] = sub.split('.xls', 1)[0]
        temp = temp.reindex(columns = temp.columns.tolist() + column_add)
        
        if df['Sample.ID'].str.contains(sub.split('.xls', 1)[0]).any():
            for item in column_add:
                temp[item] = df.loc[df['Sample.ID'] == sub.split('.xls', 1)[0], item].tolist()[0]
            
        empty = pd.concat([empty, temp])


empty.to_csv(data_path + '/Data/' + '/RT_Data.csv', index = False, na_rep='NA')
