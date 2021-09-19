import numpy as np
import pandas as pd
import os
import glob

# Insert project directory
data_path = 'C:/Users/samue/Google Drive/Elise Projects/Genomics'

# Load genomics data and get path for behavioral data
df = pd.read_excel(os.path.join(data_path + '/Data/', 'Bowman Genotyping Compilation_FIX.xlsx'))
data_path_2 = os.path.join(data_path + '/', 'penny_pickers_6_different_definitions')

# Every column is a defintion of penny picking (pp is binary, con means penny picking is continuous)
new_cols = ['7m_pp', '7m_con', '7m_m_pp', '7m_m_con', '8m_pp', '8m_con', '8m_m_pp', '8m_m_con',
           '9m_pp', '9m_con', '9m_m_pp', '9m_m_con']

# Make all NA
for col in new_cols:
    df[col] = np.nan

# Add column with excluded participants
df['excluded'] = 0
#df

# Iteratre through behavioral data directory
col = 0
for i, sheet in enumerate(os.listdir(data_path_2)):
    # Load behavioral data for pp criterion (e.g. 7m) and cut out unwanted parts of the FileName column
    df_2 = pd.read_excel(os.path.join(data_path_2 + '/', sheet))
    df_2['FileName'] = [i.split('.xls', 1)[0] for i in df_2['FileName']]
    
    # For the first excel sheet, save split ID numbers for participants and remove any extra text (marked by _)
    if i == 0:
        splits = [i.split('/', 1) for i in df_2['FileName']]
        df_2['FileName'] = [item[1].split('_', 1)[0] for item in splits]
        
        # If participant is noted as excluded, make sure to change 0 to 1 in main df (this is why first iteration is different)
        for j, jtem in enumerate(splits):
            if 'excluded' in jtem[0]:
                df.loc[df['Sample ID'] == jtem[1].split('_', 1)[0], 'excluded'] = 1
    else:
        df_2['FileName'] = [i.split('/', 1)[1].split('_', 1)[0] for i in df_2['FileName']]
    
    # Match data in behavioral data frame to IDs in main df
    # Add 2 to col variable as we have inseted 2 rows of data (one binary and one continuous)
    for k, participant in enumerate(df_2['FileName']):
        
        if df['Sample ID'].str.contains(participant).any():
            df.loc[df['Sample ID'] == participant, new_cols[col]] = df_2.iloc[k, 1]
            df.loc[df['Sample ID'] == participant, new_cols[col + 1]] = df_2.iloc[k, 2]
            
    col += 2

# Save to df
df.to_csv(data_path + '/Data/' + '/data_new_split200518.csv', index = False, na_rep='NA')

