# -*- coding: utf-8 -*-
"""
Created on Sun Feb 12 00:09:55 2023

@author: PC
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

dataset_gov = pd.read_excel("C:/Users/PC/Downloads/table_dataset_gouvernment.xlsx")

print(dataset_gov)

"Mise en place du dataframe"

df_reciepe = pd.DataFrame(dataset_gov)
df_reciepe.head()

"renommer les en-têtes pour les années"

l = np.arange(1947,2023)
year_index = np.repeat(l, 4)
print(year_index)
type(year_index) ##ndarray
year_header = year_index.tolist()
year_header = year_index.tolist() ##convertion en type list
year_header.insert(0, "")
year_header.insert(1, "")
type(year_header) ##list
len(year_header)
    
df_reciepe.columns = year_header
    
colonne_nom = ["Année","trimestre","Current receipts","Current tax receipts", "Personal current taxes","Taxes on production and imports","Taxes on corporate income","Taxes from the rest of the world","Contributions for government social insurance","From persons","From the rest of the world1","Income receipts on assets","Interest and miscellaneous receipts","Interest receipts2","Rents and royalties","Dividends","Current transfer receipts","From business (net)","From persons","From the rest of the world3","Current surplus of government entreprises4","Current expenditures","Consumption expenditures","Current transfer payments","Government social benefits","To persons","To the rest of the world5","Other current transfer payments to the rest of the world 3.5","Interest payments2","To person and business2","To the rest of the world","Subsidies4","Net government saving","Social insurance funds","Other","Addenda","Total receipts","Current receipts","Capital transfer receipts","Total expenditures","Current expenditures","Gross government investment","Capital transfer payments", "Net  purchases of nonproducted assets","Less: Consumption of fixed capital","Net lending or net borrowing"]   
#essayons de transposer le dataframe

df_reciepe_transposed = df_reciepe.T
df_reciepe_transposed 

print(df_reciepe_transposed.info())

"quelques vérifications"

df_reciepe_transposed  = df_reciepe_transposed.iloc[2:, :] #enlever 2 premières lignes
df_reciepe_transposed.reset_index(inplace=True) ##ajouter les années comme une colonne

"ajout des en-têtes du dataframe"
df_reciepe_transposed.columns = colonne_nom 

"remplacer les tirets par des valeurs manquantes"
df_reciepe_transposed = df_reciepe_transposed.replace("---", np.nan, regex=True) 

"observation de la structure du dataframe"
df_reciepe_transposed.info()
print(df_reciepe.columns)
df_reciepe_transposed.head()

"convertir le dataframe en fichier excel pour une analyse économétrique sous R"

df_reciepe_transposed.to_excel("C:/Users/PC/Documents/econometrics/reciept.xlsx",sheet_name='reciepgov') 