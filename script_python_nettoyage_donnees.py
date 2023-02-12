# -*- coding: utf-8 -*-
"""
Created on Sat Feb 11 17:52:25 2023

@author: PC
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

dataset_gov = pd.read_excel("C:/Users/PC/Downloads/table_dataset_gouvernment.xlsx")


"renommer les en-têtes pour les années"
l = np.arange(1947,2023)
year_index = np.repeat(l, 4)


df_reciepe = pd.DataFrame(dataset_gov)
df_reciepe.head()

print(df_reciepe.columns)


print(l)
print(year_index)
type(year_index) ##ndarray
year_header = year_index.tolist() ##convertion en type list
type(year_header) ##list

len(year_header)

year_header.insert(0, "")
year_header.insert(1, "")

len(year_header)

df_reciepe.columns = year_header

print(df_reciepe.columns)
print(df_reciepe.info())
print(df_reciepe.head())






print(dataset_gov.iloc[:0])






#display(df_reciepe.iloc[1:2]) ##recette courantes du gouvernment

