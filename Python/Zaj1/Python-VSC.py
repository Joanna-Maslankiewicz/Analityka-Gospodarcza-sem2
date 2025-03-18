# w teorii powinniśmy robić w Jupyter Notebook webowym, ale robię w VSC
import pandas as pd
import numpy as np 
import os

sciezka = os.getcwd()
plik = os.path.join(sciezka, 'apf_data.xlsx')
print(sciezka)
print(plik)

ramka = pd.read_excel(plik, '1')
ramka.head()