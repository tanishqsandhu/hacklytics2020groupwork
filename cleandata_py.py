# -*- coding: utf-8 -*-
"""CleanData.py

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1rCN2oH4lJXqv9wL6UmymjExU2ddaMe7u
"""

#author: Hongrui Lyu
import numpy as np
import pandas as pd

"""# **Import the Original Data ebola1.csv, trying to see what is in there**"""

data1 = pd.read_csv("ebola1.csv")
data1.head()

"""# **Clean the data: we do not need to know the source and link to that source. keeping all the other columns.**
# **We only interested on the data of cases, not suspected, death, and etc. So we just keep the rows with Cases in Category.**
"""

data1 = data1.drop(['Link', 'Sources'], axis = 1)

data1 = data1.loc[data1['Category'] == 'Cases']

"""# **Check to see whether we have get all the things cleaned**"""

data1.groupby(by = ['Category']).count()

data1.head()

data1

"""# **We only want to focus on the country and Dates. Eliminate all the province in Localite, keeping Nagional!**"""

data1 = data1.loc[data1['Localite'] == 'National']

data1

"""# **Save to a file for other group members to work on that**"""

data1.to_csv("ebola1-cleaned-by-country.csv")

"""# **Group the data by dates and country, and then save it to a file and send to other group members to work on that.**"""

data2 = data1.groupby(['Date', 'Country'])['Value'].sum()

data2

data2.to_csv('ebola1-date-country.csv')

"""# **Calculate the average increases cases numbers for each country**"""

country_list = []
rate_list = []
for item in list(data1.Country.unique()):
  country_list.append(item)
  temp = data1.loc[data1.Country == item]
  values =  temp.loc[:,'Value'].str.replace(",", "")
  temp['diff'] = values.astype('float').diff()
  rate_list.append(temp.loc[:,'diff'].mean())
data3 = pd.Series(rate_list, index = country_list)
data3

data3.to_csv("case_growing_rate.csv")