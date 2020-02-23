{"nbformat":4,"nbformat_minor":0,"metadata":{"colab":{"name":"training.py","provenance":[],"authorship_tag":"ABX9TyOCN+znwWWX5tbJILzUWmeg"},"kernelspec":{"name":"python3","display_name":"Python 3"}},"cells":[{"cell_type":"code","metadata":{"id":"D8YGNoaOMmFU","colab_type":"code","colab":{}},"source":["%matplotlib inline\n","import numpy as np\n","import pandas as pd\n","import sklearn.linear_model as skl\n","import matplotlib.pyplot as plt"],"execution_count":0,"outputs":[]},{"cell_type":"markdown","metadata":{"id":"9lZF7d3N7Qx3","colab_type":"text"},"source":["# **Read the data from files**"]},{"cell_type":"code","metadata":{"id":"zSPNRnwBv85B","colab_type":"code","outputId":"36a42c50-fb67-4276-d3b8-f7d53be1774d","executionInfo":{"status":"ok","timestamp":1582463955709,"user_tz":300,"elapsed":468,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}},"colab":{"base_uri":"https://localhost:8080/","height":195}},"source":["data = pd.read_csv('ebola1-cleaned-by-country2.csv', header = 0)\n","data.head()"],"execution_count":5,"outputs":[{"output_type":"execute_result","data":{"text/html":["<div>\n","<style scoped>\n","    .dataframe tbody tr th:only-of-type {\n","        vertical-align: middle;\n","    }\n","\n","    .dataframe tbody tr th {\n","        vertical-align: top;\n","    }\n","\n","    .dataframe thead th {\n","        text-align: right;\n","    }\n","</style>\n","<table border=\"1\" class=\"dataframe\">\n","  <thead>\n","    <tr style=\"text-align: right;\">\n","      <th></th>\n","      <th>Unnamed: 0</th>\n","      <th>Country</th>\n","      <th>Localite</th>\n","      <th>Category</th>\n","      <th>Value</th>\n","      <th>Date</th>\n","      <th>Healthcare Quality</th>\n","    </tr>\n","  </thead>\n","  <tbody>\n","    <tr>\n","      <th>0</th>\n","      <td>15</td>\n","      <td>Guinea</td>\n","      <td>National</td>\n","      <td>Cases</td>\n","      <td>122</td>\n","      <td>3/31/2014</td>\n","      <td>2</td>\n","    </tr>\n","    <tr>\n","      <th>1</th>\n","      <td>20</td>\n","      <td>Guinea</td>\n","      <td>National</td>\n","      <td>Cases</td>\n","      <td>127</td>\n","      <td>4/1/2014</td>\n","      <td>2</td>\n","    </tr>\n","    <tr>\n","      <th>2</th>\n","      <td>33</td>\n","      <td>Guinea</td>\n","      <td>National</td>\n","      <td>Cases</td>\n","      <td>143</td>\n","      <td>4/4/2014</td>\n","      <td>2</td>\n","    </tr>\n","    <tr>\n","      <th>3</th>\n","      <td>48</td>\n","      <td>Guinea</td>\n","      <td>National</td>\n","      <td>Cases</td>\n","      <td>151</td>\n","      <td>4/7/2014</td>\n","      <td>2</td>\n","    </tr>\n","    <tr>\n","      <th>4</th>\n","      <td>50</td>\n","      <td>Liberia</td>\n","      <td>National</td>\n","      <td>Cases</td>\n","      <td>21</td>\n","      <td>4/8/2014</td>\n","      <td>0</td>\n","    </tr>\n","  </tbody>\n","</table>\n","</div>"],"text/plain":["   Unnamed: 0  Country  Localite Category Value       Date  Healthcare Quality\n","0          15   Guinea  National    Cases   122  3/31/2014                   2\n","1          20   Guinea  National    Cases   127   4/1/2014                   2\n","2          33   Guinea  National    Cases   143   4/4/2014                   2\n","3          48   Guinea  National    Cases   151   4/7/2014                   2\n","4          50  Liberia  National    Cases    21   4/8/2014                   0"]},"metadata":{"tags":[]},"execution_count":5}]},{"cell_type":"markdown","metadata":{"id":"r5NYiCTj7aUg","colab_type":"text"},"source":["# **Group the data by countries, and calculating the average increasing rate. \n","# Plotting the graph**"]},{"cell_type":"code","metadata":{"id":"Lw-EDwywwPVV","colab_type":"code","outputId":"bcdc4911-b2b3-4db8-da1f-e101f2526cfb","executionInfo":{"status":"ok","timestamp":1582463959201,"user_tz":300,"elapsed":570,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}},"colab":{"base_uri":"https://localhost:8080/","height":326}},"source":["country_list = []\n","rate_list = []\n","health_care_level = []\n","for item in list(data.Country.unique()):\n","  country_list.append(item)\n","  temp = data.loc[data.Country == item]\n","  values =  temp.loc[:,'Value'].str.replace(\",\", \"\")\n","  temp['diff'] = values.astype('float').diff()\n","  rate_list.append(temp.loc[:,'diff'].mean())\n","  health_care_level.append(temp.iloc[0,6])\n","data2 = pd.DataFrame({\"Health_care_level\":health_care_level,'Avg increasing rate':rate_list}, index = country_list)\n","data2"],"execution_count":6,"outputs":[{"output_type":"stream","text":["/usr/local/lib/python3.6/dist-packages/ipykernel_launcher.py:8: SettingWithCopyWarning: \n","A value is trying to be set on a copy of a slice from a DataFrame.\n","Try using .loc[row_indexer,col_indexer] = value instead\n","\n","See the caveats in the documentation: http://pandas.pydata.org/pandas-docs/stable/user_guide/indexing.html#returning-a-view-versus-a-copy\n","  \n"],"name":"stderr"},{"output_type":"execute_result","data":{"text/html":["<div>\n","<style scoped>\n","    .dataframe tbody tr th:only-of-type {\n","        vertical-align: middle;\n","    }\n","\n","    .dataframe tbody tr th {\n","        vertical-align: top;\n","    }\n","\n","    .dataframe thead th {\n","        text-align: right;\n","    }\n","</style>\n","<table border=\"1\" class=\"dataframe\">\n","  <thead>\n","    <tr style=\"text-align: right;\">\n","      <th></th>\n","      <th>Health_care_level</th>\n","      <th>Avg increasing rate</th>\n","    </tr>\n","  </thead>\n","  <tbody>\n","    <tr>\n","      <th>Guinea</th>\n","      <td>2</td>\n","      <td>14.541667</td>\n","    </tr>\n","    <tr>\n","      <th>Liberia</th>\n","      <td>0</td>\n","      <td>51.057471</td>\n","    </tr>\n","    <tr>\n","      <th>Sierra Leone</th>\n","      <td>0</td>\n","      <td>55.327103</td>\n","    </tr>\n","    <tr>\n","      <th>Nigeria</th>\n","      <td>2</td>\n","      <td>0.592593</td>\n","    </tr>\n","    <tr>\n","      <th>Senegal</th>\n","      <td>1</td>\n","      <td>0.000000</td>\n","    </tr>\n","    <tr>\n","      <th>Mali</th>\n","      <td>1</td>\n","      <td>0.538462</td>\n","    </tr>\n","  </tbody>\n","</table>\n","</div>"],"text/plain":["              Health_care_level  Avg increasing rate\n","Guinea                        2            14.541667\n","Liberia                       0            51.057471\n","Sierra Leone                  0            55.327103\n","Nigeria                       2             0.592593\n","Senegal                       1             0.000000\n","Mali                          1             0.538462"]},"metadata":{"tags":[]},"execution_count":6}]},{"cell_type":"code","metadata":{"id":"UfmmRfXaxK0-","colab_type":"code","outputId":"5ce27ac9-cac7-4822-e614-6181ec390213","executionInfo":{"status":"ok","timestamp":1582463963545,"user_tz":300,"elapsed":738,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}},"colab":{"base_uri":"https://localhost:8080/","height":282}},"source":["fig = plt.figure()\n","ax1 = fig.add_subplot(111)\n","ax1.scatter(data2.iloc[:, 0], data2.iloc[:, 1])"],"execution_count":7,"outputs":[{"output_type":"execute_result","data":{"text/plain":["<matplotlib.collections.PathCollection at 0x7fa0afc673c8>"]},"metadata":{"tags":[]},"execution_count":7},{"output_type":"display_data","data":{"image/png":"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\n","text/plain":["<Figure size 432x288 with 1 Axes>"]},"metadata":{"tags":[]}}]},{"cell_type":"markdown","metadata":{"id":"evNrY68L7wd6","colab_type":"text"},"source":["# **Trying to see if there is a linear relationship between them**"]},{"cell_type":"code","metadata":{"id":"GDoG54tpyA39","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":106},"outputId":"6868beed-acf0-42da-bb6e-4cb59385a4ea","executionInfo":{"status":"ok","timestamp":1582463965465,"user_tz":300,"elapsed":349,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}}},"source":["\n","data2.corr()"],"execution_count":8,"outputs":[{"output_type":"execute_result","data":{"text/html":["<div>\n","<style scoped>\n","    .dataframe tbody tr th:only-of-type {\n","        vertical-align: middle;\n","    }\n","\n","    .dataframe tbody tr th {\n","        vertical-align: top;\n","    }\n","\n","    .dataframe thead th {\n","        text-align: right;\n","    }\n","</style>\n","<table border=\"1\" class=\"dataframe\">\n","  <thead>\n","    <tr style=\"text-align: right;\">\n","      <th></th>\n","      <th>Health_care_level</th>\n","      <th>Avg increasing rate</th>\n","    </tr>\n","  </thead>\n","  <tbody>\n","    <tr>\n","      <th>Health_care_level</th>\n","      <td>1.000000</td>\n","      <td>-0.782804</td>\n","    </tr>\n","    <tr>\n","      <th>Avg increasing rate</th>\n","      <td>-0.782804</td>\n","      <td>1.000000</td>\n","    </tr>\n","  </tbody>\n","</table>\n","</div>"],"text/plain":["                     Health_care_level  Avg increasing rate\n","Health_care_level             1.000000            -0.782804\n","Avg increasing rate          -0.782804             1.000000"]},"metadata":{"tags":[]},"execution_count":8}]},{"cell_type":"markdown","metadata":{"id":"krphOCgF72im","colab_type":"text"},"source":["# **Create linear model and fit the data**"]},{"cell_type":"code","metadata":{"id":"fYXweLuj1AYH","colab_type":"code","colab":{}},"source":["training = skl.LinearRegression()\n"],"execution_count":0,"outputs":[]},{"cell_type":"code","metadata":{"id":"mJhezrjH1Ls-","colab_type":"code","colab":{}},"source":["x = data2.loc[:,'Health_care_level']\n","y = data2.loc[:,'Avg increasing rate']"],"execution_count":0,"outputs":[]},{"cell_type":"code","metadata":{"id":"5uIx3qc-1qwu","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":34},"outputId":"6d3212d3-4335-4c23-c9b9-23c04e0d7e3e","executionInfo":{"status":"ok","timestamp":1582463969243,"user_tz":300,"elapsed":577,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}}},"source":["training.fit(x[:, np.newaxis], y)"],"execution_count":11,"outputs":[{"output_type":"execute_result","data":{"text/plain":["LinearRegression(copy_X=True, fit_intercept=True, n_jobs=None, normalize=False)"]},"metadata":{"tags":[]},"execution_count":11}]},{"cell_type":"code","metadata":{"id":"Nzq4yAGS1x7n","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":34},"outputId":"29d630d2-6ab8-4996-9f68-7332b254e9c5","executionInfo":{"status":"ok","timestamp":1582463970636,"user_tz":300,"elapsed":296,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}}},"source":["training.coef_\n"],"execution_count":12,"outputs":[{"output_type":"execute_result","data":{"text/plain":["array([-22.8125787])"]},"metadata":{"tags":[]},"execution_count":12}]},{"cell_type":"code","metadata":{"id":"B5O8nl9m2P9w","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":34},"outputId":"f53078f6-463a-4a3e-f69e-2d2a1158a9d1","executionInfo":{"status":"ok","timestamp":1582463971607,"user_tz":300,"elapsed":368,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}}},"source":["training.intercept_"],"execution_count":13,"outputs":[{"output_type":"execute_result","data":{"text/plain":["43.15546117984954"]},"metadata":{"tags":[]},"execution_count":13}]},{"cell_type":"markdown","metadata":{"id":"DlmnEqFC77UB","colab_type":"text"},"source":["Plotting the answer"]},{"cell_type":"code","metadata":{"id":"PtdKXBK52Tm9","colab_type":"code","colab":{"base_uri":"https://localhost:8080/","height":282},"outputId":"84d9f77d-fa9d-4204-b181-d63685680c51","executionInfo":{"status":"ok","timestamp":1582463997099,"user_tz":300,"elapsed":647,"user":{"displayName":"吕洪睿","photoUrl":"","userId":"01625523201191733760"}}},"source":["%matplotlib inline\n","regression_case = np.linspace(0, 2)\n","fig = plt.figure()\n","ax2 = fig.add_subplot(111)\n","ax2.scatter(data2.iloc[:, 0], data2.iloc[:, 1])\n","ax2.scatter(regression_case[:, np.newaxis], training.predict(regression_case[:,np.newaxis]), color = 'r')"],"execution_count":16,"outputs":[{"output_type":"execute_result","data":{"text/plain":["<matplotlib.collections.PathCollection at 0x7fa0accca4a8>"]},"metadata":{"tags":[]},"execution_count":16},{"output_type":"display_data","data":{"image/png":"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\nny3/GknnZK+zqKSa3i3ppGz6z6dryuaf7dB6ulLSv9R47oCkA9nvU7PpU8uoadbyn5R0fZHrKXvd\n35L0VkkP13l8VNKdkizp7ZLuL3I9NVjTO6ffS9L7pmvK5g9KOr1D6+piSXe0+9nnWdOsZT8o6e6i\n15WkpZLemk2fIun7Nf7+lbJdddse+IWS9kfEgYj4paSbJF0+a5nLJd2QTd8i6T22nY3fFBHPR8QP\nJO3PXq/wmiLinoh4Lpu9T9JZObxvWzXNYZWkXRHxdEQ8I2mXpMs6UNNHJd2Yw/vOKSL+W9LTcyxy\nuaQvR8V9kvptL1Vx62nemiLiW9l7SuVsTw3VNYd2tsc8ayprmzocEd/Npn8h6VFJQ7MWK2W76rYG\nPiTpR1XzT+jEFfPyMhHxoqSfSTqtwecWVVO1q1T5l3faa21P2L7P9uoc6mmmpt/L/vt2i+2zm3xu\nUTUpi5jOkXR31XAR66kR9eouaj01a/b2FJK+YXu37bEO1PMO2w/avtP2m7Kxjq8r2yep0gi/VjVc\n+LpyJcJdKen+WQ+Vsl1xU+Mc2f4jSSOSfrtqeFlETNo+V9LdtvdGxOMllPMfkm6MiOdt/5kq/2u5\npIT3bcRHJN0SEcerxjq1nrqW7Xer0sDfVTX8rmw9vV7SLtv/l+2lluG7qnxOz9oelbRD0nklvfd8\nPijpmxFRvbde6Lqy/TpV/sH4VET8PK/XbUa37YFPSjq7av6sbKzmMrYXS/oVSU81+NyiapLtSyVt\nkPShiHh+ejwiJrPfByTdq8q/1oXXFBFPVdXxRUlva/S5RdVU5SOa9V/dgtZTI+rVXdR6aojt31Dl\nc7s8Ip6aHq9aT0ck3ap8YsKGRMTPI+LZbHpc0hLbp6vD6yoz1zaV+7qyvUSV5r0tIrbXWKSc7Srv\ngL/NLwcWqxLqn6NXvgx506xlPqGZX2J+JZt+k2Z+iXlA+XyJ2UhNK1X5Eue8WeOnSnpNNn26pMeU\nw5c7Dda0tGr6dyXdF698ifKDrLZTs+mBMmrKlnujKl8uuej1VPX6y1X/i7n3a+aXTd8ucj01WNOw\nKt/hvHPW+MmSTqma/paky/KqqYG6fnX6c1OlGf4wW28NffZF1JQ9/iuq5OQnl7Gusj/zlyV9bo5l\nStmucvvgc/ywRlX5VvdxSRuysb9XZc9Wkl4r6avZBv5tSedWPXdD9rx9kt5XYk3/Jeknkh7Ifm7P\nxt8paW+2Qe+VdFWJNW2S9Ej23vdIemPVc/8kW3/7Jf1xWTVl838rafOs5xW5nm6UdFjSC6rkjVdJ\n+rikj2ePW9Lns5r3ShopYT3NV9MXJT1TtT1NZOPnZuvoweyz3ZBXTQ3W9ZdV29R9qvoHptZnX0ZN\n2TJXqnIAQ/XzCltXqkRaIemhqs9otBPbFafSA0Ciui0DBwA0iAYOAImigQNAomjgAJAoGjgAJIoG\nDgCJooEDQKL+HyJ1HN2RveG8AAAAAElFTkSuQmCC\n","text/plain":["<Figure size 432x288 with 1 Axes>"]},"metadata":{"tags":[]}}]}]}