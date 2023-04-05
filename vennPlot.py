import os
import pandas as pd
import venn
import matplotlib.pyplot as plt

os.chdir(r'D:\学习\研究生\毕业论文')
eDNA = pd.read_excel('eDNAreads.xlsx', sheet_name='Sheet1')
fishnet = pd.read_excel('traditionalSites.xlsx', sheet_name='Sheet1')
eDNA = eDNA.T
eDNA = eDNA.iloc[1:, :]
eDNA['eDNA Based'] = [1 for i in range(0, len(eDNA.index))]
eDNA.reset_index(inplace=True)
eDNA = eDNA[['index', 'eDNA Based']]
fishnet = fishnet.T
fishnet = fishnet.iloc[1:, :]
fishnet['Fishing-net Based'] = [1 for i in range(0, len(fishnet.index))]
fishnet.reset_index(inplace=True)
fishnet = fishnet[['index', 'Fishing-net Based']]
plotData = pd.merge(eDNA, fishnet, on='index', how='outer')
plotData = plotData.iloc[:, 1:]
plotData.fillna(0, inplace=True)
label = []
for i in plotData.columns:
    label.append(plotData[plotData[i] > 0].index)
nums = venn.get_labels(label, fill=['number'])
fig, ax = venn.venn2(nums, names=plotData.columns)
fig.savefig('韦恩图.pdf')
fig.savefig('韦恩图.png', bbox_inches='tight')
plt.close()

