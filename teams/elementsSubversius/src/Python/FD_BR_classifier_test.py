import numpy as np
import pandas as pd
import sys
import matplotlib.pyplot as plt

sys.path.insert(0,"C:/Braingaze/libraries")

from sklearn import svm,neighbors,ensemble
from sklearn.metrics import roc_curve, auc
from sklearn.cross_validation import StratifiedKFold
from sklearn.feature_selection import SelectKBest, f_classif
from sklearn.linear_model import BayesianRidge

import mc_module as mc

### DATA PREPARATION ###

df = pd.read_table('dtdt.csv',sep=',',encoding='mbcs').drop(['PATIENT','DAY'],axis=1).dropna()

y = np.array(df['EP']) # Entry labelization
np.place(y,y>0,1)

ml = df.drop(['EP'],axis=1).copy()

cols = ml.columns

Z = np.array(ml).reshape(len(ml.index),len(cols))
Z,y = np.vstack((Z[y==0],Z[y==1])), np.hstack((y[y==0],y[y==1]))
len_ = np.min(np.floor(np.array([len(y[y==0])/2,len(y[y==1])/2,len(Z[0])])))

### CROSS-VALIDATION AND GRID SEARCH ###

classifier = BayesianRidge()
cv = StratifiedKFold(y,n_folds=10)

pars = []

for k in np.arange(2,len_):

    X = SelectKBest(f_classif,k=k).fit_transform(Z,y) # Select k best-separating features

    for tsh in np.linspace(0.4,0.6,21):

        acc = 0.

        for i, (train, test) in enumerate(cv):
            classifier.fit(X[train], y[train])
            out = classifier.predict(X[test])
            np.place(out,out>tsh,1) # Label according to different thresholds
            np.place(out,out<=tsh,0)
            acc += 1. - np.sum(np.mod(out + y[test],2))/len(y[test])

        print(k,tsh,np.round(acc/(i+1),4)) # Accuracy for k features and threshold tsh
        if len(pars) == 0:
            pars = np.array([k,tsh,np.round(acc/(i+1),4)])
        else:
            pars = np.vstack((pars,np.array([k,tsh,np.round(acc/(i+1),4)])))

### PLOT COLORFUL STUFF ###

plt.scatter(pars[:,0],pars[:,1],c=pars[:,2],marker='+')
plt.scatter(pars[np.argmax(pars[:,2])][0],pars[np.argmax(pars[:,2])][1],c='red',s=50,label='Est. accuracy: '+str(np.max(pars[:,2])))
plt.legend()
plt.xlabel('n features')
plt.ylabel('threshold')
plt.title('Bayesian Ridge regressor performance for FD episode prediction')
plt.grid()
plt.show()
plt.clf()

print(pars[np.argmax(pars[:,2])])

k = pars[np.argmax(pars[:,2])][0]
tsh = pars[np.argmax(pars[:,2])][1]

kb = SelectKBest(f_classif,k=k)
kb.fit(Z,y)
X = kb.transform(Z)
classifier.fit(X,y)

t = classifier.predict(X)

plt.scatter(y,t,c=y,s=50)
plt.axhline(tsh,linestyle='--',color='k') # tsh may be changed to move along a ROC curve
plt.grid()
plt.xlabel('Class (0: no episode, 1: episode)')
plt.ylabel('BR Score')
plt.title('Class separation by Bayesian Ridge score')
plt.show()
plt.clf()

### CHECK FOR OVER-FITTING ###

U = np.random.uniform(0.,4.,size=np.shape(Z)) # Just create a random k-simplex and randomly label it
t = np.floor(np.random.uniform(0.,2.,size=np.shape(y)))
U = kb.transform(U) # Transform according to Skb rule

classifier.fit(X,y)
out = classifier.predict(U) # Predict random data

# Plot for visual inspection of the randomized data

plt.scatter(t,out,c=t,s=50)
plt.axhline(tsh,linestyle='--',color='k')
plt.grid()
plt.xlabel('Class (0: no episode, 1: episode)')
plt.ylabel('BR Score')
plt.title('Random class separation')
plt.show()
plt.clf()

np.place(out,out<=tsh,0)
np.place(out,out>tsh,1)
print(1. - np.sum(np.mod(out+t,2))/len(t)) # If not too far from 0.5 and FNR is similar fot FPR, everything fine