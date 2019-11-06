#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 09:52:35 2019

@author: pipeton8
"""

import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt

### Plot Parameters
mpl.rcParams['axes.spines.right'] = False
mpl.rcParams['axes.spines.top'] = False
plt.rc('text', usetex=True)

### Other parameters
alpha_safepad = 1.5 * 10**(-1)

### Simulation parameters
numPointsPlot = 1000 # Points to draw graphs
numPointsSample = 10**4

### Figure path
Fpath = "../Documento Final/Graphics/"

### Economy parameters
min_alpha = 10**(-3)
max_alpha = 1 - 10**(-3)

mu_y = np.log(1.5)
sigma_y0 = 0.25
sigma_y1 = [0.25, 1.25]

p_0 = np.array([1,10])

def generateAlphaDistribution():
    return np.sort(np.random.uniform(min_alpha, max_alpha, numPointsSample))

def generateIncomeDistribution(sigma):
    return np.sort(np.random.lognormal(mu_y,sigma,numPointsSample))

def monotExample(alpha,p):
    y = 1 # this way all calculations are relative to income.
    
    ### Plot D_t
    alpha_bar = np.linspace(alpha_safepad, 1 - alpha_safepad, numPointsPlot)

    plotDt(alpha, alpha_bar, computeDt(y,alpha, alpha_bar,p_0) * 100)

    ### Plot difference in T_t for each alpha
    plotTt(p)
    
        
def plotTt(p):
    alpha = np.linspace(alpha_safepad, 1 - alpha_safepad, numPointsPlot)
    
    ### Compute T_t for every alpha
    T_t = np.reshape(computeTt(alpha, p),(np.size(alpha),))
    
    ### Plot T_t
    f = plt.figure()
    
    plt.plot(alpha, T_t, color = '#4a60bd', linewidth = 2)
             
    plt.ylabel(r'$T_t$', rotation = 0, labelpad = 10)    
    plt.xlabel(r'$\alpha$')
    plt.xticks([0.1*k for k in range(11)], [r'${:.1f}$'.format(0.1*k) if k % 2 == 0 else '' for k in range(11)])

    ### Save figure
    f.savefig(Fpath+"MonotonocityTt.pdf", bbox_inches='tight')

def plotDt(alpha,alpha_bar,D_t):
    
    ### If just plotting one value of alpha
    if np.size(alpha) == 1:
        ### Reshape to have 1-D differences
        D_t = np.reshape(D_t,(numPointsPlot,))
        
        ### Plot
        f = plt.figure()
        
        plt.plot([alpha, alpha], [np.min(D_t), np.max(D_t)], color = 'black', linestyle = (0, (4, 10)), alpha = 0.6)
        plt.plot([0, 1], [0, 0], color = 'black', linestyle = (0, (4, 10)), alpha = 0.6)
        plt.plot(alpha_bar, D_t, color = '#4a69bd', linewidth = 2)
                 
        plt.xlabel(r'$\overline{\alpha}$')
        plt.ylabel(r'$\%$', rotation = 0, labelpad = 10)
        plt.xticks([0.1*k for k in range(11)], [r'${:.1f}$'.format(0.1*k) if k % 2 == 0 else '' for k in range(11)])
    
        ### Save figure
        f.savefig(Fpath+"MonotonocityDt.pdf", bbox_inches='tight')
    
    else:
        ### Plot
        f = plt.figure()
        
        plt.plot([0, 1], [0, 0], color = 'black', linestyle = (0, (4, 10)), alpha = 0.6)

        for i in range(np.size(alpha)):
            plt.plot(alpha_bar, D_t[i], label = r'$\alpha = {:.1f}$'.format(alpha[i]))
            plt.xlabel(r'$\overline{\alpha}$')
            plt.ylabel(r'$\%$', rotation = 0, labelpad = 10)
            plt.xticks([0.1*k for k in range(11)], [r'${:.1f}$'.format(0.1*k) if k % 2 == 0 else '' for k in range(11)])
            plt.legend()
    
        ### Save figure
        f.savefig(Fpath+"MonotonocityDt.pdf", bbox_inches='tight')   
        
    return None

def computePalpha(alpha,p):    
    ### Matrix of prices for each alpha
    P_mat = p * np.ones((np.size(alpha),1))
        
    ### Matrix of each epsilon
    epsilon = (1-alpha)**(-1)
    
    ### Compute the power of each price
    P_mat = P_mat**(1-epsilon)
    
    ### Compute sum
    P_mat = np.sum(P_mat, 1, keepdims = True)
            
    ### Take the (1-epsilon)-root of each sum
    P_mat = P_mat**(1/(1-epsilon))
        
    return P_mat

def computeTt(alpha,p, dim = 0):
    ### Compute everything as rows
    alpha = np.reshape(alpha,(np.size(alpha),1))
    
    # dim = 0 if I want to return rows
    # dim = 1 if I want to return columns
    if dim == 0:
        shape = (np.size(alpha),1)
    else:
        shape = (1,np.size(alpha))
    
    ### Return result with the shape desired
    return np.reshape(alpha/computePalpha(alpha,p),shape)

def computeDt(y,alpha,alpha_bar, p):
    # Rows are alphas, columns are alpha_bars
    return y * (computeTt(alpha,p) - computeTt(alpha_bar,p,1))

if __name__ == '__main__':
    ### Monotonocity of D_t(y, alpha, alpha_bar)
    alpha = np.linspace(0.2, 0.8, 4)  # Mean/median of alpha
    
    monotExample(alpha, p_0)
        
    ### Set seed
    np.random.seed(0)
    
    ### Generate distributions
    y = np.reshape(generateIncomeDistribution(sigma_y0),(numPointsSample,1))
    alpha = np.reshape(generateAlphaDistribution(),(numPointsSample,1))

    ### Dt
#    alpha_bar = np.reshape(np.linspace(10**-2, 1 - 10**-2, numPointsPlot),(1,numPointsPlot))
    
#    P_alpha_bar = computePalpha(alpha_bar,p_0)
#    P_alpha = computePalpha(alpha,p_0)
    
#    computeDt(y,alpha,alpha_bar,p_0)
