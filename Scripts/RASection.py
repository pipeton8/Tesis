#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 15 16:53:17 2019

@author: pipeton8
"""
from __future__ import print_function
import time
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt

### Plot Parameters
mpl.rcParams['axes.spines.right'] = False
mpl.rcParams['axes.spines.top'] = False
plt.rc('text', usetex=True)

### Figure path
Fpath = "../Documento Final/Graphics/"

### Economy parameters
numPointsPlot = 1000 # Points to draw graphs
numPointsSample = 10**7

numSigma_plot = 5 # Number of different standard deviations to plot
numSigma_calc = 10**2 # Number of different standard deviations to compute results

min_alpha = 10**(-6)
max_alpha = 1

mu_y = 10
sigma_y0 = 0.25
sigma_y1 = [0.25, 1.25]

def generateAlphaDistribution():
    ### Alpha distribution
    alpha_l = np.random.uniform(min_alpha, max_alpha, numPointsSample)
    alpha_l.sort()
    print("Done creating alpha")
    
    return alpha_l

def generateIncomeDistribution(sigma):
    ### Return
    return np.sort(np.random.lognormal(mu_y,sigma,numPointsSample))
        
def plotDensities():
    ### Linspace
    x_min = 10**(-6)
    x_max = 80000
    x = np.linspace(x_min, x_max, numPointsPlot)

    ### Create figures
    f, ax1 = plt.subplots()
    g, ax2 = plt.subplots()
    
    ### Plot median in density graph
    ax1.plot(np.exp([mu_y, mu_y]), [0,1], linestyle = (0, (4, 10)), color = 'black', alpha = 0.6)
    
    ### Plot densities and joints
    pdf0 = (np.exp(-(np.log(x) - mu_y)**2 / (2 * sigma_y0**2)) / (x * sigma_y0 * np.sqrt(2 * np.pi)))
    
    ax1.plot(x, pdf0, label = r'$y_0$ $(\sigma = {:.2f})$'.format(sigma_y0), linewidth = 3, color = '#4a69bd')
    ax2.plot(x, np.cumsum(pdf0 * (x_max-x_min)/numPointsPlot), label = r'$y_0$ $(\sigma = {:.2f})$'.format(sigma_y0), linewidth = 3, color = '#4a69bd')
         
    for sigma in [sigma_y1[0] + k*0.2 for k in range(1, numSigma_plot + 1)]:
        pdf1 = (np.exp(-(np.log(x) - mu_y)**2 / (2 * sigma**2)) / (x * sigma * np.sqrt(2 * np.pi)))        
        ax1.plot(x, pdf1, label = r'$y_1$ $(\sigma = {:.2f})$'.format(sigma))
        ax2.plot(x, np.cumsum(pdf1 * (x_max-x_min)/numPointsPlot), label = r'$y_1$ $(\sigma = {:.2f})$'.format(sigma))

    ### Customize density plot
    ax1.set_xlabel(r'$y$')
    ax1.set_xticks([10000*k for k in range(9)])
    ax1.set_xticklabels([r'${:d}$'.format(20000*(k//2)) if k % 2 == 0 else '' for k in range(9)])
    ax1.set_ylim([0, np.max(pdf0)*(1.05)])
    ax1.set_yticks([])
    ax1.legend(loc = 'upper right')
    
    ### Customize joint plot
    ax2.set_xlabel(r'$y$')
    ax2.set_ylabel(r'$\alpha$', rotation = 0, labelpad = 10)
    ax2.set_xticks([10000*k for k in range(9)])
    ax2.set_xticklabels([r'${:d}$'.format(20000*(k//2)) if k % 2 == 0 else '' for k in range(9)])
    ax2.legend(loc = 'lower right', bbox_to_anchor=(.95, 0.05))    

    ### Save figures    
    f.savefig(Fpath+"RAwageDistribs.pdf", bbox_inches='tight')
    g.savefig(Fpath+"RAjointDistribs.pdf", bbox_inches='tight')

def computeCovariance(alpha_l, y):    
    E_prod = np.mean(np.multiply(alpha_l, y))
    prod_E = np.mean(alpha_l) * np.mean(y)
        
    return E_prod - prod_E

def computeOptimalAlpha(alpha,y):
    return np.mean(alpha) + computeCovariance(alpha,y)/np.mean(y)
    
def computeError(alpha, y0, y1):
    ### Get covariances
    Cov1 = computeCovariance(alpha, y1)
    Cov0 = computeCovariance(alpha, y0)
    
    ### Expectations
    Ey0 = np.mean(y0)
    Ey1 = np.mean(y1)
    
    ### Return
    return Cov1 - Cov0/Ey0 * Ey1

def computeDiffAlphas(alpha, y0, sigma1_l):
    Diff = []
    OpAlphas = []
    
    for sigma in sigma1_l:
        ### Income distribution for this sigma
        y1 = generateIncomeDistribution(sigma)
        
        ### Compute Diff and optimal alpha
        Diff.append(computeError(alpha, y0, y1))
        OpAlphas.append(computeOptimalAlpha(alpha, y1))
        
        ### Print progress
        if len(Diff) % 10 == 0:
            print("{:d}".format(len(Diff)), end = '')
        else:
            print(".", end = '')
    
    print("")
    
    return Diff, OpAlphas


def plotDiffAlphas(Diff, OpAlphas, alphaBar):
    ### Linspace
    x = np.linspace(sigma_y1[0], sigma_y1[1], numSigma_calc)

    ### Create figure
    f, ax1 = plt.subplots()
    g, ax2 = plt.subplots()
    
    ### Plot Diff graph
    ax1.plot(x,Diff, color = '#4a69bd', linewidth = 2.5)
    ax1.plot(sigma_y1, [Diff[0], Diff[-1]], color = 'black', linestyle = (0,(4,6)), alpha = 0.6)
             
    ### Customize Diff graph
    ax1.set_xlabel(r'$\sigma_1$')
    ax1.set_xticks([k*0.25 for k in range(1,6)])
    
    ax1.set_ylabel(r'$D_1(\overline{\alpha})$', rotation = 0, labelpad = 20)
    ax1.set_ylim(top = np.max(Diff) + 1000)
    ax1.set_yticks([k*2000 for k in range(7)])
             
    ### Plot Alpha graph
    ax2.plot(x,OpAlphas, color = '#4a69bd', linewidth = 2.5)
    ax2.plot(sigma_y1, [OpAlphas[0], OpAlphas[-1]], color = 'black', linestyle = (0,(4,6)), alpha = 0.6)

    ### Customize Alpha graph
    ax2.set_xlabel(r'$\sigma_1$')
    ax2.set_xticks([k*0.25 for k in range(1,6)])

    ax2.set_ylabel(r'$\overline{\alpha}$', rotation = 0, labelpad = 15)
    ax2.set_ylim([0.45, 1.05])
    ax2.set_yticks([0.5 + k*0.1 for k in range(6)])
    
    ### Save figures
    f.savefig(Fpath+"RAdiffPlot.pdf", bbox_inches='tight')
    g.savefig(Fpath+"RAdiffAlphasPlot.pdf", bbox_inches='tight')

def plotAlphas(OpAlphasBig, bigSigma_l):
    ### Create figure
    f = plt.figure()
    
    ### Plot graph
    plt.plot(bigSigma_l,OpAlphasBig, color = '#4a69bd', linewidth = 2.5)
    plt.plot([bigSigma_l[0],bigSigma_l[-1]], [OpAlphasBig[0], OpAlphasBig[-1]], color = 'black', linestyle = (0,(4,6)), alpha = 0.6)
    
    ### Customize graph
    plt.xlabel(r'$\sigma$')
    plt.ylabel(r'$\overline{\alpha}$', rotation = 0, labelpad = 15)
    plt.ylim([0.45, 1.05])
    
    ### Save figure
    f.savefig(Fpath+'RAoptimalAlphas.pdf', bbox_inches = 'tight')
    
    
if __name__ == "__main__":
#    t0 = time.time()
#    
#    np.random.seed(0)
#    
#    ### Sigma Linspace
#    sigma1_l = np.linspace(0.25, sigma_y1[-1], numSigma_calc)
#
#    ### Generate distribution for both y and alpha
#    plotDensities()
#    print("Done graphs")
#    
#    ### Distributions
#    alpha = generateAlphaDistribution()
#    y0 = generateIncomeDistribution(sigma_y0)
#    print("Done creating y0")
#    
#    ### Compute \overline{\alpha}
#    alphaBar = computeOptimalAlpha(alpha,y0)
#    
#    print("Optimal alpha is: {:.3f}".format(alphaBar))
#    
#    ### Compute diff and optimal alphas
#    Diff, OpAlphas = computeDiffAlphas(alpha, y0, sigma1_l)
#    
#    ### Plot diff and optimal alpha graphs
#    plotDiffAlphas(Diff, OpAlphas, alphaBar)
#    
#    ### Plot optimal alphas for general standard deviations
#    bigSigma_l = np.linspace(0, 5, numSigma_calc * 2)
#    
#    _, OpAlphasBig = computeDiffAlphas(alpha, alpha, bigSigma_l)
#    
#    plotAlphas(OpAlphasBig, bigSigma_l)
    
    ### To find derivatives of certain size
    for i in range(np.size(bigSigma_l)-1):
        ddx = (OpAlphasBig[i+1] - OpAlphasBig[i])/(bigSigma_l[i+1] - bigSigma_l[i])
        ddx_rel = ddx/OpAlphasBig[i]
        
        if ddx_rel <= 2 * 10**(-2) and ddx_rel > 0:
            print(ddx)
            print(ddx_rel)
            print(i)
            print(bigSigma_l[i])
            print(OpAlphasBig[i])
            break
    
    ### Done! Print the time
#    print("Done! I took {:.0f} seconds".format(time.time() - t0))