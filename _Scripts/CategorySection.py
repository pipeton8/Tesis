#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 09:52:35 2019

@author: pipeton8
"""

from __future__ import print_function
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
numPointsPlot = 100 # Points to draw graphs
numPointsSample = 10**4

numAlphaGrid = 10**6

numSigma_calc = 10**3 # Number of different standard deviations to compute results
numSigma_plot = 4 # Number of different standard deviations to plot
sigma_jump = 0.25

### Figure path
Fpath = "../_Paper/Graphics/"

### Economy parameters
min_alpha = alpha_safepad
max_alpha = 1 - alpha_safepad

mu_y = np.log(5.5)
sigma_y0 = 0.25
sigma_y1 = [0.25, 1.25]

p_0 = np.array([1,10])
L = [0.5, 1.5] # lambdas


def generateAlphaDistribution(min_alpha = alpha_safepad, max_alpha = 1-alpha_safepad):
    return np.sort(np.random.uniform(min_alpha, max_alpha, numPointsSample))

def generateIncomeDistribution(sigma):
    return np.sort(np.random.lognormal(mu_y,sigma,numPointsSample))

def monotExample(alpha,p):
    y = 1 # this way all calculations are relative to income.
    
    ### Plot D_t
    alpha_bar = np.linspace(alpha_safepad, 1 - alpha_safepad, numPointsPlot)

    plotDt(alpha, alpha_bar, computedt(y,alpha, alpha_bar,p_0) * 100, "MonotonocityDt.pdf")

    ### Plot difference in T_t for each alpha
    plotTt(p,"MonotonocityTt.pdf")
    
        
def plotTt(p, name):
    alpha = np.linspace(alpha_safepad, 1 - alpha_safepad, numPointsPlot)
    
    # Figure    
    f = plt.figure()
    
    # Plot parameters
    xlim = niceLim([0.1,0.9])
    xticks = [0.1 + 0.2 * k for k in range(5)]
    xticks_labels = [r'${:.1f}$'.format(xticks[k]) for k in range(len(xticks))]
    
    if np.size(p) == 2:
        ylim = niceLim([0.5, 3])
        yticks = [0.5 * k for k in range(1,7)]
        yticks_labels = [r'${:.1f}$'.format(yticks[k]) for k in range(len(yticks))]
    
        T_t = np.reshape(computeTt(alpha, p),(np.size(alpha),))  
        
        plt.plot(alpha, T_t, color = '#4a60bd', linewidth = 2)
                 
    else:
        ylim = niceLim([0,6])
        yticks = [k for k in range(7)]
        yticks_labels = [r'${:d}$'.format(yticks[k]) if k % 2 == 0 else '' for k in range(len(yticks))]        
        
        for p_i in p:
            kwargs = Ttkwargs(p_i,p)
            
            T_t = np.reshape(computeTt(alpha, p_i),(np.size(alpha),))    
            
            plt.plot(alpha, T_t, **kwargs)
        
        plt.legend()
             
    plt.xlabel(r'$\alpha$')
    plt.ylabel(r'$T_t$', rotation = 0, labelpad = 10)    

    plt.xlim(xlim)
    plt.xticks(xticks, xticks_labels)
    
    plt.ylim(ylim)
    plt.yticks(yticks, yticks_labels)

    ### Save figure
    f.savefig(Fpath+name, bbox_inches='tight')

def Ttkwargs(p_i,p):
    
    if (p_i == p[0]).all():
        return {'linewidth' : 2.5,
                'color' : '#4a60bf',
                'label' : r'$p_0 = ({:.1f},{:.1f})$'.format(p_i[0], p_i[1])
                }
        
    elif (p_i == p[1]).all():
        return {'linewidth' : 1.5,
                'color' : 'darkorange',
                'label' : r'$p_1 = ({:.1f},{:.1f})$'.format(p_i[0], p_i[1])
                }
        
    elif (p_i == p[2]).all():
        return {'linewidth' : 1.5,
                'color' : 'mediumorchid',
                'label' : r'$p_1 = ({:.1f},{:.1f})$'.format(p_i[0], p_i[1])
                }

def plotDt(alpha,alpha_bar,D_t,name):
    
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
        f.savefig(Fpath+name, bbox_inches='tight')
    
    else:
        ### Plot
        f = plt.figure()
        
        # Plot parameters
        lineKwargs = {'color' : 'black', 'linestyle' : (0, (4, 10)), 'alpha' : 0.6}
        
        xlim = niceLim([0.1,0.9])
        xticks = [0.1 + 0.2 * k for k in range(5)]
        xticks_labels = [r'${:.1f}$'.format(xticks[k]) for k in range(len(xticks))]
        
        ylim = niceLim([-225, 75])
        yticks = [-225 + 75*k for k in range(5)]
        yticks_labels = [r'${:d}$'.format(yticks[k]) for k in range(len(yticks))]
        
        plt.plot([0, 1], [0, 0], **lineKwargs)

        for i in range(np.size(alpha)):
            plt.plot(alpha_bar, D_t[i], label = r'$\alpha = {:.1f}$'.format(alpha[i]))

        plt.xlabel(r'$\overline{\alpha}$')
        plt.ylabel(r'$\%$', rotation = 0, labelpad = 10)

        plt.xlim(xlim)
        plt.xticks(xticks, xticks_labels)
        
        plt.ylim(ylim)
        plt.yticks(yticks, yticks_labels)

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
    # else return previous shape
    if dim == 0:
        shape = (np.size(alpha),1)
    elif dim == 1:
        shape = (1,np.size(alpha))
    else:
        shape = (np.size(alpha),)

    ### Return result with the shape desired
    return np.reshape(alpha/computePalpha(alpha,p),shape)

def computedt(y,alpha,alpha_bar, p):
    # Rows are alphas, columns are alpha_bars
    return y * (computeTt(alpha,p) - computeTt(alpha_bar,p,1))

def plotJointDist(p_0, L):
    ### Linspace
    x_min = 10**(-6)
    x_max = 30
    y = np.linspace(x_min, x_max, numPointsPlot)
    
    ### Create graphs
    f, ax1 = plt.subplots()
    G = []
    AX2 = []
    
    for i in range(np.size(L)):
        g, ax2 = plt.subplots()
        G.append(g)
        AX2.append(ax2)
    
    ### Plot parameters
    xlabel = r'$y_t$'
    Ttlabel = r'$T_t$'
    alphalabel = r'$\alpha$'

    xlim = [-1, 31]
    xticks = [5*k for k in range(7)]
    xticks_labels = [r'${:d}$'.format(xticks[k]) if k % 2 == 0 else '' for k in range(7)]

    alim = [0.1, 0.9]
    aticks = [0.15 + (0.35/2) * k for k in range(5)]
    aticks_labels = [r'${:.2f}$'.format(aticks[k]) if k % 2 == 0 else '' for k in range(len(aticks))]
    
    ### Plot gray dashed line at median
    ax1.plot([np.exp(mu_y), np.exp(mu_y)], alim, color = 'black', linestyle = (0, (4, 10)), alpha = 0.6)
    
    TtBounds = [[10**24, 0] for i in range(np.size(L))]
    
    ### Generate alpha
#    alpha = generateAlphaDistribution()
    
    for sigma in [sigma_y1[0] + k*sigma_jump for k in range(numSigma_plot + 1)]:
        ### kwargs with sigma
        kwargs = getKwargs(sigma)
        
        ### "alpha"
        pdf = (np.exp(-(np.log(y) - mu_y)**2 / (2 * sigma**2)) / (y * sigma * np.sqrt(2 * np.pi)))
        alpha = compressAlpha(np.cumsum(pdf * (x_max-x_min)/numPointsPlot))

        ### Plot y_t, alpha_t
        ax1.plot(y, alpha, **kwargs)
        
        ### Compute and plot T_t in every suboplot
        for i in range(np.size(L)):
            p = setP(sigma, p_0, L[i])
            
            Tt = np.reshape(computeTt(alpha, p),(np.size(alpha),))
            
            TtBounds[i] = updateTtBounds(TtBounds[i], [np.min(Tt), np.max(Tt)])
            
            AX2[i].plot(y, Tt, **kwargs)
        
    ### Customize y_t,alpha_t plot ans save
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(alphalabel, rotation = 0, labelpad = 10)
    ax1.set_xlim(xlim)
    ax1.set_xticks(xticks)
    ax1.set_xticklabels(xticks_labels)

    ax1.set_ylim(alim)
    ax1.set_yticks(aticks)
    ax1.set_yticklabels(aticks_labels)
    ax1.legend(loc = 'lower right')
    
    f.savefig(Fpath+"JointYA.pdf", bbox_inches = 'tight')
    
    ### Customize y_t, T_t plots and save
    for i in range(np.size(L)):
        ylim = niceLim(TtBounds[i])
        yticks, yticks_labels = Ttticks(i)
        
        AX2[i].plot([np.exp(mu_y), np.exp(mu_y)], ylim, color = 'black', linestyle = (0, (4, 10)), alpha = 0.6)

        AX2[i].set_xlabel(xlabel)
        AX2[i].set_ylabel(Ttlabel, rotation = 0, labelpad = 10)
        
        AX2[i].set_xlim(xlim)
        AX2[i].set_ylim(ylim)

        AX2[i].set_xticks(xticks)
        AX2[i].set_xticklabels(xticks_labels)

        AX2[i].set_yticks(yticks)
        AX2[i].set_yticklabels(yticks_labels)

        AX2[i].legend()
        
        G[i].savefig(Fpath+"JointYT_{:d}.pdf".format(i), bbox_inches = 'tight')

def compressAlpha(alpha):
    return (alpha - 0.5) * (1 - 2 * alpha_safepad) + 0.5

def niceLim(bounds):
    padPercentage = 0.08
    boundsDiff = bounds[1] - bounds[0]
    pad = boundsDiff/(1 - 2*padPercentage) * padPercentage
    
    return [bounds[0] - pad, bounds[1] + pad]
    
def Ttticks(i):
    if i == 0:
        tickLimits = [0.5, 5.5]
        diff = tickLimits[1] - tickLimits[0]
        yticks = [tickLimits[0] + diff/4 * k for k in range(5)]
        yticks_labels = [r'${:.1f}$'.format(yticks[k]) if k % 2 == 0 else '' for k in range(len(yticks))]
    else:
        tickLimits = [0.25, 2.75]
        diff = tickLimits[1] - tickLimits[0]
        yticks = [tickLimits[0] + diff/4 * k for k in range(5)]
        yticks_labels = [r'${:.2f}$'.format(yticks[k]) if k % 2 == 0 else '' for k in range(len(yticks))]
        
    return yticks, yticks_labels
    
def updateTtBounds(TtBounds, newBounds):
    newLeft = min(TtBounds[0], newBounds[0])
    newRight = max(TtBounds[1], newBounds[1])
    
    return [newLeft, newRight]
        
def getKwargs(sigma):
    if sigma == sigma_y0:
        kwargs = { 'label'     : setLabelName(0,sigma),
                    'linewidth' : 3,
                    'color'     : '#4a69bd'
                    }
    else:
        kwargs = {'label' : setLabelName(1,sigma)}
    
    return kwargs
        

def setLabelName(i,sigma):

    return r'$y_{:d}$ ($\sigma = {:.2f}$)'.format(i,sigma)
        
def setP(sigma, p_0, L):
    if sigma == sigma_y0:
        return p_0
    else:
        return p_0 * L

def generateAlphaBarGrid(min_alpha = alpha_safepad, max_alpha = 1-alpha_safepad):
    return np.linspace(min_alpha, max_alpha, numAlphaGrid)

def computeCovariance(y,T):
    E_prod = np.mean(np.multiply(T, y))
    prod_E = np.mean(T) * np.mean(y)
        
    return E_prod - prod_E

def computeOptimalAlpha(params, T, y,p):

    cov = computeCovariance(y,T)
    
    Mt = computeMt(cov,np.mean(y),np.mean(T))
    
    Tgrid = computeTt(params,p)
    
    absD = abs(Mt - Tgrid)
    
    i = np.argmin(absD)
    
    return params[i]

def computeMt(cov, meanY, meanT):
    return cov/meanY + meanT

def computeDt(Tbar, Mt):
    return Mt - Tbar

def computeDiffAlphas(params, alpha, y0, p0, L, sigma1_l):
    Diffs = [[], []]
    OpAlphas = []

    p1 = p0 * L

    ## T distribution
    T = computeTt(alpha,p0)

    ### Compute Diff and optimal alpha at t=0
    alphaBar = computeOptimalAlpha(params, T, y0, p0)
    Tbar0 = computeTt(alphaBar, p0, dim = -1)[0]
    Tbar1 = L**(-1) * Tbar0
    
    print("For lambda = {:.1f}, alpha = {:.2f}, T0 = {:.2f} and T1 = {:.2f}".format(L,alphaBar,Tbar0, Tbar1))
    
    for sigma in sigma1_l:
        ### Income distribution for this sigma
        y1 = generateIncomeDistribution(sigma)
        T = computeTt(alpha,p1)
        
        Mt = computeMt(computeCovariance(y1,T), np.mean(y1), np.mean(T))
        
        Diffs[0].append(computeDt(Tbar0, Mt))
        Diffs[1].append(computeDt(Tbar1, Mt))
        OpAlphas.append(computeOptimalAlpha(params, T, y1, p1))
        
        ### Print progress
        if len(Diffs[0]) % 100 == 0:
            print("{:d}".format(len(Diffs[0])), end = '')
        elif len(Diffs[0]) % 20 == 0:
            print(".", end = '')
    
    print("")
    
    return Diffs, OpAlphas

def plotDtvSigma(params, alpha, y0, p0, L, sigma1_l):
    
    for i in range(len(L)):
        Diffs, OpAlphas = computeDiffAlphas(params, alpha, y0, p0, L[i], sigma1_l)
        
        for j in range(len(Diffs)):
            ### Create figure
            f, ax1 = plt.subplots()

            ### Plot Diff graphs
            ax1.plot(sigma1_l,Diffs[j], linestyle = '', marker = '.', color = '#4a69bd', linewidth = 2.5)
    #        ax1.plot(sigma_y1, [Diff[0], Diff[-1]], color = 'black', linestyle = (0,(4,6)), alpha = 0.6)
                     
            ### Customize Diff graph
            ax1.set_xlabel(r'$\sigma_1$')
            ax1.set_xticks([k*0.25 for k in range(1,6)])
            
            ax1.set_ylabel(r'$D_1(\overline{T}'+'_{:d})$'.format(j), rotation = 0, labelpad = 20)
            if j == 1:
                ax1.set_ylim(niceLim([-10**-6, 10**-6]))
#                ax1.set_yticks([])
                
            else:
                ax1.set_ylim(niceLim[-0.2, 0.2])
           
        #    ax1.set_yticks([k*2000 for k in range(7)])

            ### Save figures
            f.savefig(Fpath+"Categorydiff{:d}Plot_{:d}.pdf".format(j,i), bbox_inches='tight')
                
        ### Create figure
        g, ax2 = plt.subplots()        
                 
        ### Plot Alpha graph
        ax2.plot(sigma1_l,OpAlphas, linestyle = '', marker = '.', color = '#4a69bd', linewidth = 2.5)
#        ax2.plot(sigma_y1, [OpAlphas[0], OpAlphas[-1]], color = 'black', linestyle = (0,(4,6)), alpha = 0.6)
    
        ### Customize Alpha graph
        ax2.set_xlabel(r'$\sigma_1$')
        ax2.set_xticks([k*0.25 for k in range(1,6)])
    
        ax2.set_ylabel(r'$\overline{\alpha}$', rotation = 0, labelpad = 15)
        ax2.set_ylim([0.15, 0.85])
    #    ax2.set_yticks([0.5 + k*0.1 for k in range(6)])
        
        ### Save figures
        g.savefig(Fpath+"CategoryAlphasPlot_{:d}.pdf".format(i), bbox_inches='tight')


if __name__ == '__main__':
    ### Set seed
    np.random.seed(0)

    ### Monotonocity of D_t(y, alpha, alpha_bar)
#    alpha = np.linspace(0.2, 0.8, 4)
    
#    monotExample(alpha, p_0)
    
    ### Plot T_t
#    plotTt([p_0, L[0]*p_0, L[1]*p_0],"TtLambdas.pdf")

    ### Plot joint distributions
#    plotJointDist(p_0, L)
    
    ## Generate distributions
    y_0 = generateIncomeDistribution(sigma_y0)
    alpha = generateAlphaDistribution()
    
    ### Grid of optimal alphas
    alphaBarParams = generateAlphaBarGrid()
    
    ### Plot Diff vs sigma
    sigma1_l = np.linspace(sigma_y1[0], sigma_y1[1], numSigma_calc)

    plotDtvSigma(alphaBarParams, alpha, y_0, p_0, L, sigma1_l)

    