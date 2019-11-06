#/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 15 16:53:17 2019

@author: pipeton8
"""
from __future__ import print_function
from copy import deepcopy
import numpy as np
#import math

import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from HARK.ConsumptionSaving.ConsRepAgentModel import RepAgentConsumerType
import HARK.ConsumptionSaving.ConsumerParameters as Params

from CarrollSection_KS_Unemp import consExample

### Plot Parameters
mpl.rcParams['axes.spines.right'] = False
mpl.rcParams['axes.spines.top'] = False
plt.rc('text', usetex=True)

### Figure path
Fpath = "../Documento Final/Graphics/"

### Economy parameters
numAgents = 200
numPointsRho = 50 # Points to search for optimal parameters
numPointsBeta = 1000

min_CRRA = 1
mu_CRRA = 3
max_CRRA = 5

min_DiscFac = 0.90
auxMin_DiscFac = 0.94
auxMax_DiscFac = 0.96
mu_DiscFac = 0.99
max_DiscFac = 0.99

min_y = 0.001
max_y = 10
#mu_y = nan

def cFunc(CRRA, DiscFac):
    # Parameters
    RA_params = deepcopy(Params.init_idiosyncratic_shocks)
    RA_params['CRRA'] = CRRA
    RA_params['DiscFac'] = DiscFac
    RA_params['DeprFac'] = 0.025
    RA_params['CapShare'] = 0.36
    RA_params['UnempPrb'] = 0.0
    RA_params['LivPrb'] = [1.0]
    
    # Make and solve a rep agent model
    RAmodel = RepAgentConsumerType(**RA_params)
    RAmodel.solve()

    # Consumption function
    return RAmodel.solution[0].cFunc

def plotCRRA(CRRA_l):
      # Plots
    print('Consumption function:')
    m_grid = np.linspace(0,5,100)

    f = plt.figure()
    plt.xlabel(r'$y$')
    plt.ylabel(r'$c$', rotation = 0, labelpad = 10)
    plt.ylim([0, 1.5])
    plt.xlim([0, 5])
    
    for CRRA in CRRA_l:
        c_m = cFunc(CRRA,mu_DiscFac)(m_grid)
        plt.plot(m_grid,c_m, label = r'$\rho = {:.0f}$'.format(CRRA))

    plt.legend(loc = 'upper left')
    plt.show()
    f.savefig(Fpath+"CarrollConsRho.pdf", bbox_inches='tight')

def plotDiscFac(DiscFac_l):
      # Plots
    print('Consumption function:')
    m_grid = np.linspace(0,5,100)

    f = plt.figure()
    plt.xlabel(r'$y$')
    plt.ylabel(r'$c$', rotation = 0, labelpad = 10)
    plt.ylim([0, 1.5])
    plt.xlim([0, 5])
    
    for DiscFac in DiscFac_l:
        c_m = cFunc(mu_CRRA,DiscFac)(m_grid)
        plt.plot(m_grid,c_m, label = r'$\beta = {:.3f}$'.format(DiscFac))

    plt.legend(loc = 'upper left')
    plt.show()
    f.savefig(Fpath+"CarrollConsBeta.pdf", bbox_inches='tight')

def generateDistribution():
    # Generate distribution
    np.random.seed(0)
    CRRA_l = np.random.triangular(min_CRRA,mu_CRRA,max_CRRA,numAgents)
    DiscFac_l = np.random.triangular(min_DiscFac,mu_DiscFac,max_DiscFac,numAgents)
    y = np.random.uniform(min_y,max_y,numAgents)
    
    # Plot
    plotDistribution(CRRA_l, DiscFac_l)
    plotDistribution(CRRA_l, DiscFac_l, y)
    
    # Return
    return CRRA_l, DiscFac_l, y

def plotDistribution(CRRA_l, DiscFac_l, yDist = []):
    if np.size(yDist) == 0:
        print('Parameter distribution:')
        f = plt.figure()
        plt.plot([mu_CRRA,mu_CRRA],[min_DiscFac-1, max_DiscFac+1], ls = '--', color = 'red', alpha = 0.6)
        plt.plot([min_CRRA-1,max_CRRA+1],[mu_DiscFac, mu_DiscFac], ls = '--', color = 'red', alpha = 0.6)
        plt.plot(CRRA_l, DiscFac_l, linestyle = '', marker = '.', color = '#4a69bd', alpha = 1)
        plt.xlabel(r'$\rho$')
        plt.ylabel(r'$\beta$', rotation = 0, labelpad = 10)
        plt.ylim([0.9, 1])
        plt.xlim([1, 5])
        plt.yticks(np.linspace(0.9,1,11))
        plt.show()
        f.savefig(Fpath+"CarrollDistrib.pdf", bbox_inches='tight')
    else:
        print('Joint distribution:')
        f = plt.figure()
        ax = f.gca(projection='3d')
        ax.zaxis.set_rotate_label(False) 
        ax.scatter(DiscFac_l, CRRA_l, yDist)
        ax.set_xlabel(r'$\beta$')
        ax.set_ylabel(r'$\rho$')
        ax.set_zlabel(r'$y$', labelpad = 2)
        ax.set_yticks([1,2,3,4,5])
        ax.set_xticks(np.linspace(0.9,1,6))
        f.savefig(Fpath+"CarrollDistrib3d.pdf", bbox_inches='tight')

def computeMPC(cFunc,y):
    h = np.linspace(0.0001,0.001,50)
    
    ddx_list = (cFunc(y+h) - cFunc(y))/h
        
    return np.mean(ddx_list)

def aggregateMPC(CRRAdist = [], BetaDist = [], yDist = [], params = []):    
    MPC_l = []

    if len(params) == 0:
        for i in range(numAgents):
            if (i+1) % 20 == 0:
                print("{:d}".format(i+1), end = '')
            elif (i+1) % 4 == 0:
                print(".", end = '')
            
            MPC_l.append(computeMPC(cFunc(CRRAdist[i],BetaDist[i]),yDist[i]))

        print('')
        return np.mean(MPC_l)    
    
    else:
        i = 0

        for (CRRA,DiscFac) in params:
            if (i+1) % 500 == 0:
                print("{:d}".format(i+1), end = '')
            elif (i+1) % 100 == 0:
                print(".", end = '')
                
            i += 1
            
            MPC_l.append(np.mean(computeMPC(cFunc(CRRA,DiscFac),yDist)))
        
        print('')
        return MPC_l

def graphsMPC(params, MPC_e_l, MPC):
    N = np.shape(params)[0]

    x = [params[i][1] for i in range(N)]
    y = [params[i][0] for i in range(N)]
    
    # MPC by parameters
    f = plt.figure()
    ax = f.gca(projection='3d')
    ax.zaxis.set_rotate_label(False) 
    ax.plot_trisurf(x,y, MPC*np.ones_like(x), alpha = 0.6, color = 'red')
    ax.plot_trisurf(x,y, MPC_e_l, linewidth = 0.2, antialiased = True, alpha = 1.0)
    ax.set_xlabel(r'$\overline{\beta}$', rotation = 0)
    ax.set_ylabel(r'$\overline{\rho}$', fontsize = 10, rotation = 0)
    ax.set_zlabel(r'$MPC(G)$', fontsize = 10, rotation = 0, labelpad = 12)
    ax.set_yticks([1,2,3,4,5])
    ax.set_xticks(np.linspace(0.9,1,6))
    f.savefig(Fpath+"CarrollMPC.pdf", bbox_inches='tight') 
    
    # Difference by parameters
    f = plt.figure()
    ax = f.gca(projection='3d')
    ax.zaxis._axinfo['label']['space_factor'] = 3
    ax.zaxis.set_rotate_label(False) 
    ax.plot_trisurf(x,y, MPC_e_l-MPC*np.ones_like(x), linewidth = 0.2, antialiased = True, alpha = 1.0)
    ax.set_xlabel(r'$\overline{\beta}$', rotation = 0)
    ax.set_ylabel(r'$\overline{\rho}$', fontsize = 10, rotation = 0)
    ax.set_zlabel(r'$D(\overline{\rho}, \overline{\beta})$', fontsize = 10, rotation = 0, labelpad = 10)
    ax.set_yticks([1,2,3,4,5])
    ax.set_xticks(np.linspace(0.9,1,6))
    f.savefig(Fpath+"CarrollDiff.pdf", bbox_inches='tight') 

def graphCurve(params, MPC_e_l, MPC, toleranceExp):
    N = np.shape(params)[0]
     
    # Compute difference
    diff = MPC_e_l - MPC * np.ones_like(MPC_e_l)
    
    # Create x and y for the graph
    y = []
    x = []
    
    # Keep only the pairs for wich diff " = 0 "
    for i in range(N):
        if abs(diff[i]) <= 10**(toleranceExp):
            y.append(params[i][1])
            x.append(params[i][0])
    
    print(len(x))
    print(x)
    
    # Plots
    f = plt.figure()
    plt.plot(x, y, color = 'black', linestyle = '', marker = '.' )
    plt.xlabel(r'$\overline{\rho}$')
    plt.ylabel(r'$\overline{\beta}$', rotation = 0, labelpad = 15)
    plt.ylim([0.9, 1])
    plt.xlim([1, 5])
    plt.show()
    f.savefig(Fpath+"CarrollMPCcurve_"+str(toleranceExp)+".pdf", bbox_inches='tight')    

if __name__ == "__main__":

### Plot Consumption function example
#    consExample(Fpath+"CarrollCons.pdf")       

#### Figures to show differences in beta and rho
#    plotCRRA(np.linspace(1,5,5))
#    plotDiscFac(np.linspace(0.89,0.99,5))
    
#### Generate distribution for both rho and beta
##     There are 200 people in the economy
#    CRRAdist, BetaDist, yDist = generateDistribution()
#
#### Big grid to show graphs
#
#    ### Paramters to compute MPC (in aggregation)
#    parameters = [(np.linspace(min_CRRA,max_CRRA,numPointsRho)[i],
#                   np.linspace(min_DiscFac,max_DiscFac,numPointsBeta)[j]) 
#                    for i in range(numPointsRho) for j in range(numPointsBeta)]
#
#    ### Compute REAL Aggregate MPC 
#    MPC = aggregateMPC(CRRAdist, BetaDist, yDist)
#    print("Done Real MPC: {:.3f}".format(MPC))
#    
#    ### Compute RA MPC for each choice of parameters
#    MPC_e_l = aggregateMPC(params = parameters, yDist = yDist)
#    print("Done RA MPC, big grid")
#        
#    ### Compare MPC and MPC_e
#    graphsMPC(parameters, MPC_e_l, MPC)
#
#### Finer grid to compute exact diff
#    parameters = [(np.linspace(min_CRRA,max_CRRA,numPointsRho)[i],
#                       np.linspace(auxMin_DiscFac,auxMax_DiscFac,numPointsBeta)[j]) 
#                        for i in range(numPointsRho) for j in range(numPointsBeta)]
#
#    ### Compute RA MPC for each choice of parameters
#    MPC_e_l = aggregateMPC(params = parameters, yDist = yDist)
#    print("Done RA MPC, finer grid")
#
#    ### Graph curve with diff = 0
    graphCurve(parameters, MPC_e_l, MPC, -4)
#    graphCurve(parameters, MPC_e_l, MPC, -5)