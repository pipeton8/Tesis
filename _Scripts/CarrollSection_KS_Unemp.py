#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 15 16:53:17 2019

@author: pipeton8
"""

import matplotlib.pyplot as plt
import numpy as np
from HARK.ConsumptionSaving.ConsAggShockModel import AggShockMarkovConsumerType
from HARK.ConsumptionSaving.ConsAggShockModel import CobbDouglasMarkovEconomy

### General Dictionaries

# Agent Dictionary
def consExample(filename):
    KSAgentDictionary = { 
        "CRRA"          : 3.0,                  # Coefficient of relative risk aversion
        "DiscFac"       : 0.99,                 # Intertemporal discount factor
        "LivPrb"        : [1.0],                # Survival probability
        "AgentCount"    : 1000,                 # Number of agents of this type (only matters for simulation)
        "aNrmInitMean"  : 0.0,                  # Mean of log initial assets (only matters for simulation)
        "aNrmInitStd"   : 0.0,                  # Standard deviation of log initial assets (only for simulation)
        "pLvlInitMean"  : 0.0,                  # Mean of log initial permanent income (only matters for simulation)
        "pLvlInitStd"   : 0.0,                  # Standard deviation of log initial permanent income (only matters for simulation)
        "PermGroFacAgg" : 1.0,                  # Aggregate permanent income growth factor (only matters for simulation)
        "T_age"         : None,                 # Age after which simulated agents are automatically killed
        "T_cycle"       : 1,                    # Number of periods in the cycle for this agent type
    #
    # Parameters for constructing the "assets above minimum" grid
        "aXtraMin"      : 0.001,                # Minimum end-of-period "assets above minimum" value
        "aXtraMax"      : 20,                   # Maximum end-of-period "assets above minimum" value               
        "aXtraExtra"    : [None],               # Some other value of "assets above minimum" to add to the grid
        "aXtraNestFac"  : 3,                    # Exponential nesting factor when constructing "assets above minimum" grid
        "aXtraCount"    : 24,                   # Number of points in the grid of "assets above minimum"
    #
    # Parameters describing the income process
        "PermShkCount"  : 1,                    # Number of points in discrete approximation to permanent income shocks - no shocks of this kind!
        "TranShkCount"  : 1,                    # Number of points in discrete approximation to transitory income shocks - no shocks of this kind!
        "PermShkStd"    : [0.],                 # Standard deviation of log permanent income shocks - no shocks of this kind!
        "TranShkStd"    : [0.],                 # Standard deviation of log transitory income shocks - no shocks of this kind!
        "UnempPrb"      : 0.0,                  # Probability of unemployment while working - no shocks of this kind!
        "UnempPrbRet"   : 0.0,                  # Probability of "unemployment" while retired - no shocks of this kind!
        "IncUnemp"      : 0.0,                  # Unemployment benefits replacement rate
        "IncUnempRet"   : 0.0,                  # "Unemployment" benefits when retired
        "tax_rate"      : 0.0,                  # Flat income tax rate
        "T_retire"      : 0,                    # Period of retirement (0 --> no retirement)
        "BoroCnstArt"   : 0.0,                  # Artificial borrowing constraint; imposed minimum level of end-of period assets   
        "cycles"        : 0,                    # Consumer is infinitely lived
        "PermGroFac"    : [1.0],                # Permanent income growth factor
    # 
    # New Parameters that we need now    
        'MgridBase': np.array([0.1, 0.3,  0.6,
                               0.8, 0.9,  0.98,
                               1.0, 1.02, 1.1,
                               1.2, 1.6,  2.0,
                               3.0]),           # Grid of capital-to-labor-ratios (factors)
        'MrkvArray': np.array([[0.875, 0.125],
                               [0.125, 0.875]]),# Transition probabilities for macroecon. [i,j] is probability of being in state j next
                                                # period conditional on being in state i this period. 
        'PermShkAggStd' : [0.0, 0.0],           # Standard deviation of log aggregate permanent shocks by state. No continous shocks in a state.
        'TranShkAggStd' : [0.0, 0.0],           # Standard deviation of log aggregate transitory shocks by state. No continuous shocks in a state.
        'PermGroFacAgg' : 1.0
    }
    
    # Economy Dictionary
    KSEconomyDictionary = {
        'PermShkAggCount': 1, 
        'TranShkAggCount': 1, 
        'PermShkAggStd'  : [0.0, 0.0], 
        'TranShkAggStd'  : [0.0, 0.0], 
        'DeprFac'        : 0.025,       # Depreciation factor
        'CapShare'       : 0.36,        # Share of capital income in cobb-douglas production function
        'DiscFac'        : 0.99,
        'CRRA'           : 3.0,
        'PermGroFacAgg'  : [1.0, 1.0],
        'AggregateL'     : 1.0,         # Fix aggregate labor supply at 1.0 - makes interpretation of z easier
        'act_T'          : 1200,        # Number of periods for economy to run in simulation
        'intercept_prev' : [0.0, 0.0],  # Make some initial guesses at linear savings rule intercepts for each state
        'slope_prev'     : [1.0, 1.0],  # Make some initial guesses at linear savings rule slopes for each state
        'MrkvArray'      : np.array([ [0.875, 0.125],
                                      [0.125, 0.875]]), # Transition probabilities
        'MrkvNow_init'   : 0   # Pick a state to start in (we pick the first state)
    }
    
    ### Representative Agent
    
    # Create the Krusell-Smith agent as an instance of AggShockMarkovConsumerType 
    KSAgent = AggShockMarkovConsumerType(**KSAgentDictionary)
    
    # Construct the income distribution for the Krusell-Smith agent
    prb_eg = 0.96         # Probability of   employment in the good state
    prb_ug = 1 - prb_eg   # Probability of unemployment in the good state
    prb_eb = 0.90         # Probability of   employment in the bad state
    prb_ub = 1 - prb_eb   # Probability of unemployment in the bad state
    p_ind  = 1            # Persistent component of income is always 1
    ell_ug = ell_ub = 0   # Labor supply is zero for unemployed consumers in either agg state
    ell_eg = 1.0/prb_eg   # Labor supply for employed consumer in good state
    ell_eb = 1.0/prb_eb   # 1=pe_g*ell_ge+pu_b*ell_gu=pe_b*ell_be+pu_b*ell_gu
    
    # IncomeDstn is a list of lists, one for each aggregate Markov state
    # Each contains three arrays of floats, representing a discrete approximation to the income process. 
    # Order: 
    #   state probabilities 
    #   idiosyncratic persistent income level by state (KS have no persistent shocks p_ind is always 1.0)
    #   idiosyncratic transitory income level by state
    
    KSAgent.IncomeDstn[0] = [ 
        [np.array([prb_eg,prb_ug]), np.array([p_ind,p_ind]), np.array([ell_eg,ell_ug])], # Agg state good
        [np.array([prb_eb,prb_ub]), np.array([p_ind,p_ind]), np.array([ell_eb,ell_ub])]  # Agg state bad
    ]
    
    KSEconomy = CobbDouglasMarkovEconomy(agents = [KSAgent], **KSEconomyDictionary) # Combine production and consumption sides into an "Economy"
    
    # Calibrate the magnitude of the aggregate shocks
    Tran_g = 1.01 # Productivity z in the good aggregate state
    Tran_b = 0.99 # and the bad state
    
    # Aggregate productivity shock distribution by state.
    # First element is probabilities of different outcomes, given the state you are in. 
    # Second element is agg permanent shocks (here we don't have any, so just they are just 1.).
    # Third  element is agg transitory shocks, which are calibrated the same as in Krusell Smith.
    
    KSAggShkDstn = [
        [np.array([1.0]),np.array([1.0]),np.array([Tran_g])], # Aggregate good
        [np.array([1.0]),np.array([1.0]),np.array([Tran_b])]  # Aggregate bad
    ]
    
    KSEconomy.AggShkDstn = KSAggShkDstn
    
    # Construct the economy, make an initial history, then solve 
    KSAgent.getEconomyData(KSEconomy)   # Makes attributes of the economy, attributes of the agent
    KSEconomy.makeAggShkHist()          # Make a simulated history of the economy
    
    # Tolerance level 
    KSEconomy.tolerance = 0.01
    
    # Solve the economy using the market method. 
    KSEconomy.solve() 
    
### Plots
    print('Consumption function at each aggregate market resources gridpoint (in general equilibrium):')
    m_grid = np.linspace(0,20,200)
    
    c_m = KSAgent.solution[0].cFunc[0](m_grid,10.0*np.ones_like(m_grid))
    f = plt.figure()
    
    plt.plot(m_grid,c_m, color = '#4a69bd', linewidth = 2.5)

    plt.ylabel(r'$c(y)$', labelpad = 15, rotation = 0)
    plt.ylim([-0.15, 2.15])
    plt.yticks([0 + 0.25 * k for k in range(9)], [r'${:.1f}$'.format(0 + 0.25 * k) if k % 2 == 0 else '' for k in range(9)])

    plt.xlabel(r'$y$')
    plt.xlim([-1, 21])
    plt.xticks([0 + 2.5 * k for k in range(9)], [r'${:.0f}$'.format(0 + 2.5 * k) if k % 2 == 0 else '' for k in range(9)])
    plt.show()
    f.savefig(filename, bbox_inches='tight')
