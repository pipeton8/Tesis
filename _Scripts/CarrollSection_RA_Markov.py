#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 15 16:53:17 2019

@author: pipeton8
"""
from copy import deepcopy
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt

from HARK.ConsumptionSaving.ConsRepAgentModel import RepAgentMarkovConsumerType
import HARK.ConsumptionSaving.ConsumerParameters as Params

# Plot Parameters
mpl.rcParams['axes.spines.right'] = False
mpl.rcParams['axes.spines.top'] = False
plt.rc('text', usetex=True)

# Figure path
Fpath = "../Documento Final/Graphics/"

# Parameters
RA_markov_params = deepcopy(Params.init_idiosyncratic_shocks)
RA_markov_params['CRRA'] = 3.0
RA_markov_params['DiscFac'] = 0.99
RA_markov_params['DeprFac'] = 0.025
RA_markov_params['CapShare'] = 0.36
RA_markov_params['UnempPrb'] = 0.0
RA_markov_params['LivPrb'] = [1.0]
RA_markov_params['PermGroFac'] = [[1.01, 0.99]]
RA_markov_params['MrkvArray'] = np.array([[0.125,0.875],[0.875, 0.125]])
RA_markov_params['MrkvNow'] = 0

# Create and solve
RA_Agg_model = RepAgentMarkovConsumerType(**RA_markov_params)
RA_Agg_model.IncomeDstn[0] = 2*[RA_Agg_model.IncomeDstn[0]]
RA_Agg_model.solve() 

# Consumption function
cFunc2 = RA_Agg_model.solution[0].cFunc[0]

# Plots
print('Consumption function:')
m_grid = np.linspace(0,10,200)
c_m_2 = cFunc2(m_grid)
plt.plot(m_grid,c_m_2)
plt.show()

MPC = (cFunc2(43.964) - cFunc2(43.963))/0.001