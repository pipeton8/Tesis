#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 14:51:36 2019

@author: pipeton8
"""

import os
os.environ["PATH"] += os.pathsep + '/Library/TeX/texbin'

print(os.environ["PATH"])