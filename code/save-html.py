# -*- coding: utf-8 -*-

import os
import sys
import time
from selenium import webdriver

# Set the working directory

os.chdir('/Users/jhelvy/gh/staging/carsheet/')
# print(os.getcwd())

# Open Chrome to carsheet.io

chromedriverPath = '/Users/jhelvy/gh/staging/carsheet/code/chromedriver'
driver = webdriver.Chrome(chromedriverPath)
driver.get('https://carsheet.io/')

# Manually select the columns of interest

# Now get and save the page source html

driver.get('https://carsheet.io/all-cars/1-100/')

filePath = 'html/1.html'
with open(filePath, 'w') as f:
    f.write(driver.page_source)
