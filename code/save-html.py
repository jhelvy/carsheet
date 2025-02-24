# -*- coding: utf-8 -*-

import os
import sys
import time
from selenium import webdriver
from selenium.webdriver.chrome.service import Service

# Function for saving the html
def save_page(i):
    html = driver.page_source
    # Update path below based on where you want to save the raw html pages
    filePath = '/Users/jhelvy/Desktop/html/' + str(i) + '.html'
    with open(filePath, 'w') as f:
        f.write(str(html.encode('UTF-8')))

# Open Chrome to carsheet.io
chromedriverPath = '/Users/jhelvy/Documents/chromedriver-mac-x64/chromedriver'
service = Service(executable_path=chromedriverPath)
driver = webdriver.Chrome(service=service)
driver.get('https://carsheet.io/')

# Manual Adjustments:

# 1) Select all data columns
# 2) Set rows to 100

# Now save the first page

save_page(1)

# Now loop through each page and get and save the page source html

page_path = '//*[@id="carsheet_next"]/a'
for i in range(1, 654):
    # Click the next button using the updated method
    driver.find_element("xpath", page_path).click()
    time.sleep(10)  # Let the data load
    save_page(i + 1)
