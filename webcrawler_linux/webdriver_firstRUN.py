# Libraries 
import pandas as pd
import numpy as np
import time
import re
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.keys import Keys
import multiprocessing as mp

def date_char2num(char):
    '''
    input: Thu May 31, 2018
    output: 05312018
    '''
    
    split_date = re.split(r", | ", char)
    return(split_date[1]+split_date[2]+'_'+split_date[3])

def labelinsight_login(url, headless = False, showpics = True):
    '''
    url: url of login page
    headless: True means run webdriver in background
    showpics: True means show pictures
    '''
    
    # instantiate a chrome options object so you can set the size and headless preference
    Firefox_options = Options()
    if headless == True:
        Firefox_options.add_argument("--headless")
        Firefox_options.add_argument("--window-size=1920x1080")
        
    
    firefox_profile = webdriver.FirefoxProfile()
    if showpics == False:
        firefox_profile.set_preference('permissions.default.stylesheet', 2)
        firefox_profile.set_preference('permissions.default.image', 2)
        firefox_profile.set_preference('dom.ipc.plugins.enabled.libflashplayer.so', 'false')

    driver=webdriver.Firefox(firefox_options=Firefox_options,
                             executable_path=r'./geckodriver',
                             firefox_profile= firefox_profile)
    driver.get(url)

    try:
        driver.find_element_by_xpath('//*[@id="userEmail"]').send_keys(email)
        driver.find_element_by_name('userPassword').send_keys(password)
        
        driver.find_element_by_css_selector('.Button-dYVdnu').click()
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, '.sc-iAyFgw > div:nth-child(3) > li:nth-child(1) > a:nth-child(1)')))
    except: 
        print("Error \nDriver will be closed in 3 sec")
        time.sleep(3)
#         driver.quit()
        print("Driver closed")
    return(driver)

def get_maininfo(driver):
    '''
    driver: logined  driver and on targeted item page
    '''

    item_info = []

    # title
    element = driver.find_element_by_css_selector('.no-top-margin')
    item_info.append(element.text)
    # Brand
    element = driver.find_element_by_css_selector('.flexgrid-fixed > div:nth-child(1) > p:nth-child(2)')
    item_info.append(element.text)
    # Sub Brand
    element = driver.find_element_by_css_selector('div.border-bottom:nth-child(2) > p:nth-child(2)')
    item_info.append(element.text)
    # Manufacturer
    element = driver.find_element_by_css_selector('div.border-bottom:nth-child(3) > p:nth-child(2)')
    item_info.append(element.text)
    # Last Collected
    element = driver.find_element_by_css_selector('.badge')
    item_info.append(element.text)
    # Collected on
    element = driver.find_element_by_css_selector('div.border-bottom:nth-child(5) > p:nth-child(2)')
    CollectedDate = date_char2num(element.text)
    item_info.append(element.text)
    # Collected by
    element = driver.find_element_by_css_selector('div.border-bottom:nth-child(6) > p:nth-child(2)')
    item_info.append(element.text)

    ###########################detail
    # Aisle
    element = driver.find_element_by_css_selector('div.shaded-light:nth-child(3) > div:nth-child(1) > div:nth-child(1) > p:nth-child(2)')
    item_info.append(element.text)
    # Shelf
    element = driver.find_element_by_css_selector('div.shaded-light:nth-child(3) > div:nth-child(1) > div:nth-child(2) > p:nth-child(2)')
    item_info.append(element.text)
    # Category
    element = driver.find_element_by_css_selector('div.shaded-light:nth-child(3) > div:nth-child(1) > div:nth-child(3) > p:nth-child(2)')
    item_info.append(element.text)
    
    # UPC 
    element = driver.find_element_by_css_selector('#product-header_detailsmorehidden > div:nth-child(1) > p:nth-child(2)')
    UPC = element.text
    item_info.append(element.text)
    # LabelInsight Id
    element = driver.find_element_by_css_selector('#product-header_detailsmorehidden > div:nth-child(2) > p:nth-child(2)')
    item_info.append(element.text)
    # Size
    element = driver.find_element_by_css_selector('#product-header_detailsmorehidden > div:nth-child(3) > p:nth-child(2)')
    item_info.append(element.text)
    # Serving Size 1
    element = driver.find_element_by_css_selector('div.text-center:nth-child(4) > p:nth-child(2)')
    item_info.append(element.text)
    # Serving Description
    element = driver.find_element_by_css_selector('div.text-center:nth-child(5) > p:nth-child(2)')
    item_info.append(element.text)
    # Servings Per Container
    element = driver.find_element_by_css_selector('div.text-center:nth-child(6) > p:nth-child(2)')
    item_info.append(element.text)
    # Use RACC
    element = driver.find_element_by_xpath('/html/body/div/div/div/div/main/content/div/div[2]/div[2]/div[2]/div[1]/div[4]/p[2]')
    item_info.append(element.text)
    # RACC
    element = driver.find_element_by_xpath('/html/body/div/div/div/div/main/content/div/div[2]/div[2]/div[2]/div[1]/div[5]/p[2]')
    item_info.append(element.text)
    # Effective RACC
    element = driver.find_element_by_xpath('/html/body/div/div/div/div/main/content/div/div[2]/div[2]/div[2]/div[1]/div[6]/p[2]')
    item_info.append(element.text)
    
    main_info = pd.DataFrame(columns=['title','Brand','Sub Brand',
                                   'Manufacturer','Last Collected',
                                   'Collected on','Collected by',
                                   'Aisle','Shelf','Category','UPC',
                                   'LabelInsight Id','Size',
                                   'Serving Size 1',
                                   'Serving Description',
                                   'Servings Per Container',
                                   'Use RACC','RACC','Effective RACC'])
    main_info.loc[0] = item_info
    filename = './output/' + UPC + '_' + CollectedDate + '_maininfo.csv'
    
    main_info.to_csv(filename)
    
    return(UPC, CollectedDate)

def get_table(driver, css_sel_table):
    # driver.find_element_by_css_selector('button.btn:nth-child(3)').click()
    # WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, '#product-header_detailsmorehidden > div:nth-child(1) > p:nth-child(2)')))
    
    table_id = driver.find_element_by_css_selector(css_sel_table)
    rows = table_id.find_elements(By.TAG_NAME, "tr") # get all of the rows in the table
    # time.sleep(8)
    # print("locate table, begin to grap table")
    # print(("total number of rows is ", len(rows)))
    for ind, row in enumerate(rows):
        # print(ind)
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.TAG_NAME, "th")))
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.TAG_NAME, "td")))

        # time.sleep(2)
        col_name = []
        if ind == 0:
            cols = row.find_elements(By.TAG_NAME, "th") #note: index start from 0, 1 is col 2
            for col in cols:
                col_name.append(col.text)
            table_panel = pd.DataFrame(columns=col_name)
        else:
            line_value = []
            # Get the columns (all the column 2)
            cols = row.find_elements(By.TAG_NAME, "td") #note: index start from 0, 1 is col 2
            for col in cols:
                if col.text == '':
                    try:
                        image = col.find_element_by_tag_name("img")
                        if image.get_attribute("src") == 'https://app.labelinsight.com/explore/assets/images/icon-check.png':
                             line_value.append('True')
                        elif image.get_attribute("src") == 'https://app.labelinsight.com/explore/assets/images/icon-x.png':
                             line_value.append('False')
                        else:
                            line_value.append(col.text)
                    except:
                        line_value.append(col.text)
                else:
                    line_value.append(col.text) #prints text from the element
            # print(line_value)
            table_panel.loc[ind-1] = line_value
    return(table_panel)

def get_all_table(driver,UPC,CollectedDate):
    '''
    get all tables for a single item

    driver: logined driver and already turned to target item page
    '''
    # driver = labelinsight_login(True)
    # driver.find_element_by_css_selector(css_selector).click()
    try:
        # Nutrition Facts Panel
        table_tmp = get_table(driver, '.nfp-table-container')
        filename = './output/' + UPC + '_' + CollectedDate + '_NutritionFactsPanel.csv'
        table_tmp.to_csv(filename)
        
        # FDA Nutrient Content Attributes
        table_tmp = get_table(driver, '#fda_nutrient_content_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_FDANutrientContentAttributes.csv'
        table_tmp.to_csv(filename)

        # Nutrition Facts Attributes
        table_tmp = get_table(driver, '#nutrition_facts_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_NutritionFactsAttributes.csv'
        table_tmp.to_csv(filename)

        # Certification & Affiliation Attributes
        table_tmp = get_table(driver, '#certification_\&_affiliation_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_CertificationAffiliationAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Marketing Claim Attributes
        table_tmp = get_table(driver, '#marketing_claim_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_MarketingClaimAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Ingredient Analysis Attributes
        table_tmp = get_table(driver, '#ingredient_analysis_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_IngredientAnalysisAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Raw Ingredient Attributes
        table_tmp = get_table(driver, '#raw_ingredient_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_RawIngredientAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Amazon Nutrition Facts Attributes
        table_tmp = get_table(driver, '#amazon_nutrition_facts_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_RawIngredientAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Amazon Specialty Attributes
        table_tmp = get_table(driver, '#amazon_specialty_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_AmazonSpecialtyAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Dierbergs Whole Life Attributes
        table_tmp = get_table(driver, '#dierbergs_whole_life_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_DierbergsWholeLifeAttributes.csv'
        table_tmp.to_csv(filename)
        
        # # Hy-Vee Attributes
        # table_tmp = get_table(driver, '#hy-vee_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        # table_tmp.to_excel(writer,'Hy-Vee Attributes')
        
        # # Kroger VIP Attributes
        # table_tmp = get_table(driver, '#kroger_vip_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        # table_tmp.to_excel(writer,'Kroger VIP Attributes')
        
        # # Trader Joe's Dietary Guides & Lists Attributes
        # table_tmp = get_table(driver, 'table.attributes_table:nth-child(2)')
        # table_tmp.to_excel(writer,"Trader Joe's Dietary Guides & Lists Attributes")
        
        # # Wegman's Wellness Keys Attributes
        # table_tmp = get_table(driver, '#wegman\'s_wellness_keys_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        # table_tmp.to_excel(writer,"Wegman's Wellness Keys Attributes")
  
    except:    
        # Certification & Affiliation Attributes
        table_tmp = get_table(driver, '#certification_\&_affiliation_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_CertificationAffiliationAttributes.csv'
        table_tmp.to_csv(filename)
        
        # Marketing Claim Attributes
        table_tmp = get_table(driver, '#marketing_claim_attributes > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1)')
        filename = './output/' + UPC + '_' + CollectedDate + '_MarketingClaimAttributes.csv'
        table_tmp.to_csv(filename)

def get_maininfo_all(url_pool, driver):
    '''
    url_pool: url of all items, list
    driver: logined driver
    '''
    for url in url_pool:
        driver.get(url)
        #     WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CSS_SELECTOR, 'button.btn:nth-child(3)')))
        driver.find_element_by_css_selector('button.btn:nth-child(3)').click()
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, '#product-header_detailsmorehidden > div:nth-child(1) > p:nth-child(2)')))
        UPC, CollectedDate = get_maininfo(driver)
        get_all_table(driver,UPC,CollectedDate)


def run1():
    ### login url and email and password
    url = 'https://app.labelinsight.com/login'
    global email
    global password
    email = 'kristina.brecko@simon.rochester.edu'
    password = '9xQWPnXdEkWCDR'

    ### read Food Essentials ID
    FoodEssentials = pd.read_csv('FoodEssentialsID.csv')

    ### create url pool
    url_pool = ['https://app.labelinsight.com/explore/product/id/%s'%i for i in FoodEssentials['FoodEssentials ID']]
    #### 338120 records

    time_start=time.time()
    # initial webdriver
    driver = labelinsight_login(url, headless = False, showpics = True)
        
    # initial selected item url pool
    sel_urls = url_pool[1:3]

    item_infos = get_maininfo_all(sel_urls, driver)
    driver.quit()

    time_end = time.time()

    print('time cost:',time_end-time_start,'s')

def run2():
    ### login url and email and password
    url = 'https://app.labelinsight.com/login'
    global email
    global password
    email = 'kristina.brecko@simon.rochester.edu'
    password = '9xQWPnXdEkWCDR'

    ### read Food Essentials ID
    FoodEssentials = pd.read_csv('FoodEssentialsID.csv')

    ### create url pool
    url_pool = ['https://app.labelinsight.com/explore/product/id/%s'%i for i in FoodEssentials['FoodEssentials ID']]
    #### 338120 records

    time_start=time.time()
    # initial webdriver
    driver = labelinsight_login(url, headless = True, showpics = True)
        
    # initial selected item url pool
    sel_urls = url_pool[4:6]

    item_infos = get_maininfo_all(sel_urls, driver)
    driver.quit()

    time_end = time.time()

    print('time cost:',time_end-time_start,'s')

if __name__ == '__main__':
    p1 = mp.Process(target=run1)
    p2 = mp.Process(target=run2)
    p1.start()
    p2.start()
    p1.start()
    p2.start()

    print("The number of CPU is:" + str(multiprocessing.cpu_count()))
    for p in multiprocessing.active_children():
        print("child   p.name:" + p.name + "\tp.id" + str(p.pid))
    print("END!!!!!!!!!!!!!!!!!")

