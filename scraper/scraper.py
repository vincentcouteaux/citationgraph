import os
os.environ['PATH'] += ':/home/couteaux/elm/scraper/'
print(os.environ['PATH'])
from selenium import webdriver
#from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.options import Options


#url = "https://www.semanticscholar.org/search?q=adversarial%20autoencoders&sort=relevance"

options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)
driver.implicitly_wait(1)

def searchSS(keywords):
    url = "https://www.semanticscholar.org/search?q={}&sort=relevance".format(keywords)
    driver.get(url)

    a = driver.find_elements_by_class_name('search-result')
    #print([x.text for x in a])
    for x in a:
        title = x.find_element_by_class_name('search-result-title')
        print(title.text)
        link = title.find_element_by_tag_name("a").get_attribute("href")
        print(link)
searchSS("adversarial%20autoencoders")
searchSS("GAN")
driver.quit()
