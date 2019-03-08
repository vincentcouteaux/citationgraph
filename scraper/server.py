import os
os.environ['PATH'] += ':/home/couteaux/elm/scraper/'
print(os.environ['PATH'])
from selenium import webdriver
#from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.options import Options
import json
import re
from http.server import BaseHTTPRequestHandler,HTTPServer

PORT_NUMBER = 8080

options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)
driver.implicitly_wait(1)

def searchSS(keywords):
    url = "https://www.semanticscholar.org/search?q={}&sort=relevance".format(keywords)
    driver.get(url)

    a = driver.find_elements_by_class_name('search-result')
    #print([x.text for x in a])
    answers = []
    for x in a:
        answer = {}
        title = x.find_element_by_class_name('search-result-title')
        answer["title"] = title.text
        link = title.find_element_by_tag_name("a").get_attribute("href")
        answer["link"] = link
        try:
            answer["ssid"] = re.search("/([a-z,0-9]+)$", link).group(1)
        except:
            pass
        answer["authors"] = x.find_element_by_class_name('author-list').text
        answer["year"] = int(x.find_element_by_xpath(".//li[@data-selenium-selector='paper-year']").text)
        answers.append(answer)
    print(answers)
    return answers

class myHandler(BaseHTTPRequestHandler):
	
	#Handler for the GET requests
	def do_GET(self):
            self.send_response(200)
            self.send_header('Content-type','text/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            # Send the html message
            tosend = json.dumps(searchSS(self.path[1:]))
            self.wfile.write(tosend.encode("utf-8"))
            return

try:
	#Create a web server and define the handler to manage the
	#incoming request
	server = HTTPServer(('', PORT_NUMBER), myHandler)
	print('Started httpserver on port ' , PORT_NUMBER)
	
	#Wait forever for incoming htto requests
	server.serve_forever()

except KeyboardInterrupt:
        print('^C received, shutting down the web server')
        server.socket.close()
        driver.quit()
