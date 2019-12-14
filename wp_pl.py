from bs4 import BeautifulSoup
import requests
import json
from selenium import webdriver 
from selenium.webdriver.common.by import By 
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait 
from selenium.webdriver.support import expected_conditions as EC 
from selenium.common.exceptions import TimeoutException
from selenium.common.exceptions import NoSuchElementException
import time

#przygotowanie przeglądarki
driver = webdriver.Firefox()
driver.implicitly_wait(10)
driver.get("https://parenting.pl/szukaj?q=szczepienia&type=forum")
submit = driver.find_elements_by_xpath("//*[contains(text(), 'SERWISU')]")[1]
submit.click()
#przygotowanie słownika
data = { "wp.pl" : { 
				"threads" : []
			}
		}
#klasa na dane
class Thread:
	def __init__(self, href):
		self.href = href
		self.posts = []
	def add_post(self, content, date, login):
		self.posts.append(self.Post(content, date, login))
	def return_dict(self):
		dict_posts = []
		for post in self.posts:
			dict_post = { 
			"content" : post.content,
			"date" : post.date,
			"login" : post.login
			}
			dict_posts.append(dict_post)
		dict_thread = {
		"href" : self.href,
		"posts" : dict_posts
		}
		return dict_thread

	class Post:
		def __init__(self, content = [], date = "", login = ""):
			self.content = content
			self.date = date
			self.login = login

def add_to_thread(html_soup, thread):
	articles_all = html_soup.find('div', class_ = 'forum-thread')
	articles = articles_all.find_all('article')
	for art in articles:
		p_list = art.findChildren('p', recursive = False)
		p_text = []
		for p in p_list:
			p_text.append(p.text)
		content = '\\n'.join(p_text)
		date = art.find("time")['datetime']
		login = art.find("img")['alt']
		thread.add_post(content, date, login)

r = requests.get("https://parenting.pl/szukaj?q=szczepienia&type=forum")
html_soup = BeautifulSoup(r.text, 'html.parser')
try:
	for i in range(2,16):
		print('strona: ' + str(i-1))
		results_elements = html_soup.find_all('article' ,class_ = 'results__unit')
		results_elements[:] = [x for x in results_elements if len(x.find_all('li', class_ = 'results__info__item')) > 1]
		links = []
		for result in results_elements:
			links.append(result.a['href'])
		threads = []
		for thread in links:
			threads.append(Thread(thread))
		for thread in threads:
			r = requests.get(thread.href)
			html_soup = BeautifulSoup(r.text, 'html.parser')
			add_to_thread(html_soup, thread)
			if html_soup.find('li', class_ = 'pg_next') != None:
	#przewijanie listy komentarzy	
				print('wszedłem do pętli')
				driver.get(thread.href)
				while(1):
					try:
						driver.find_element_by_xpath("//li[@class = 'pg_next']").click()
						html_soup = BeautifulSoup(driver.page_source, 'html.parser')
						add_to_thread(html_soup, thread)
					except NoSuchElementException:
						print('jestem w except')
						break
			data["wp.pl"]["threads"].append(thread.return_dict())
		with open('wp_parenting_hybrid.txt', 'w+') as f:
			json.dump(data, f)
		r = requests.get('https://parenting.pl/szukaj?q=szczepienia&type=forum&page='+str(i))
		html_soup = BeautifulSoup(r.text, 'html.parser')
finally:
	with open('wp_parenting_hybrid.txt', 'w+') as f:
	json.dump(data, f)
	driver.close()



		

 