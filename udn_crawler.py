# -*- coding: utf-8 -*-
from requests import session
from bs4 import BeautifulSoup
import threading

import csv

import os
import time

import html5lib
import random

SEED_LOGIN_URL = "http://udndata.com.tw/ndapp/Login"
SEED_CRAWLER_URL = "http://udndata.com.tw/ndapp/SearchCategory?cateID="
ROOT_URL =  "http://udndata.com"
BLOCK_SIZE = 2048
URL_KEY_WORD = "Searchdec2007"
AUTH_DATA = { "pUserID":"ccudn",
			  "pPassword":"news1"}

try:
	WEB_COOKIE = session()
	WEB_COOKIE.post(SEED_LOGIN_URL, data=AUTH_DATA)
except:
	print ("login failed!")
	exit(0)

URL_STACK_LOCK = threading.Lock()
SAVE_FILE_LOCK = threading.Lock()

url_stack = []
visited_stack = []
valid_query_id = []

def open_csv(csv_name):
	csv_file = open(csv_name,'wb+')
	return csv_file

def close_csv(csv_file):
	csv_file.close()

def insert_into_csv(csv_file, data):
	csv_writer = csv.writer(csv_file)
	csv_writer.writerows(data)

def insert_into_db(data):
	query = """INSERT INTO `news_table` (news_class, news_title, news_bref_content, news_info) VALUES (%s, %s, %s, %s ) """	
	DB_CURSOR.executemany(query, data)
	DB_CONN.commit()

def get_valid_id():
	for x in xrange(3000,154):
		response = WEB_COOKIE.get(SEED_CRAWLER_URL + str(x))
		soup = BeautifulSoup(response.text)
		if web_is_target(soup):
			print (x, "is valid")
			valid_query_id.append(x)
	valid_id_file = open("valid_query_id.txt","w+")
	valid_id_file.write(str(valid_query_id))


def generate_init_url():
	url_stack.append(SEED_CRAWLER_URL + str(190))
	visited_stack.append(SEED_CRAWLER_URL + str(190))


def fetch_url(soup):
	for tag in soup.find_all("a", href=True):
		sub_url = tag.get('href')
		find_key = sub_url.find(URL_KEY_WORD)
		if not find_key > 0:
			continue

		new_url = ROOT_URL + sub_url
	
		if (ROOT_URL in new_url) and (new_url not in visited_stack):
			url_stack.append(new_url)
			visited_stack.append(new_url)

def get_url():
	if len(url_stack) > 0:
		with URL_STACK_LOCK:
			url = url_stack[-1]
			url_stack.pop(-1)
			return url

	return False


def init_crawler():
	while len(url_stack) < 100:
		download_html()

def web_is_target(soup):
	counter = 0 
	font_set = soup.find_all("font", {"color":"#FF6600"})
	if len(font_set) >= 4:
		return True
	return False

def download_html():
	target_url = get_url()

	if not target_url:
		print ("get url error", len(url_stack))
		return False
	try:
		response = WEB_COOKIE.get(target_url)
	except:
		print ("url broken!")
		return False

	soup = BeautifulSoup(response.text, "html5lib")
	if not web_is_target(soup):
		return False


	fetch_url(soup)

	return soup

def saveFile(data):
	for row in data:
		fileDir = "newsData/" + "TSMC"
		if not os.path.exists(fileDir):
				os.makedirs(fileDir)

		des = fileDir + "/" + str(random.randint(1, 1000000)) + ".txt"

		try:
			fileHandle = open(des, "w+")
			fileHandle.write(row[3]+'\n')
			fileHandle.write(row[2])
			fileHandle.close()
		except:
			print (des," failed to save file")
			pass


def crawler(thread_id):
	print (thread_id," started")

	name_counter = 0

	row_counter = 0
	
	while True :

		html_tree = download_html()
		if not html_tree:
			if not len(url_stack) > 0:
				break
			continue

		for e in html_tree.findAll('br'):
			 e.extract()

		data_row = []
		font_set = html_tree.find_all("font", {"color":"#FF6600"})


		news_class = font_set[3].contents[0]

	
		for a_tag in html_tree.find_all("a",{"class":"t"}):
			try:
				news_title = a_tag.contents[0]

				content_anchor = a_tag.parent.parent.parent
				story_contents = content_anchor.find_all("span")

				bref_content = story_contents[0].contents[0]
				news_info = story_contents[1].contents[0]

				data_row.append(
					(news_class, news_title, bref_content, news_info)
				)
			except:
				pass

		try:
			#print(data_row)
			with SAVE_FILE_LOCK:
				saveFile(data_row)
		except:
			pass

			


def main():

	if not os.path.exists("newsData"):
		os.makedirs("newsData")

	
 #   	# while True:
	generate_init_url()

	NumOfThread = 5  #number of thread

	thread_pool = []

	for x in range(1,NumOfThread):
		t = threading.Thread(target= crawler, args= [x])
		thread_pool.append(t)
		t.start()

if __name__ == '__main__':
	main()