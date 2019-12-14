class Thread_base:
	def __init__(self, href):
		self.href = href
		self.title = ''
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
		"title" : self.title,
		"posts" : dict_posts
		}
		return dict_thread
	def add_to_thread(self, soup):
		pass
	class Post:
		def __init__(self, content = [], date = "", login = ""):
			self.content = content
			self.date = date
			self.login = login
