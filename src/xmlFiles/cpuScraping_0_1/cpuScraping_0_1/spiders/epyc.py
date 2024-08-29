#requires edit of "{insert python path here}/site-packages/tldextract/cache.py"
#change any and all references of md5 to become sha256 for it to work

from pathlib import Path

import scrapy


class EpicSpider(scrapy.Spider):
	name = "epyc"

	def start_requests(self):
		urls = [
			"https://en.wikichip.org/wiki/amd/epyc",
		]
		for url in urls:
			yield scrapy.Request(url=url, callback=self.parse)

	def parse(self, response):
		page = response.url.split("/")[-1]
		filename = f"quotes-{page}.html"
		Path(filename).write_bytes(response.body)
		self.log(f"Saved file {filename}")

