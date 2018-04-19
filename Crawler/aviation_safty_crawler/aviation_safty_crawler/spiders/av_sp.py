# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import *
from scrapy.linkextractors import *

years = list()

class AvSpSpider(CrawlSpider):
    name = 'av_sp'
    allowed_domains = ['aviation-safety.net']
    start_urls = ['https://aviation-safety.net/database/']

    rules = (
        Rule(LinkExtractor(allow=('^https\\:\\/\\/aviation\\-safety\\.net\\/database\\/dblist\\.php\\?Year\\='), restrict_css=('.innertube','p','a')),
             callback="parse_item",
             follow=False),)

    def parse_item(self, response):
        years.append(response.url)
        print(len(years))
