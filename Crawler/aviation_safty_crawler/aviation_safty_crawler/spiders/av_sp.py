import scrapy
from scrapy.spiders import *
from scrapy.linkextractors import *

years = list()


class AvSpSpider(CrawlSpider):
    name = 'av_sp'
    allowed_domains = ['aviation-safety.net']
    start_urls = ['https://aviation-safety.net/database/']

    rules = (
        Rule(LinkExtractor(allow='^https\\:\\/\\/aviation\\-safety\\.net\\/database\\/dblist\\.php\\?Year\\=',
                           restrict_css=('.innertube', 'p', 'a')),
             callback="parse_item",
             follow=False),)

    def parse_item(self, response):
        records = response.css('td.list a::attr(href)').extract()
        for x in records:
            x = 'https://aviation-safety.net' + str(x)
            yield scrapy.Request(x, callback=self.parse_detail_page)

    def parse_detail_page(self, response):
        s = response.css('td.desc').extract()
        print(s)
