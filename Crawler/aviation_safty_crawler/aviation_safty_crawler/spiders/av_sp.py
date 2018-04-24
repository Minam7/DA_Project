import scrapy
import csv
from scrapy.spiders import *
from scrapy.linkextractors import *


class AvSpSpider(CrawlSpider):
    name = 'av_sp'
    allowed_domains = ['aviation-safety.net']
    start_urls = ['https://aviation-safety.net/database/']

    with open('asn.csv', 'w') as myfile:
        wr = csv.writer(myfile)
        header = ['Date', 'Time', 'Type', 'Operator', 'Registration', 'C/n/msn', 'FirstFlight', 'TotalAirframeHrs',
                  'Cycles', 'Engines',
                  'Crew', 'Passengers', 'Total', 'AirplaneDamage', 'AirplaneFate', 'Location', 'Phase', 'Nature',
                  'DepartureAirport',
                  'FlightNumber', 'Narrative']
        wr.writerow(header)

    rules = (
        Rule(LinkExtractor(allow='^https\\:\\/\\/aviation\\-safety\\.net\\/database\\/dblist\\.php\\?Year\\=',
                           restrict_css=('.innertube', 'p', 'a')),
             callback="parse_item",
             follow=False),
    )

    def parse_item(self, response):
        records = response.css('td.list a::attr(href)').extract()
        other_pages = response.css('table + p + div.pagenumbers a::attr(href)').extract()
        for x in records:
            x = 'https://aviation-safety.net' + str(x)
            yield scrapy.Request(x, callback=self.parse_detail_page)
        for y in other_pages:
            y = 'https://aviation-safety.net/database/dblist.php' + str(y)
            yield scrapy.Request(y, callback=self.parse_item_other)

    def parse_item_other(self, response):
        records = response.css('td.list a::attr(href)').extract()
        for x in records:
            x = 'https://aviation-safety.net' + str(x)
            yield scrapy.Request(x, callback=self.parse_detail_page)

    def parse_detail_page(self, response):
        s = response.css(
            'td.desc::text, td.caption::text, td.desc a::text, span.caption::text, span.caption + br + span::text').extract()
        title = ['Date:', 'Time:', 'Type:', 'Operator:', 'Registration:', 'C/n / msn:', 'First flight:',
                 'Total airframe hrs:', 'Cycles:',
                 'Engines:', 'Crew:', 'Passengers:', 'Total:', 'Airplane damage:', 'Airplane fate:', 'Location:',
                 'Phase:', 'Nature:',
                 'Departure airport:', 'Flightnumber:', 'Narrative:']
        flag = False
        row = []
        ind = 0
        for p in s:
            if flag:
                if p in title:
                    flag = True
                    if ind == title.index(p):
                        ind = ind + 1
                    else:
                        for i in range(0, title.index(p) - ind):
                            row.append('NA')
                        ind = title.index(p) + 1
                else:
                    flag = False
                    row.append(p)
            else:
                if p in title:
                    flag = True
                    if ind == title.index(p):
                        ind = ind + 1
                    else:
                        for i in range(0, title.index(p) - ind):
                            row.append('NA')
                        ind = title.index(p) + 1

        with open('asn.csv', 'a+') as myfile:
            wr = csv.writer(myfile)
            wr.writerow(row)
