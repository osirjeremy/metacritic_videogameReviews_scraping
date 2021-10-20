from scrapy import Spider, Request
from videogames.items import VideogamesItem
import re

class MetacriticSpider(Spider):
	name = "metacritic_spider"
	allowed_urls = ['www.metacritic.com']
	start_urls = ['https://www.metacritic.com/browse/games/score/metascore/all/all/filtered?view=detailed&page=0']

	def parse(self, response):
		# define url pattern for each of the review pages
		# 
		num_pages = int(response.xpath('//li[@class="page last_page"]/a//text()').extract_first())
		page_urls = ['https://www.metacritic.com/browse/games/score/metascore/all/all/filtered?view=detailed&page={}'.format(x) for x in range(0,num_pages+1)]


		for url in page_urls:
			yield Request(url = url, callback = self.parse_games_list)

	def parse_games_list(self, response):
		# Parse ranking and url for each videogamee 
		#game_ranks = response.xpath('//table[@class="clamp-list"]/tr//span[@class="title numbered"]/text()').extract()

		game_urls = response.xpath('//table[@class="clamp-list"]//td[@class="clamp-summary-wrap"]/a/@href').extract()

		platforms = response.xpath('//div[@class="platform"]//span[@class="data"]/text()').extract()

		#print("---"*50, len(game_urls), "---"*50)
		#rank = 0
		for i, url in enumerate(['https://www.metacritic.com{}'.format(x) for x in game_urls]):
				for rank in [re.findall('\d+',response.xpath('//table[@class="clamp-list"]/tr//span[@class="title numbered"]/text()').extract()[i])]:
					platform = platforms[i].strip()
					yield Request(url=url, meta = {'rank': rank, 'platform': platform}, callback = self.parse_game_page)

	def parse_game_page(self,response):
		rank = response.meta['rank']
		platform = response.meta['platform']
		
		title = response.xpath('//div[@class="product_title"]//h1/text()').extract_first()
		
		critic_score_pos = response.xpath('//div[@class="metascore_w xlarge game positive"]//span/text()').extract_first()
		critic_score_mix = response.xpath('//div[@class="metascore_w xlarge game mixed"]//span/text()').extract_first()
		critic_score_neg = response.xpath('//div[@class="metascore_w xlarge game negative"]//span/text()').extract_first()
		

		critic_consensus = response.xpath('//div[@class="score_summary metascore_summary"]//div[@class="summary"]//span[@class="desc"]/text()').extract_first().strip()
		num_critic_reviews = response.xpath('//div[@class="score_summary metascore_summary"]//div[@class="summary"]//span[@class="count"]//a/span/text()').extract_first()
		num_critic_reviews = re.findall('\d+', num_critic_reviews)[0]
		
		user_score_pos = response.xpath('//div[@class="metascore_w user large game positive"]/text()').extract_first()
		user_score_mix = response.xpath('//div[@class="metascore_w user large game mixed"]/text()').extract_first()
		user_score_neg = response.xpath('//div[@class="metascore_w user large game negative"]/text()').extract_first()
		
		user_consensus = response.xpath('//div[@class="userscore_wrap feature_userscore"]//span[@class="desc"]/text()').extract_first()
		num_user_reviews = response.xpath('//div[@class="userscore_wrap feature_userscore"]//span[@class="count"]//a/text()').extract_first()
		num_user_reviews = re.findall('\d+', num_user_reviews)[0]
		
		game_summary_long = response.xpath('//li[@class="summary_detail product_summary"]//span[@class="blurb blurb_expanded"]/text()').extract_first()
		game_summary = ''.join(response.xpath('//li[@class="summary_detail product_summary"]//span/text()').extract()[1:])
		game_summary = re.sub('\s{2,}', ' ', game_summary)

		
		release_date = response.xpath('//li[@class="summary_detail release_data"]//span[@class="data"]/text()').extract_first()
		
		
		#platform = response.xpath('//div[@class="product_title"]/span[@class="platform"]/text()').extract_first().strip()
		
		developer = response.xpath('//li[@class="summary_detail developer"]/span[@class="data"]/a[@class="button"]/text()').extract_first()
		genres = response.xpath('//li[@class="summary_detail product_genre"]//span[@class="data"]//text()').extract()
		num_players = str(response.xpath('//li[@class="summary_detail product_players"]//span[@class="data"]//text()').extract_first())
		esrb_rating = response.xpath('//li[@class="summary_detail product_rating"]//span[@class="data"]//text()').extract_first()

		# add these fields to our item

		item = VideogamesItem()
		item['rank'] = rank
		item['title'] = title

		item['critic_score_pos'] = critic_score_pos
		item['critic_score_mix'] = critic_score_mix
		item['critic_score_neg'] = critic_score_neg
		
		item['critic_consensus'] = critic_consensus
		item['num_critic_reviews'] = num_critic_reviews
		
		item['user_score_pos'] = user_score_pos
		item['user_score_mix'] = user_score_mix
		item['user_score_neg'] = user_score_neg
		
		item['user_consensus'] = user_consensus
		item['num_user_reviews'] = num_user_reviews
		
		item['game_summary'] = game_summary
		item['game_summary_long'] = game_summary_long
		

		item['release_date'] = release_date
		item['platform'] = platform
		item['developer'] = developer
		item['genres'] = genres
		item['num_players'] = num_players
		item['esrb_rating'] = esrb_rating

		yield item 



## Next step is to yielf an item containing each of these fields