# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class VideogamesItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    rank = scrapy.Field()
    title = scrapy.Field()
    
    critic_score_pos = scrapy.Field()
    critic_score_mix = scrapy.Field()
    critic_score_neg = scrapy.Field()


    critic_consensus = scrapy.Field()
    num_critic_reviews = scrapy.Field()
    
    user_score_pos = scrapy.Field()
    user_score_mix = scrapy.Field()
    user_score_neg = scrapy.Field()

    user_consensus = scrapy.Field()
    num_user_reviews = scrapy.Field()

    game_summary_long = scrapy.Field()
    game_summary = scrapy.Field()

    release_date = scrapy.Field()
    platform = scrapy.Field()
    developer = scrapy.Field()

    genres = scrapy.Field()
    num_players = scrapy.Field()
    esrb_rating = scrapy.Field()
