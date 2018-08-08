import mysql.connector
import pandas as pd
import os
from searchtweets import load_credentials, gen_rule_payload, ResultStream
from datetime import datetime as DateTime, timedelta as TimeDelta, timezone as TimeZone
from mysql.connector.optionfiles import read_option_files
from sqlalchemy.engine.url import URL
from sqlalchemy import create_engine
from unidecode import unidecode
import re

def dateFormat(unformatted):
    remove_ms = lambda x:re.sub("\+\d+\s","",x)
    mk_dt     = lambda x:DateTime.strptime(remove_ms(x), "%a %b %d %H:%M:%S %Y")
    my_form   = lambda x:"{:%Y-%m-%d %H:%M}".format(mk_dt(x))
    return my_form(unformatted)

# set up database connection
myDB  = URL(
    drivername = 'mysql+mysqlconnector', 
    host       = '127.0.0.1',
    database   = 'twitter_db',
    query      = { 'option_files' : os.environ['HOME'] + '/.my.cnf',
                   'option_groups' : 'spl',
                   'charset' : 'utf8' }
)
engine = create_engine(name_or_url=myDB)

# set api end point string
os.environ["SEARCHTWEETS_ENDPOINT"] = "https://api.twitter.com/1.1/tweets/search/fullarchive/dev.json"

# define location of api keys
premium_search_args = load_credentials("~/.twitter_keys.yaml", 
                                       yaml_key="search_tweets_api",
                                       account_type="premium",
                                       env_overwrite=True)

# define search period
searchFrom = "2017-11-21"
searchTo   = "2017-11-30"

current = searchFrom

while current <= searchTo:
    # add time constraints to timestamp string as search otherwise starts from 00:00 backwards
    timestampStringFrom = "{} {}:{}".format(current, "12", "00")
    timestampStringTo   = "{} {}:{}".format(current, "13", "00")
    
    # define payload rules
    rule = gen_rule_payload(pt_rule="(#bitcoin OR #btc) lang:en",
                            results_per_call=100,
                            from_date=timestampStringFrom,
                            to_date=timestampStringTo)
    
    # generate tweet data object
    rs     = ResultStream(rule_payload = rule,
                          max_results  = 100,
                          max_pages    =1,
                          **premium_search_args)
    tweets = list(rs.stream())

    df                       = pd.DataFrame.from_dict(tweets, orient='columns')
    df_sel                   = df[['created_at', 'text', 'id']]
    df_sel['text']           = df_sel['text'].apply(unidecode)
    df_sel.columns.values[2] = "tweet_id"
    users                    = pd.DataFrame([d.get('screen_name') for d in df['user']], columns=['user_name'])
    df_sel                   = df_sel.join(users)
    df_sel['created_at']     = df_sel['created_at'].apply(dateFormat)
    df_sel.to_sql(name='tweets', con=engine, if_exists = 'append', index=False)

    current = DateTime.strptime(current, '%Y-%m-%d') + TimeDelta(days=1)
    current = DateTime.strftime(current, '%Y-%m-%d')
