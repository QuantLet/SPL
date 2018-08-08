import os
from sqlalchemy.engine.url import URL
from sqlalchemy import create_engine
from pandas.io import sql

myDB = URL(drivername = 'mysql+mysqlconnector', 
           host       = '127.0.0.1',
           database   = 'twitter_db',
           query      = { 'option_files' : os.environ['HOME'] + '/.my.cnf',
                          'option_groups' : 'spl',
                          'charset' : 'utf8'}
)

engine = create_engine(name_or_url=myDB)
cnx    = engine.raw_connection()
data   = sql.read_sql("SELECT * FROM tweets", cnx)

data.to_csv(os.environ['HOME'] +'/tweets.csv')

