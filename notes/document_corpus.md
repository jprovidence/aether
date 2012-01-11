# Aether Document Corpus

## Sources

Documents in the Aether Document corpus will come mainly from blogs. This is due most blog articles being of a single topic and relatively
concise. All articles will be in English and of a 300 word minimum length. Articles will be on random topics and of various writing styles.


## Article Retrieval

#### Overview

All articles will be obtained from blogs with some variety of XML feed (RSS/Atom). Blogs will be polled daily for new content. The initial
blog index will be seeded by a hand-selected list. As articles are parsed, all outgoing links will be checked in an attempt to discover new
feeds. 

In addition, a web-crawler may be used for feed discovery. Results of the crawler will be screened computationally, by a human or both. In
practice, it seems many feeds discovered in this manner are spam, low-quality or non-English. Only if the discovered feeds pass screening
will they be included into the overall index. 


#### Implementation

Code for the implementation described below can be found [here](https://github.com/jprovidence/aether/blob/master/src/Entry.hs) and
[here](https://github.com/jprovidence/aether/blob/master/src/Feed.hs). If interested, many functions used in the processing of raw XML can be
found [here](https://github.com/jprovidence/aether/blob/master/src/Parse.hs). XML processing functions make heavy use of the Haskell
[HXT](http://www.haskell.org/haskellwiki/HXT) library and [Arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) language extension.


All blog data will be stored in a PostgreSQL database. The schema is relatively simple and consists of two tables:

A) Feeds

` id [PKey], serial   |   num_entries, integer  |   last_updated, timestamp without timezone   |   url, text `

B) Entries

` id [PKey], serial   |   feed_id [FKEY], integer   |   content, text   |   date, timestamp without timezone   |   title, text   |   link, text `


Within the Aether program, blog data will have the following Haskell representation:

A) Feeds
```haskell
data Feed = Feed { _url :: String
                 , _id  :: Int
                 } deriving Show
```

B) Entries
```haskell
data Entry = Entry { description :: String
                   , title       :: String
                   , link        :: String
                   , date        :: String
                   } deriving Show 
```

The [HDBC](http://hackage.haskell.org/package/HDBC) library will be used to communicate with the database. Generally, boilerplate database
code will be encapsulated in the functions `transact` and `wrap` which run transactions requiring a transaction (update, insert) and those 
that do not (queries) respectively. Both are higher order functions of type `(Connection -> IO b) -> IO b`. Utility functions are also 
defined to simplify common operations, such as Feed lookup given a url or id.

New feeds are incorporated into the index according to the following routine:

1. Visit feed URL and retrive XML data.
2. Submit data to the parser, which will return `[Entry]`.
3. Commit feed data to the database:  
   `num_entries` can be initialized by computing the length of the `[Entry]` returned from the parser.  
   `last_updated` corresponds to the most recent returned pubDate.  
   `url` is the initial url from which all XML data was retrieved.
4. Obtain the `id` assigned by the database after step 3.
5. Map a commit action over each item in the returned `[Entry]`:
   `content`, all non-HTML/javascript text between `<description>` tags.
   `feed_id`, retrieved in step 4.
   `date`, pubDate of the article
   `title`, title of the article
   `link`, origLink if available, otherwise feed link.


Feeds are updated by:

1. Re-visiting the feed URL and retriving XML data.
2. Selecting only those entries with dates after the `last_updated` parameter.
3. Mapping a commit action over all entries returned from step 2 (if any).
