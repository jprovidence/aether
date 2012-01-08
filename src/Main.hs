module Main (
    main
) where


import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Control.Exception as EX
import Database.HDBC
import Database.HDBC.PostgreSQL
import Feed
import Parse
import Entry
import Communication
import Viterbi
import Text.Sim


stringOfText = "The Viterbi Algorithm was originally concocted to separate signal from noise in telecom lines. Or something. It happened before I was born, so I’m just taking wikipedia’s word for it. It’s all about probabilities, but I think it can be explained simply by thinking of drunken tourists dancing a conga line. Imagine you were walking past a conga line (you’re both going in the same direction) trying to guess the probability that each person you passed would trip over and get blood all over your Hawaiian shirt. Easy! Of course you could fathom a pretty close guess based on how many Rum’n Cokes that particular person had consumed. The teetotaler probably  has a 10% chance of tripping, whereas the guy with an empty mini-keg as a hat is pushing 60%. We can call these percentages the ‘emission probability’, or the emission of blood onto shirt probability."


main = do
    vit <- trainVit
    tag vit (B.pack stringOfText) >>= nouns >>= putStrLn . show
    --t <- htmlText "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --putStrLn $ show t
    --lks <- htmlLinks "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --return lks >>= putStrLn . show . fromJust
    --communicate




