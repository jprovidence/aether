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


stringOfText = "Finally, duck programmed systems seem more agile in that major changes can be made on the fly without reprogramming. Let’s speak plainly: Without reprogramming doesn’t really mean Without those pesky and expensive programmers. It really means Without all that ridiculous project overhead of writing requirements, writing tests, managing a small project to implement the changes, testing, and signing off that it has been done as specified"
otherText = "Duck programming also exposes projects to Naked Risk the possibility that bad things will happen without safeguards to prevent it or processes for recovering from disaster. Duck programming can be seductive to development teams because it pushes a lot of project risk away from the project team and onto the shoulders of the users. If something goes drastically wrong, the response from the team will be a shrug and the cryptic notation PBKAC.2 The system works as designed thus any problem is the fault of the users for misusing it."


main = do
    vit <- trainVit
    --tag vit (B.pack stringOfText) >>= nouns >>= putStrLn . show
    sim <- similarity vit proportionalRep cartwheel euclideanDistance otherText stringOfText
    putStrLn $ show sim
    --t <- htmlText "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --putStrLn $ show t
    --lks <- htmlLinks "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --return lks >>= putStrLn . show . fromJust
    --communicate




