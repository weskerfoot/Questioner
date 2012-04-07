import Ask
import Control.Monad.Loops
import Control.Monad
import Data.List
import System.Console.Readline
import Text.Read.HT
import qualified System.IO.Strict as So

questionPath = "/home/wes/haskell/Questioner/questions.json"

check = (readline "> ") >>= output where
    output (Nothing) = return Nothing
    output (Just a) | a == "END" = return Nothing
                                | otherwise    = return (Just a)

getQuestions = whileJust check return
getQuestionFile = So.readFile questionPath

-- Reads all of the questions and returns Nothing if they failed to parse
maybeQuestions :: Monad m => m String -> m (Maybe Question)
maybeQuestions questions =  (liftM (\x -> maybeRead x :: Maybe Question) questions)

-- Unpacks all of the values
dejust :: Monad m => m (m (Maybe r)) -> m r
dejust values = liftM (\(Just x) -> x) $ join values

-- get all of the questions from "questions.json" and then append the new ones to it
refreshedQuestions :: IO [Question]
refreshedQuestions = liftM2 (++) (liftM decodeQuestions getQuestionFile) (liftM newquestions getQuestions) where
    -- Gets all of the new questions by filtering against ones that failed to parse
    newquestions questions =  dejust $ filterM check (maybeQuestions questions) where
        check (Just a) = return True
        check (Nothing) = return False

main = do
    newqs <- (liftM encodeQuestions refreshedQuestions)
    writeFile questionPath newqs