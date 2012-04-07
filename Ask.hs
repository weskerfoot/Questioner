module Ask (decodeQuestions, encodeQuestions, correct, Question, readquestion)  where

import Control.Monad
import Control.Applicative
import Data.List
import Data.List.Split
import Text.JSON

data Question = Question {text :: String ,
                                                     possibleAnswers :: [String],
                                                     correctAnswer :: String}

letters = join $ zipWith (\l n -> (replicate n) <$> l) (repeat ['a'..'z']) ([1, 2..])

instance Show Question where
    show (Question text answers correct) = (text ++ "\n" ++ possibleAnswers) where
        possibleAnswers = (unlines (zipWith (\n a -> n ++"\t"++ a) letters answers))
        
instance Read Question where
    readsPrec _ a = readquestion a

readquestion q = decompose $ splitOn ":" q where
    decompose (text:answers:correct:[]) = [(Question text (splitOn "|" answers) correct, "")]
    decompose (_) = [(Question "" [] "", q)]

instance JSON Question where
    readJSON a = Ok (getInfo (Ok a))
    showJSON (Question text posAns corAns) = JSObject (toJSObject [("question", putQuestion text), 
                                                                                                                               ("answers", putAnswers posAns), 
                                                                                                                               ("correct", putCorrect corAns)])

getr (Ok a) = a
getj (JSObject a) = a

getQuestion attribute decoded = fromJSString $ getr $ (valFromObj attribute (getj $ getr decoded))
getAnswers decoded = map fromJSString $ getr (valFromObj "answers" (getj $ getr decoded))

getInfo decoded = Question (getQuestion "question" decoded) (getAnswers decoded) (getQuestion "correct" decoded)

putAnswers possibleAnswers = JSArray $ map JSString $ map toJSString possibleAnswers
putQuestion text = JSString $ toJSString text
putCorrect answer = JSString $ toJSString answer

correct :: String -> Question -> Bool
correct answer question
    | answer == (correctAnswer question) = True
    | otherwise = False

decodeQuestions :: String -> [Question]
decodeQuestions json = getr $ decode json

encodeQuestions :: [Question] -> String
encodeQuestions q = encode q