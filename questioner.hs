import Ask
import System.Random.Shuffle
import Control.Applicative
import qualified System.IO.Strict as So
import System.Console.Readline
import Control.Monad
import Text.Printf

questionPath = "/home/wes/haskell/Questioner/questions.json"
getQuestionFile = So.readFile questionPath

getjust (Just a) = a

main = do
                questions <- ((shuffleM . (return . repeat)) <$> decodeQuestions <$> getQuestionFile)
                q <- questions
                foldM_ ask (0, 0) (join (join q)) where
                    ask (t, a) q = do
                                        printf "Percent correct so far: %f\n" ((a/t)*100 :: Float)
                                        print q
                                        answer <- readline "> "
                                        let isCorrect = (correct (getjust answer) q)
                                        printf "%s is %s\n\n" (getjust answer) (show isCorrect)
                                        case isCorrect of
                                            (True) -> return (t+1, a+1)
                                            (False) -> return (t+1, a)