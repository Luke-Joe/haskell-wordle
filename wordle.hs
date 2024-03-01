import System.Random
import Data.List (intersect, (\\))
import ListOfWords (wordList)
{-# LANGUAGE LambdaCase #-}


tempList :: [String]
tempList = ["which","there","their","about","would","these","other","words","could","write","first"]

generateRandomWord :: [a] -> IO a
generateRandomWord wordList = do
    index <- randomRIO (0, length wordList - 1)
    return $ wordList !! index

-- Define the target word
--targetWord :: String
--targetWord = generateRandomWord

-- Function to check if a character is present in the target word
containsCharacter :: Char -> String -> Bool
containsCharacter c word = c `elem` word

-- Function to provide feedback on the correctness and position of the guessed word
evaluateGuess :: String -> String -> [State]
evaluateGuess guess target =
    let correctChars = intersect guess target -- returns list of correct characters in guess
        correctPositions = zipWith (\x y -> if x == y then Success else Misplace) guess target -- Checks if any are in correct spot, labels
        remainingGuesses = guess \\ correctChars -- returns list of incorrect characters in guess
        feedback = map (\c -> if c `elem` remainingGuesses then Fail else Success) guess -- Checks characters in guess for incorrectness
        misplaceIndices = [i | (i, c) <- zip [0..] guess, c `elem` target && c /= target !! i]
    in zipWith (\fb idx -> if idx `elem` misplaceIndices then Misplace else fb) feedback [0..]


-- Validate input correctness. Must be 5 characters and a valid english word
validateInput :: String -> Bool
validateInput input = (length input == 5) && (elem input wordList)

-- Main game loop
playWordle :: String -> Int -> IO ()
playWordle target attemptsLeft
    | attemptsLeft == 0 = putStrLn $ "Out of attempts. Game over. The answer was: "++ target
    | otherwise = do
        putStrLn $ "Attempts left: " ++ show attemptsLeft
        putStrLn "Enter your guess:"
        guess <- getLine
        let feedback = evaluateGuess guess target
        if validateInput guess 
            then do
                putStrLn "Feedback:"
                putStrLn (concatMap (\s -> showState s) feedback)
                putStrLn $ "\x1b[37m"
                putStrLn "" 
                if guess == target
                    then putStrLn "Congratulations! You guessed the word!"
                    else playWordle target (attemptsLeft - 1)
            else do 
                putStrLn "" 
                putStrLn $ "\x1b[91m"
                putStrLn "Your guess must be a valid 5 letter english word."
                putStrLn $ "\x1b[37m"
                putStrLn "" 
                playWordle target attemptsLeft

data State = Fail | Success | Misplace

showState :: State -> String
showState state = case state of
  Fail -> "\ESC[91mO" -- Red grid
  Success -> "\ESC[32mO" -- Green circle
  Misplace -> "\ESC[33mO" -- Yellow grid

-- Main function
main :: IO ()
main = do
    buffer <- generateRandomWord wordList
    let targetWord = buffer :: String
    putStrLn "Welcome to Wordle!"
    putStrLn "Try to guess the target word in 5 attempts."
    putStrLn "The target word contains 5 letters."
    playWordle targetWord 5
