import Data.Char

hackermind :: String -> Int -> IO ()
hackermind code turns = 
    do
        -- check user input
        let (correct, feedback) = check_input code turns
        if correct
            -- if user input checks out, start the game
            then do
                -- show greeting
                putStrLn "Welcome to Hackermind!"
                let code' = [read [x] :: Int | x <- code]
                turn code' turns
            -- if not, show feedback to user
            else putStrLn feedback

check_input :: String -> Int -> (Bool, String)
check_input code turns = 
    -- turns has to be an even number greater than zero
    if turns <= 0 || (mod turns 2) /= 0
        then (False, "Please enter an even number of turns.")
        else check_code code
        
check_code :: String -> (Bool, String)
check_code code = 
    do
        -- code has to be four digits between 1 and 6
        let correct_digits = [x | x <- code, isDigit x && elem x ['1'..'6']]
        if (length correct_digits) /= 4
            then (False, "Please enter four digits between 1 and 6.")
            else (True, "")
    
turn :: [Int] -> Int -> IO ()
turn code 0 = putStrLn "You lose!"
turn code turns = 
    do
        if turns == 1
            then putStrLn ("You have one turn left. Use it wisely!")
            else putStrLn ("You have " ++ (show turns) ++ " turns left.")
        make_guess code turns
        
make_guess :: [Int] -> Int -> IO ()
make_guess code turns = 
    do
        putStr "Your guess: "
        guess <- getLine
        let (correct, msg) = check_code guess
        if correct
            then do
                let (victory, feedback) = check_guess code guess
                if victory 
                    then putStrLn feedback
                    else do
                        show_feedback feedback
                        -- move to next turn
                        turn code (turns - 1)
            else do
                -- if guess isn't in correct format, show feedback
                putStrLn msg
                -- then start the turn again
                turn code turns
        
check_guess :: [Int] -> String -> (Bool, String)
check_guess code guess =
    do
        -- first turn the string into an array of integers
        let guess' = [read [x] :: Int | x <- guess]
        if guess' == code
            then (True, "You win!")
            else (False, get_feedback code guess')
                
get_feedback :: [Int] -> [Int] -> String
get_feedback code guess =
    do
        -- get all blacks first
        let blacks = [if x == y
                then 'b'
                else ' ' | (x, y) <- zip code guess]
            -- then get what remains of the code and guess without the
            -- correct guesses
            (remainder_code, remainder_guess) = 
                unzip [(x, y) | (x, y, z) <- zip3 code guess blacks, z /= 'b']
            -- get the whites from the remainders
            whites = get_whites remainder_code remainder_guess ""
        -- return combined black and white pegs as feedback
        [x | x <- whites ++ blacks, x /= ' ']
                
get_whites :: [Int] -> [Int] -> String -> String
get_whites code [] whites = whites
get_whites code guess whites = 
    do
        let digit = head guess
        -- first check if the first item of the guess is in the code
        if (elem digit code)
            then do
                -- if so, remove one occurrence of the digit from the code
                -- so it isn't counted again
                let indices = [0..(length code)]
                    index = head [x | (x, y) <- zip indices code, y == digit]
                    code' = let (xs, ys) = splitAt index code in xs ++ (tail ys)
                -- add one white peg, and repeat on the rest of the guess
                get_whites code' (tail guess) (whites ++ "w")
            -- if the first item isn't in the code, proceed
            -- with rest of the guess
            else get_whites code (tail guess) whites

show_feedback :: String -> IO ()
show_feedback feedback = 
    do
        let black_count = get_pin_count feedback 'b'
            white_count = get_pin_count feedback 'w'
        putStrLn ("Black pins: " ++ (show black_count))
        putStrLn ("White pins: " ++ (show white_count))
                
get_pin_count :: String -> Char -> Int
get_pin_count feedback colour = length [x | x <- feedback, x == colour]
