{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}

charToPhoneDigit :: Char -> Int
charToPhoneDigit ch 
  | elem ch "abcABC" = 2
  | elem ch "defDEF" = 3
  | elem ch "ghiGHI" = 4
  | elem ch "jklJKL" = 5
  | elem ch "mnoMNO" = 6
  | elem ch "pqrsPQRS" = 7
  | elem ch "tuvTUV" = 8
  | elem ch "wxyzWXYZ" = 9

filterList w = [ x | x <- list, elem (head(x)) (['a'..'z']++['A'..'Z'])]
  where list = map (:[]) w

wordsToPhone' :: String -> Int
wordsToPhone' [] = 0
wordsToPhone' w = (charToPhoneDigit (head(head(list)))*10^(length(w)-1)) + wordsToPhone (concat(tail(list)))
  where list = map (:[]) w

wordsToPhone w = wordsToPhone' (concat(filterList w)) 

filterList' list num1 = [ x | x <- list, (wordsToPhone x) == num1]

main = do
    putStrLn "Enter a four digit number"
    num <- readLn
    dictionary' <- readFile "/usr/share/dict/american-english"
    putStrLn (show (filterList' (words dictionary') num))
