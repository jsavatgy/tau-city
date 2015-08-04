import Data.Char
import Data.List.Split (chunksOf)
import Options.Applicative

data Options = Options {
  scheme :: String,
  cols :: Int,
  rows :: Int,
  height :: Int,
  nokey :: Bool,
  ones :: Bool }

optsParser :: Parser Options
optsParser = Options
  <$> strOption
     (  short 's'
     <> long "scheme" 
     <> metavar "SCHEME"
     <> help "color scheme: set1, set2, ..."
     <> value "set1" )
  <*> option auto
     (  short 'c'
     <> long "cols"
     <> metavar "COLS"
     <> help "Number of colums (x) at every line"
     <> value 10 )
  <*> option auto
     (  short 'r'
     <> long "rows"
     <> metavar "ROWS"
     <> help "Number of lines (y) to print"
     <> value 10 )
  <*> option auto
     (  short 'h'
     <> long "height"
     <> metavar "HEIGHT"
     <> help "Height of each picture (one digit) in pixels"
     <> value 56 )
  <*> switch
    (  long "nokey"
    <> help "Don't print the key (picture of numeric keypad)" )
  <*> switch
    (  long "ones"
    <> help "Print ones (the number 6 for tau)" )


optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "Tau Synesthesia"
  <> header   "tau - tau desimals to synestesia mnemonic converter" )

keypad = ["789","456","123","0"]

img opts n = 
  "<img src=\"./" ++ (scheme opts) ++ "/" ++ 
   [n] ++ ".png\" height=" ++ 
   (show (height opts)) ++ ">"

generate n str = ((take n) . filtered) str
filtered str = filter isDigit str
fileName = "tau-digits.txt"
every opts = generate ((rows opts)*(cols opts)+1)
rowsOf n = chunksOf n
table opts = (rowsOf (rows opts)) . (drop 1) . (every opts)
mpimg opts t = concat (map (img opts) t)
addImgTag opts = map (mpimg opts)
lineByLine = map (++"<br>\n")

main = do
  opts <- execParser optsParserInfo
  if (nokey opts)
    then putStrLn ""
    else do 
      putStrLn ((concat . lineByLine . (addImgTag opts)) keypad)
      putStrLn "</br>\n\n"
  content <- readFile fileName
  if (ones opts)
    then putStrLn (img opts (head (every opts content)) ++"<br>\n")
    else putStrLn ""
  putStrLn ((concat . lineByLine . (addImgTag opts)) (table opts content))

