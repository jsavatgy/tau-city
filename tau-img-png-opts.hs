import Data.Char
import Data.List (sort)
import Data.List.Split (chunksOf,wordsBy)
import Options.Applicative
import Graphics.Rendering.Cairo hiding (height)
import System.FilePath

data Options = Options {
  scheme :: String,
  output :: String,
  fromChrs :: String,
  onlyChrs :: String,
  loop :: Bool,
  cols :: Int,
  rows :: Int,
  dropDigits :: Int,
  height :: Int }

optsParser :: Parser Options
optsParser = Options
  <$> strOption
     (  short 's'
     <> long "scheme" 
     <> metavar "SCHEME"
     <> help "Color scheme: set1, set2, ..."
     <> value "set1" )
  <*> strOption
     (  long "output"
     <> metavar "PNGFILE"
     <> help "Create a PNG image file with the given name"
     <> value "output.png" )
  <*> strOption
     (  long "fromchrs"
     <> metavar "FROMCHRS"
     <> help "Take input from given string instead, non-digit means newline, e.g. \"12345 67890\""
     <> value "" )
  <*> strOption
     (  long "onlychrs"
     <> metavar "ONLYCHRS"
     <> help "Print only these chars, e.g. \"13579\" for odds only"
     <> value "1234567890" )
  <*> switch
     ( long "loop"
     <> help "Loop over ONLYCHRS, printing multiple pictures" )
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
     (  long "drop"
     <> metavar "DROP"
     <> help "How many digits to omit from beginning. Default = 1 (the units not shown)"
     <> value 1 )
  <*> option auto
     (  long "height"
     <> metavar "HEIGHT"
     <> help "Height of each picture (one digit) in pixels"
     <> value 56 )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  (  fullDesc
  <> progDesc "Tau Synesthesia"
  <> header   "tau - tau desimals and synestesia mnemonics" )

generate n str = ((take n) . filtered) str
filtered str = filter isDigit str
fileName = "tau-digits.txt"
every opts = generate ((rows opts)*(cols opts) + dropDigits opts)
rowsOf n = chunksOf n
table opts = (rowsOf (cols opts)) . (drop (dropDigits opts)) . (every opts)

margin = 4

factor a b = intToDouble b / intToDouble a
imgName scheme ch = scheme ++ "/" ++ [ch] ++ ".png"

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

paintStr opts x fac g = do
  let 
    (ch,i,j) = g
    mrg = margin / fac
  surf <- liftIO (imageSurfaceCreateFromPNG (imgName (scheme opts) ch))
  setSourceSurface surf (mrg+i*x) (mrg+j*x)
  paint

maxlen strs = maximum [length s | s <- strs]
wlen h strs = intToDouble (h * maxlen strs)
whig h strs = intToDouble (h * length strs)
imgW h strs = round (2*margin + (wlen h strs))
imgH h strs = round (2*margin + (whig h strs))

createPng opts strs = do
  let w = imgW (height opts) strs
      h = imgH (height opts) strs
  surf1 <- imageSurfaceCreateFromPNG (imgName (scheme opts) '1')
  img <- createImageSurface FormatARGB32 w h
  xInt <- imageSurfaceGetWidth surf1
  let 
    x = intToDouble xInt
    fac = factor xInt (height opts)
    t' = concat [[(s,i,j) | (s,i) <- zip str ([0..])] 
                          | (str,j) <- zip strs ([0..])]
    t = filter (\(s,i,j) -> s `elem` (onlyChrs opts)) t'
  renderWith img ( do
    setSourceRGB 1 1 1
    scale fac fac
    paint
    mapM_ (paintStr opts x fac) t
    )
  surfaceWriteToPNG img (output opts)
  liftIO ( do 
    putStrLn ("Created: " ++ output opts)
    putStrLn ("Size: " ++ show w ++ "x" ++ show h ++ " pixels")
    )

oneloop opts content = do
  if ((fromChrs opts) == "")
    then createPng opts (table opts content)
    else createPng opts (wordsBy (not . isDigit) (fromChrs opts))

readInt :: String -> Int
readInt = read

next c = show (((readInt c) + 1) `mod` 10)
cycs2 s = concat [next [c] | c <- s]
cycs1 = sort . cycs2
takeUntilDuplicate = foldr (\x r -> x : takeWhile (/= x) r) []
grow x = x ++ grow [cycs1 (last x)]
cycles x = (takeUntilDuplicate . grow) [sort x]

filePathNumExt n name =
  dropExtension name ++ "-" ++ n ++ takeExtension name

main = do
  opts <- execParser optsParserInfo
  content <- readFile fileName
  if (loop opts)
    then mapM_ 
      (\n -> oneloop opts { 
        output= filePathNumExt n (output opts), 
        onlyChrs = n } 
        content)
      (cycles (onlyChrs opts))
    else oneloop opts content

