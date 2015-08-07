import Graphics.Rendering.Cairo

strs = ["12345","67890"]
h = 50
margin = 4

factor a b = intToDouble b / intToDouble a
imgName ch = "set5/" ++ [ch] ++ ".png"

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

paintStr x fac g = do
  let 
    (ch,i,j) = g
    mrg = margin / fac
  surf <- liftIO (imageSurfaceCreateFromPNG (imgName ch))
  setSourceSurface surf (mrg+i*x) (mrg+j*x)
  paint

wlen = intToDouble (h*length (strs !! 0))
whig = intToDouble (h*length strs)
imgW = round (2*margin + wlen)
imgH = round (2*margin + whig)

main = do
  surf1 <- imageSurfaceCreateFromPNG (imgName '1')
  img <- createImageSurface FormatARGB32 imgW imgH
  xInt <- imageSurfaceGetWidth surf1
  let 
    x = intToDouble xInt
    fac = factor xInt h
    t = concat [[(s,i,j) | (s,i) <- zip str ([0..])] 
                         | (str,j) <- zip strs ([0..])]
  renderWith img ( do
    setSourceRGB 1 1 1
    scale fac fac
    paint
    mapM_ (paintStr x fac) t
    )
  surfaceWriteToPNG img "img-test-strs.png"
  
  -- str = "abcdefghij"
  -- (a 1)(b 2)(c 3) ...
  -- strs = ["abcde","fghij"]
  -- (a 0 1)(b 1 1)(c 2 1)(d 3 1)(e 4 1) (f 0 2)(g 1 2) ...
  -- [[(s,i,j) | (s,i) <- zip str [0..]] | (str,j) <- zip strs [0..]]
  -- ("abcde" 0) ("fghij" 1)
  -- strs = ["1234","5678","90"]
  -- [[(s,i,j) | (s,i) <- zip str [0..]] | (str,j) <- zip strs [0..]]
  -- [[('1',0,0),('2',1,0),('3',2,0),('4',3,0)],[('5',0,1),('6',1,1),('7',2,1),('8',3,1)],[('9',0,2),('0',1,2)]]
  -- concat [[('1',0,0),('2',1,0),('3',2,0),('4',3,0)],[('5',0,1),('6',1,1),('7',2,1),('8',3,1)],[('9',0,2),('0',1,2)]]
  -- [('1',0,0),('2',1,0),('3',2,0),('4',3,0),('5',0,1),('6',1,1),('7',2,1),('8',3,1),('9',0,2),('0',1,2)]

