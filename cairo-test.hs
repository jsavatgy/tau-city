import Graphics.Rendering.Cairo

str = "1234567890"
h = 50
margin = 4

factor a b = intToDouble b / intToDouble a
imgName ch = "set5/" ++ [ch] ++ ".png"

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

paintStr x fac g = do
  let 
    (ch,i) = g
    mrg = margin / fac
  surf <- liftIO (imageSurfaceCreateFromPNG (imgName ch))
  setSourceSurface surf (mrg+i*x) mrg
  paint

wlen = intToDouble (h*length str)
imgW = round (2*margin + wlen)
imgH = round (2*margin + intToDouble h)

main = do
  surf1 <- imageSurfaceCreateFromPNG (imgName '1')
  img <- createImageSurface FormatARGB32 imgW imgH
  xInt <- imageSurfaceGetWidth surf1
  let 
    x = intToDouble xInt
    fac = factor xInt h
  renderWith img ( do
    setSourceRGB 1 1 1
    scale fac fac
    paint
    mapM_ (paintStr x fac) [(ch,i) |(ch,i) <- zip str [0..]]
    )
  surfaceWriteToPNG img "img-test.png"
  
  
