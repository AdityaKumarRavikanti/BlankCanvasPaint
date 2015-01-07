{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank                     -- import the blank canvas
import Control.Concurrent
import Control.Concurrent.STM
import           Data.Text (Text)
import Data.List
import Control.Monad.Writer

toolImageURLs::[IO Text]
toolImageURLs = [(readDataURL "image/png" "Pencil.png"),
		  (readDataURL "image/png" "Rectangle.png"),
		  (readDataURL "image/png" "Line.png"),
		  (readDataURL "image/png" "Ellipse.png"),
		  (readDataURL "image/png" "Eraser.png")]


mapping :: [(Tool,IO Text)]
mapping = [(Pencil,(readDataURL "image/png" "Pencil.png")),
	   (Rectangle,(readDataURL "image/png" "Rectangle.png")),
	   (Line,(readDataURL "image/png" "Line.png")),
	   (Ellipse,(readDataURL "image/png" "Ellipse.png")),
	   (Eraser,(readDataURL "image/png" "Eraser.png"))]

data Tool = Pencil | Line | Rectangle | Ellipse | Eraser deriving (Show, Eq)
data Area = Draw | Select Tool deriving (Show, Eq)
--type OffScreen = Canvas CanvasContext
type Canvasstack = [Canvas ()]
type Coordinates = (Double,Double)
data Current = Current Tool Canvasstack Status Coordinates
data Status = Active|InActive
    --deriving (Show, Eq)

pencilColor , eraserColor :: Text
pencilColor = "black"
eraserColor = "white"



main :: IO ()
main = do
 -- let offscreenbuffer = newCanvas (600,500)
  let initial = Current Line [] InActive (0,0)
  initial_var<-newTVarIO initial
  
  blankCanvas 4000 {events = ["mousedown" ,"mousemove","mouseup"]} $ \ context -> do
         forkIO $ viewer context initial_var
         control_loop context initial_var []
         return ()

viewer :: DeviceContext -> TVar Current-> IO()        
viewer context current_var =do
    Current tool stack status activepos<- readTVarIO current_var
    urls <- sequence toolImageURLs
    send context $ do
		let sz = min (width context) (height context)
		let size = sz*0.4
		images<-mapM newImage urls
		clearRect (0,0,width context,height context)
		beginPath()
		save()
		translate ((width context) / 2,(height context) / 2)
		drawFrame size images
		restore()
		case tool of 
		  Pencil->selectToolUI 0 context
		  Line -> selectToolUI 2 context
		  Ellipse -> selectToolUI 3 context
		  Rectangle -> selectToolUI 1 context
		  Eraser -> selectToolUI 4 context
		
		sequence_ stack
    viewer context current_var

control_loop :: DeviceContext -> TVar Current -> Canvasstack -> IO()
control_loop context current_var permstack= do
    Current tool stack status (xs,ys)<- readTVarIO current_var
    event <- wait context
    let extract :: Maybe (Double,Double) -> (Double,Double)
	extract (Just x) = x
	extract Nothing = (0,0)
	
    let (xe,ye) = extract (ePageXY event)
    case ((eType event),((ePageXY event) >>= \pos-> findArea context pos),status,tool) of
	 ("mousedown",Just Draw,InActive,_) -> do
	      let newCurrent = Current tool stack Active (xe,ye)
	      atomically $ writeTVar current_var newCurrent
	      control_loop context current_var permstack
	 --("mousedown",Just (Select newtool),InActive)->
	 ("mouseup",_,Active,_) -> do
	      let permstack = stack
	      let newCurrent = Current tool stack InActive (0,0)
	      atomically $ writeTVar current_var newCurrent
	      control_loop context current_var permstack
	 ("mousemove",Just Draw,Active,tool) -> do
             let newstack = permstack ++ [(drawing tool (xs,ys) (xe,ye))]
	     let newCurrent = Current tool newstack Active $ if tool == Pencil || tool == Eraser 
										  then (xe,ye) else (xs,ys)
	     atomically $ writeTVar current_var newCurrent
	     control_loop context current_var $if tool==Pencil || tool == Eraser 
							    then newstack else permstack
	 ("mousedown",Just (Select newtool),InActive,_) -> do
	      let newCurrent = Current newtool stack InActive (0,0)
	      atomically $ writeTVar current_var newCurrent
	      control_loop context current_var permstack
	 _ -> control_loop context current_var permstack
		
findArea :: DeviceContext->(Double,Double)-> Maybe Area
findArea context (x,y) = do
    let w= width context 
    let h= height context
    let size = (min (width context) (height context))*0.4
    let border = size-10
    let transX = x - (w/2)
    let transY = y - (h/2)
    let transXS = transX -(-size+10)
    let transYS = transY - (-size-(size/6))
    if (abs transX) < border && (abs transY) < border
       then return Draw 
       else if (transYS<45) && (transXS < 50) && (transXS > 0)
       then return $Select Pencil
       else if (transYS<45) && (transXS > 60) && (transXS < 120)
       then return $Select Rectangle
       else if (transYS<45) && (transXS > 130) && (transXS < 190)
       then return $Select Line 
       else if (transYS<45) && (transXS > 200) && (transXS < 260)
       then return $Select Ellipse
       else if (transYS<45) && (transXS > 270) && (transXS < 330)
       then return $Select Eraser
       else Nothing
		
selectToolUI :: Int -> DeviceContext-> Canvas ()
selectToolUI num context= do
	let sz = min (width context) (height context)
	let size = sz*0.4
	save()
	translate ((width context) / 2,(height context) / 2)
	save()
	translate (-size+10,-size-(size/6))
	lineWidth 1
	strokeStyle "black"
	strokeRect (fromIntegral (num*70-5),-5,45,45)
	restore()
	restore()
		

drawFrame :: Double ->[CanvasImage]-> Canvas ()
drawFrame size images= do
  strokeStyle "grey"
  lineWidth 10
  strokeRect (-size,-size,2*size,2*size)
  strokeStyle "green"
  lineWidth 4
  strokeRect (-size,-size-(size/5), 2*size, size/6)
  save()
  translate (-size+10,-size-(size/6))
  sequence_ [ do
		case elemIndex x images of
			Just i -> do
				drawImage (x, [fromIntegral i*70,0])
			Nothing -> return ()
			| x<-images
			]
  restore()	  

  
  
  
  
drawing :: Tool -> (Double,Double) -> (Double,Double) -> Canvas()
drawing Line (xs,ys) (xe,ye) = drawLine pencilColor 1 (xs,ys) (xe,ye)
drawing Rectangle (xs,ys) (xe,ye) = drawRect (xs,ys) (xe,ye)
drawing Pencil (xs,ys) (xe,ye) = drawLine pencilColor 1 (xs,ys) (xe,ye)
drawing Eraser (xs,ys) (xe,ye) = drawLine eraserColor 20 (xs,ys) (xe,ye)
drawing Ellipse (xs,ys) (xe,ye) =  drawCircle (xs,ys) (xe,ye)
--drawing _ start end = return ()


drawRect :: (Double, Double) -> (Double, Double) -> Canvas ()
drawRect (xs,ys) (xe,ye) = do
		beginPath()
		strokeStyle pencilColor
		let w= xe - xs
		let h= ye - ys
		strokeRect (xs,ys,w,h)
		closePath()


drawLine :: Text->Double->(Double, Double) -> (Double, Double) -> Canvas ()
drawLine color width (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth width
        strokeStyle color
        stroke()
        closePath()

drawCircle :: (Double, Double) -> (Double, Double) -> Canvas ()
drawCircle (xs,ys) (xe,ye)=do
  let dx= abs (xe-xs)
  let dy = abs (ye-ys)
  let x = (min xe xs) +  (fromIntegral (round (dx/2)))
  let y = (min ye ys) + (fromIntegral (round (dy/2)))
  let r = (fromIntegral (round $ sqrt ((dx*dx)+(dy*dy))))
  let startingAngle = 0 
  let endingAngle = 2 * pi
  let circumference = max dx dy
  let scaleX = dx / circumference
  let scaleY = dy / circumference
  save()
  translate (x,y)
  --translate (xs,ys)
  scale (scaleX,scaleY)
  beginPath()
  lineWidth 1
  strokeStyle "black"
  arc (0,0, r, startingAngle,endingAngle,False)
  stroke()
  closePath()
  restore()
  
  
-- newoffscreen<- send context $ do
	 -- offContext <- offscreen
	 -- with offContext $do
	   -- moveTo(50,50)
           -- lineTo(200,100)
           -- lineWidth 10
           -- strokeStyle "red"
           -- stroke()
         -- drawImage (offContext,[0,0])
         -- return offContext
    -- putStrLn $ show (width context)
    -- putStrLn $ show (height context) 
{-let ellipsecheck :: (Double,Double)->(Double,Double)-> Bool
		  ellipsecheck (xs,ys) (xe,ye)= case (findArea context (xo,yo)) of
						  Nothing -> False
						  Just Draw -> True
						  Just (Select x) -> False
			where xo = 2*xs - xe
			      yo = 2*ys - ye
	
	      let toDraw = (if tool == Ellipse then (ellipsecheck (xs,ys) (xe,ye)) else True)
	      if not toDraw 
		then (control_loop context current_var permstack)
		else-}    