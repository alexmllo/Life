
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw
import Drawing.Vector
import Data.Text

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        , gmPaused :: Bool
        , gmInterval :: Time    -- generation interval when not paused
        , gmElapsedTime :: Time -- elpased time from last generation
        }

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x}
setGmZoom x g        = g{ gmZoom = x}
setGmShift x g       = g{ gmShift = x}
setGmPaused x g      = g{ gmPaused = x}
setGmInterval x g    = g{ gmInterval = x}
setGmElapsedTime x g = g{ gmElapsedTime = x}

data GridMode = NoGrid | LivesGrid | ViewGrid

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = Prelude.foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    , gmZoom = 1.0, gmShift = (0.0,0.0)
    , gmPaused = True
    , gmInterval = 1.0  -- in seconds
    , gmElapsedTime = 0.0
    }

-----------------------------------------------------

nextGrid :: GridMode -> GridMode
nextGrid NoGrid = LivesGrid
nextGrid LivesGrid = ViewGrid
nextGrid _ = NoGrid
        
nextState :: Bool -> Bool
nextState True = False
nextState False = True
        
pointToPos :: Point -> Game -> Pos
pointToPos p game =
    let (gx,gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
    in (round gx, round gy)
    
modifications (x,y) z d = scaled z z (translated x y d)

drawGridCase mode x y game = 
    case mode of
        NoGrid -> blank
        LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell (gmBoard game))
        ViewGrid -> drawGrid (pointToPos((-x),(-y)) game) (pointToPos (x,y) game)

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game

handleEvent (KeyDown "G") game =
    setGmGridMode (nextGrid (gmGridMode game)) game
    
handleEvent (KeyDown "I") game =
    if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game
    else game
    
handleEvent (KeyDown "O") game =
    setGmZoom (gmZoom game * 0.5) game
    
handleEvent (KeyDown "ARROWUP") game =
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0,5)) game  -- *^ = vector * escalar
    
handleEvent (KeyDown "ARROWDOWN") game =
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0,-5)) game
    
handleEvent (KeyDown "ARROWLEFT") game =
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (-5,0)) game
    
handleEvent (KeyDown "ARROWRIGHT") game =
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (5,0)) game
    
handleEvent (KeyDown " ") game =
    setGmPaused (nextState (gmPaused game)) game
    
handleEvent (KeyDown "+") game =
    if (gmInterval game * 1/2) > 0.125 then setGmInterval (gmInterval game * 1/2) game
    else game
    
handleEvent (KeyDown "-") game =
    setGmInterval (gmInterval game * 2) game
    
handleEvent (TimePassing dt) game =
    if (gmElapsedTime game + dt >= gmInterval game && gmPaused game == False) then (setGmElapsedTime 0 $ setGmBoard (nextGeneration (gmBoard game)) game) else setGmElapsedTime (gmElapsedTime game + dt) game
    -- en cas de que el gmElapsedTime sigui mes gran o igual que gmInterval llavors evolucionarem a la nova generacio de taulell i el ElapsedTime a zero

handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game -- gmBoard game: se accede al valor del campo gmBoard del objeto game

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = pointToPos (x,y) game
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

handleEvent _ game =                            -- Ignore other events
    game

-----------------------------------------------------
-- Drawing

draw game =
    let gridMode = gmGridMode game
        board = gmBoard game
        zoom = gmZoom game
        x = viewWidth
        y = viewHeight
    in writeInstructions instructions <> modifications (gmShift game) zoom (drawBoard board <> drawGridCase gridMode x y game)

writeInstructions a = foldMap writeInstruction a

writeInstruction :: (String,String,Int) -> Drawing
writeInstruction (s1, s2, line) =
    let dx1 = ((-viewWidth) * 0.5 +1)
        dx2 = (dx1 + 6)
        dy = (viewHeight * 0.5 -1 - fromIntegral line)
    in colored blue $ translated dx1 dy (atext startAnchor (pack s1)) <> translated dx2 dy (atext startAnchor (pack s2))
    
instructions :: [(String,String,Int)]
instructions = [("N","Next Step",1),("G","Change to grid mode",2),("O","Zomm out",3),("I","Zoom in",4),("ARROWUP","shift down",5),("ARROWDOWN","Shift up",6),("ARROWRIGHT","Shift left",7),("ARROWLEFT","Shift right",8),("SPACE","Toggle pause/run",9),("+","increase run velocity",10),("-","Decrease run velocity",11),("Use the mouse to set live/death cells","",12)]
