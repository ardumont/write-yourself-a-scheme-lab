module Scheme.IOPrimitive where

import Scheme.Type
import Control.Monad.Error
import System.IO
import Scheme.Parser (readExprList
                     , readExpr )

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- | makePort wraps the Haskell function openFile, converting it to the right type
-- and wrapping its return value in the Port constructor.
-- It's intended to be partially-applied to the IOMode, ReadMode for
-- open-input-file and WriteMode for open-output-file:
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- | closePort also wraps the equivalent Haskell procedure, this time hClose:
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftM (const $ Bool True) (liftIO $ hClose port)
closePort _           = return (Bool False)

-- | readProc wraps the Haskell hGetLine and then sends the result to parseExpr,
-- to be turned into a LispVal suitable for Scheme:
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

-- | writeProc converts a LispVal to a string and then writes it out on the specified port
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftM (const $ Bool True) (liftIO $ hPrint port obj)

-- | readContents reads the whole file into a string in memory.
-- It's a thin wrapper around Haskell's readFile, again just lifting
-- the IO action into an IOThrowsError action and wrapping it in a String constructor:
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String (liftIO $ readFile filename)

-- | The helper function load doesn't do what Scheme's load does (we handle that later).
-- Rather, it's responsible only for reading and parsing a file full of statements.
-- It's used in two places: readAll (which returns a list of values) and load (which evaluates those values as Scheme expressions).
load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

-- | readAll then just wraps that return value with the List constructor:
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
