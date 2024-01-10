
module Main where

import System.IO(hPutStrLn, stderr, hSetBuffering,
                 stdin, stdout, BufferMode(..))
import System.Environment(getArgs)
import System.Exit(exitFailure)

import Control.Monad.Trans.Class(lift)
import qualified System.Console.Haskeline as RL(
        InputT, runInputT, getInputLine, defaultSettings,
        outputStrLn,
    )

import Lexer(tokenize)
import Parser(parse)
import Eval(eval, evalShow, Value(..))

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t", input] = runTokenizer input
run ["-p", input] = runParser input
run [input]       = runEval input
run []            = runREPL
run _             = usage

runTokenizer :: String -> IO ()
runTokenizer filename = do
  source <- readFile filename
  case tokenize source of
    Left errmsg  -> die errmsg
    Right tokens -> do
      mapM_ (putStrLn . show) tokens

runParser :: String -> IO ()
runParser filename = do
  source <- readFile filename
  case parse source of
    Left errmsg  -> die errmsg
    Right expr   -> putStrLn (show expr)

runEval :: String -> IO ()
runEval filename = do
  source <- readFile filename
  case parse source of
    Left errmsg  -> die errmsg
    Right expr   -> do
      res <- eval expr
      case res of
        Left errmsg -> die errmsg
        Right ()    -> return ()

runREPL :: IO ()
runREPL = do
    welcome
    RL.runInputT RL.defaultSettings rec
  where
    welcome :: IO ()
    welcome = do
      putStrLn "Mariposa"
    rec :: RL.InputT IO ()
    rec = do
      cmd <- RL.getInputLine "mariposa> "
      case cmd of
        Nothing -> do
          RL.outputStrLn "Bye."
          return ()
        Just line -> do
          case parse line of {
            Left errmsg -> RL.outputStrLn errmsg
          ; Right expr  -> do
              res <- lift $ evalShow expr
              case res of
                Left errmsg -> RL.outputStrLn errmsg
                Right (value, strValue) ->
                  case value of
                    VNone -> return ()
                    _     -> RL.outputStrLn strValue
          }
          rec

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  mariposa                 repl"
  putStrLn "  mariposa foo.m           run program"
  putStrLn "  mariposa -t foo.m        tokenize"
  putStrLn "  mariposa -p foo.m        parse"

die :: String -> IO ()
die msg = do
  hPutStrLn stderr ("---ERROR---")
  hPutStrLn stderr msg
  exitFailure

