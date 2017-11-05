{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Stack.hs
module Stack where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

type Stack = [Int]
type Output = [Int]
type Program = [Instruction]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp {
  unComp :: VM a
  } deriving (
    Monad,
    MonadReader Program,
    MonadWriter Output,
    MonadState Stack
  )

data Instruction =
  Push Int |
  Pop |
  Puts

evalInstruction :: Instruction -> Comp ()
evalInstruction i = case i of
  Pop -> modify tail
  Push n -> modify (n:)
  Puts -> do
    tos <- gets head
    tell [tos]

eval :: Comp ()
eval = do
  instructions <- ask
  case instructions of
    [] -> return ()
    (i: is) -> evalInstruction i >> local (const is) eval

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)

program :: Program
program = [
  Push 42,
  Push 27,
  Puts,
  Pop,
  Puts,
  Pop
  ]

main :: IO ()
main = mapM_ print $ execVM program
