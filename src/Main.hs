{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Exception hiding (TypeError)
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Function((&))
import           Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Typeable
import qualified Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text.Prettyprint.Doc.Render.String
import           Dhall
import           Dhall.Core hiding (Type, pretty)
import qualified Dhall.Core
import           Dhall.Context
import           Dhall.Parser
import           Dhall.Pretty
import           Dhall.Import
import           Dhall.TypeCheck
import           Options.Applicative hiding (Parser, Const, auto)
import qualified Options.Applicative
import           Paths_dhall_io (getDataDir)
import           System.FilePath
import           System.IO

data IOExpr
  = IOModule
  | IOTyCon
  deriving Eq

instance Pretty IOExpr where
  pretty IOModule = "IOModule"
  pretty IOTyCon = "IO"

prettyShow :: Pretty a => a -> String
prettyShow = Data.Text.Prettyprint.Doc.Render.String.renderString
             . layoutPretty defaultLayoutOptions
             . Data.Text.Prettyprint.Doc.pretty

-- | Like 'Type' but for expressions with 'Embed's
data Type' x a
  = Type'
    { extract' :: Expr Src x -> Maybe a
    , expected' :: Expr Src x
    }

main :: IO ()
main = join $ execParser $ info (options <**> helper)
     ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative")

inputFileOption :: Options.Applicative.Parser (IO (FilePath, Text))
inputFileOption = readIt <$>
    optional (strOption
    (  long "file"
      <> short 'f'
      <> metavar "FILENAME"
      <> help "Input file to run instead of stdin"
    ))
  where
    readIt Nothing = ("<<stdin>>",) <$> T.hGetContents stdin
    readIt (Just p) = (p,) <$> T.readFile p

options :: Options.Applicative.Parser (IO ())
options =
  main'
    <$> inputFileOption
    <*> strOption
        (  long "type"
          <> short 't'
          <> metavar "TYPE"
          <> help "Output type of the program; if set, a value of this type will be printed on successful completion of the program."
          <> value "{}"
        )

typeCheck :: Expr Src X -> Expr Src X -> Either (TypeError Src X) (Expr Src X)
typeCheck expr expectedType =
  let toText = Data.Text.Prettyprint.Doc.Render.Text.renderStrict
               . layoutPretty defaultLayoutOptions
               . Dhall.Pretty.prettyExpr
      suffix = toText expectedType
      annot = case expr of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr expectedType)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr expectedType
  in typeWith Dhall.Context.empty expr


main' :: IO (FilePath, Text) -> Text -> IO ()
main' getMainFile resultTypeText = do
    (mainFilePath, mainFileContents) <- getMainFile
    resultType <- throws (exprFromText "<< --type >>" resultTypeText) >>= load
    _ <- throws (typeCheck resultType (Const Dhall.Core.Type))

    datadir <- getDataDir
    let dhallDir = datadir </> "dhall"
        mainTypePath = T.pack (dhallDir </> "mainType.dhall")
        ioModulePath = T.pack (dhallDir </> "ioModule.dhall")
        
    ioType <- throws (Dhall.Parser.exprFromText "importing mainType.dhall" mainTypePath) >>= load
    ioModuleType <- throws (Dhall.Parser.exprFromText "importing ioModule.dhall" ioModulePath) >>= load
    
    let ioType' = normalize $ ioType `App` resultType
        normalizer = const Nothing
        typer :: IOExpr -> Expr s IOExpr
        typer = denote . ioTyper (App (fmap absurd ioModuleType) (Embed IOTyCon))
        ioProgramTypeReally = fmap absurd ioType'
        importer :: Import -> StateT Status IO (Expr Src Import)
        -- FIXME: use a github URL instead
        importer i | Local Absolute (File (Directory []) "ioModule.dhall") <- importType (importHashed i) =
                       pure $ fmap absurd ioModuleType
        importer i = exprFromImport i
    r <- loadForIO mainFilePath importer (ioProgramTypeReally) normalizer (const Nothing) typer mainFileContents
    r' <- run (r `App` Embed IOTyCon `App` Embed IOModule)
    -- Print the result, but not {=}
    when (not $ (denote resultType :: Expr Src X) == Record mempty) $
      putStrLn $ prettyShow r'

run :: Expr Src IOExpr -> IO (Expr Src IOExpr)
run x = case Dhall.Core.normalize x of
    App (App (App (App (Field (Embed IOModule) "bind") _) _) m) f -> do
      r <- run m
      run $ f `App` r

    App (App (Field (Embed IOModule) "pure") _) x -> do
      pure x

    Field (Embed IOModule) "readLn" -> do
      t <- T.getLine
      pure $ absurd <$> embed (injectWith defaultInterpretOptions) t

    App (Field (Embed IOModule) "writeLn") arg1
         | Just (t :: Text) <- extract' (autoWith' defaultInterpretOptions) (denote arg1) -> do
      T.putStrLn t
      pure $ Record mempty

    t -> do
      error $ "Execution stuck on expression " <> prettyShow t

class Interpret' embed a where
  autoWith' :: InterpretOptions -> Type' embed a
instance Interpret' a Text where
  autoWith' = liftType . autoWith

liftType :: Type a -> Type' b a
liftType t = Type' { extract' = \a -> extract t =<< traverse (const Nothing) a
                   , expected' = absurd <$> expected t
                   }

ioTyper :: Expr Src IOExpr -> IOExpr -> Expr Src IOExpr
ioTyper _      IOTyCon = Pi "_" (Const Dhall.Core.Type) (Const Dhall.Core.Type)
ioTyper ioType IOModule = ioType

loadForIO
    :: forall x a. (Eq x, Pretty x, Typeable x) =>
       FilePath
    -- ^ The source file to report locations from; only used in error messages
    -> (Import -> StateT Status IO (Expr Src Import))
    -> Expr Src x
    -- ^ The type of value to decode from Dhall to Haskell
    -> Dhall.Core.Normalizer X
    -> Dhall.Core.Normalizer x
    -> Dhall.TypeCheck.Typer x
    -> Text
    -- ^ The Dhall program
    -> IO (Expr Src x)
    -- ^ The decoded value in Haskell
loadForIO filename importer expectedType n n' typer txt = do
    let ctx = Dhall.Context.empty
        ctx' = Dhall.Context.empty

    rawExpr <- throws (Dhall.Parser.exprFromText filename txt)
    expr' <- fmap absurd <$> Dhall.Import.loadWith importer ctx n rawExpr
    let toText = Data.Text.Prettyprint.Doc.Render.Text.renderStrict
               . layoutPretty defaultLayoutOptions
               . Dhall.Pretty.prettyExpr
        suffix = toText expectedType
        annot :: Expr Src x
        annot = case expr' of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr' expectedType)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr' expectedType
    _ <- throws (Dhall.TypeCheck.typeWithA typer ctx' annot)
    let normalized = Dhall.Core.normalizeWith n' expr'
    pure normalized

throws :: Exception e => Either e a -> IO a
throws (Left e) = throw e
throws (Right r) = pure r
