{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.ReadFS
  ( -- * ReadFS Effect
    ReadFS (..),
    ReadFSErr (..),
    ReadFSIOC (..),

    -- * Reading raw file contents
    readContentsBS,
    readContentsText,

    -- * Resolving relative filepaths
    resolveFile,
    resolveFile',
    resolveDir,
    resolveDir',

    -- * Checking whether files exist
    doesFileExist,
    doesDirExist,

    -- * Parsing file contents
    readContentsParser,
    readContentsJson,
    readContentsToml,
    readContentsYaml,
    readContentsXML,
    module X,
  )
where

import Control.Algebra as X
import qualified Data.Aeson.Types as Aeson
import Control.Applicative (Alternative, (<|>), empty)
import Control.Effect.Diagnostics
import Control.Effect.Record
import Control.Effect.Record.TH
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Exception as E
import Control.Monad ((<=<), guard)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Constraint, Type)
import Data.PolyKinded
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Void (Void)
import Data.Yaml (decodeEither', prettyPrintParseException)
import Parse.XML (FromXML, parseXML, xmlErrorPretty)
import Path
import qualified Path.IO as PIO
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Toml
import Control.Carrier.Reader
import Control.Monad.Trans (MonadTrans)
import Control.Effect.Sum (Member(inj))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Generics.Kind.TH
import GHC.Generics hiding ((:+:))
import qualified GHC.Generics as Generics
import GHC.TypeLits
import Generics.Kind hiding ((:+:))
import qualified Generics.Kind as GKind

data ReadFS (m :: Type -> Type) k where
  ReadContentsBS' :: Path x File -> ReadFS m (Either ReadFSErr ByteString)
  ReadContentsText' :: Path x File -> ReadFS m (Either ReadFSErr Text)
  DoesFileExist :: Path x File -> ReadFS m Bool
  DoesDirExist :: Path x Dir -> ReadFS m Bool
  ResolveFile' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs File))
  ResolveDir' :: Path Abs Dir -> Text -> ReadFS m (Either ReadFSErr (Path Abs Dir))

data ReadFSErr
  = -- | A file couldn't be read. file, err
    FileReadError FilePath Text
  | -- | A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
    FileParseError FilePath Text
  | -- | An IOException was thrown when resolving a file/directory
    ResolveError FilePath FilePath Text
  deriving (Eq, Ord, Show)

instance ToDiagnostic ReadFSErr where
  renderDiagnostic = \case
    FileReadError path err -> "Error reading file " <> pretty path <> " : " <> pretty err
    FileParseError path err -> "Error parsing file " <> pretty path <> " : " <> pretty err
    ResolveError base rel err -> "Error resolving a relative file. base: " <> pretty base <> " . relative: " <> pretty rel <> " . error: " <> pretty err

-- | Read file contents into a strict 'ByteString'
readContentsBS' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr ByteString)
readContentsBS' path = send (ReadContentsBS' path)

-- | Read file contents into a strict 'ByteString'
readContentsBS :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m ByteString
readContentsBS = fromEither <=< readContentsBS'

-- | Read file contents into a strict 'Text'
readContentsText' :: Has ReadFS sig m => Path b File -> m (Either ReadFSErr Text)
readContentsText' path = send (ReadContentsText' path)

-- | Read file contents into a strict 'Text'
readContentsText :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m Text
readContentsText = fromEither <=< readContentsText'

-- | Resolve a relative filepath to a file
resolveFile' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs File))
resolveFile' base path = send (ResolveFile' base path)

-- | Resolve a relative filepath to a file
resolveFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolveFile dir path = fromEither =<< resolveFile' dir path

-- | Resolve a relative filepath to a directory
resolveDir' :: Has ReadFS sig m => Path Abs Dir -> Text -> m (Either ReadFSErr (Path Abs Dir))
resolveDir' base path = send (ResolveDir' base path)

-- | Resolve a relative filepath to a directory
resolveDir :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs Dir)
resolveDir dir path = fromEither =<< resolveDir' dir path

-- | Check whether a file exists
doesFileExist :: Has ReadFS sig m => Path b File -> m Bool
doesFileExist path = send (DoesFileExist path)

-- | Check whether a directory exists
doesDirExist :: Has ReadFS sig m => Path b Dir -> m Bool
doesDirExist path = send (DoesDirExist path)

type Parser = Parsec Void Text

-- | Read from a file, parsing its contents
readContentsParser :: forall a sig m b. (Has ReadFS sig m, Has Diagnostics sig m) => Parser a -> Path b File -> m a
readContentsParser parser file = do
  contents <- readContentsText file
  case runParser parser (toFilePath file) contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack (errorBundlePretty err)))
    Right a -> pure a

-- | Read JSON from a file
readContentsJson :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsJson file = do
  contents <- readContentsBS file
  case eitherDecodeStrict contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack err))
    Right a -> pure a

readContentsToml :: (Has ReadFS sig m, Has Diagnostics sig m) => Toml.TomlCodec a -> Path b File -> m a
readContentsToml codec file = do
  contents <- readContentsText file
  case Toml.decode codec contents of
    Left err -> fatal (FileParseError (toFilePath file) (Toml.prettyTomlDecodeErrors err))
    Right a -> pure a

-- | Read YAML from a file
readContentsYaml :: (FromJSON a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsYaml file = do
  contents <- readContentsBS file
  case decodeEither' contents of
    Left err -> fatal (FileParseError (toFilePath file) (T.pack $ prettyPrintParseException err))
    Right a -> pure a

-- | Read XML from a file
readContentsXML :: (FromXML a, Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m a
readContentsXML file = do
  contents <- readContentsText file
  case parseXML contents of
    Left err -> fatal (FileParseError (toFilePath file) (xmlErrorPretty err))
    Right a -> pure a

newtype ReadFSIOC m a = ReadFSIOC {runReadFSIO :: m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadFail)

instance Has (Lift IO) sig m => Algebra (ReadFS :+: sig) (ReadFSIOC m) where
  alg hdl sig ctx = ReadFSIOC $ do
    case sig of
      L (ReadContentsBS' file) -> do
        res <- catchingIO (BS.readFile (toFilePath file)) (FileReadError (toFilePath file))
        pure (res <$ ctx)
      L (ReadContentsText' file) -> do
        res <- catchingIO (decodeUtf8 <$> BS.readFile (toFilePath file)) (FileReadError (toFilePath file))
        pure (res <$ ctx)
      L (ResolveFile' dir path) -> do
        res <- catchingIO (PIO.resolveFile dir (T.unpack path)) (ResolveError (toFilePath dir) (T.unpack path))
        pure (res <$ ctx)
      L (ResolveDir' dir path) -> do
        res <- catchingIO (PIO.resolveDir dir (T.unpack path)) (ResolveError (toFilePath dir) (T.unpack path))
        pure (res <$ ctx)
      -- NB: these never throw
      L (DoesFileExist file) -> (<$ ctx) <$> sendIO (PIO.doesFileExist file)
      L (DoesDirExist dir) -> (<$ ctx) <$> sendIO (PIO.doesDirExist dir)
      R other -> alg (runReadFSIO . hdl) other ctx
    where
      catchingIO :: IO a -> (Text -> ReadFSErr) -> m (Either ReadFSErr a)
      catchingIO io mangle = sendIO $ E.catch (Right <$> io) (\(e :: E.IOException) -> pure (Left (mangle (T.pack (show e)))))

---------------


gtoJSON' :: forall t. (GenericK t, GToJSONK (RepK t) 'LoT0) => t -> Value
gtoJSON' x = gtoJSON (fromK @_ @t @'LoT0 x)

gfromJSON' :: forall t. (GenericK t, GFromJSONK (RepK t) 'LoT0) => Value -> Aeson.Parser t
gfromJSON' v = fmap (toK @_ @t @'LoT0) (gfromJSON v)

class GToJSONK (f :: LoT k -> Type) (x :: LoT k) where
  gtoJSON :: f x -> Value
class GFromJSONK (f :: LoT k -> Type) (x :: LoT k) where
  gfromJSON :: Value -> Aeson.Parser (f x)

instance ToJSON (Interpret t x) => GToJSONK (Field t) x where
  gtoJSON (Field t) = toJSON t
instance FromJSON (Interpret t x) => GFromJSONK (Field t) x where
  gfromJSON = fmap Field . parseJSON

instance GToJSONK U1 x where
  gtoJSON U1 = Null
instance GFromJSONK U1 x where
  gfromJSON Null = pure U1
  gfromJSON _    = empty

instance (GToJSONK f x, GToJSONK g x) => GToJSONK (f GKind.:+: g) x where
  gtoJSON (L1 f) = gtoJSON f
  gtoJSON (R1 g) = gtoJSON g
instance (GFromJSONK f x, GFromJSONK g x) => GFromJSONK (f GKind.:+: g) x where
  gfromJSON v = (L1 <$> gfromJSON v) <|> (R1 <$> gfromJSON v)

instance (GToJSONK f x, GToJSONK g x) => GToJSONK (f :*: g) x where
  gtoJSON (f :*: g) = toJSON (gtoJSON f, gtoJSON g)
instance (GFromJSONK f x, GFromJSONK g x) => GFromJSONK (f :*: g) x where
  gfromJSON v = do (f, g) <- parseJSON v
                   (:*:) <$> gfromJSON f <*> gfromJSON g

instance forall name f x i fx st. (GToJSONK f x, KnownSymbol name) => GToJSONK (M1 i ('MetaCons name fx st) f) x where
  gtoJSON (M1 f) = toJSON (symbolVal $ Proxy @name, gtoJSON f)
instance forall name f x i fx st. (GFromJSONK f x, KnownSymbol name) => GFromJSONK (M1 i ('MetaCons name fx st) f) x where
  gfromJSON v = do (name, f) <- parseJSON v
                   guard $ name == symbolVal (Proxy @name)
                   M1 <$> gfromJSON f

instance GToJSONK f x => GToJSONK (M1 i ('MetaData _1 _2 _3 _4) f) x where
  gtoJSON (M1 f) = gtoJSON f
instance GFromJSONK f x => GFromJSONK (M1 i ('MetaData _1 _2 _3 _4) f) x where
  gfromJSON = fmap M1 . gfromJSON

instance GToJSONK f x => GToJSONK (M1 i ('MetaSel _1 _2 _3 _4) f) x where
  gtoJSON (M1 f) = gtoJSON f
instance GFromJSONK f x => GFromJSONK (M1 i ('MetaSel _1 _2 _3 _4) f) x where
  gfromJSON = fmap M1 . gfromJSON

instance (Interpret c x => GToJSONK f x) => GToJSONK (c :=>: f) x where
  gtoJSON (SuchThat f) = gtoJSON f
instance (Interpret c x, GFromJSONK f x) => GFromJSONK (c :=>: f) x where
  gfromJSON = fmap SuchThat . gfromJSON

instance (forall t. GToJSONK f (t ':&&: x)) => GToJSONK (Exists k f) x where
  gtoJSON (Exists x) = gtoJSON x
instance (forall t. GFromJSONK f (t ':&&: x)) => GFromJSONK (Exists k f) x where
  gfromJSON = fmap Exists . gfromJSON

instance ToJSON (Test m a) where
  toJSON = gtoJSON'

--instance FromJSON (Test m a) where
  --parseJSON = gfromJSON'

-------------------
newtype ReplayC (e :: (Type -> Type) -> Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a =
  ReplayC { runReplayC :: ReaderC (Map Value Value) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (Member e sig, Algebra sig m, Replayable e) => Algebra (e :+: sig) (ReplayC e sig m) where
  alg hdl sig' ctx = ReplayC $ do
    mapping <- ask @(Map Value Value)
    case sig' of
      L something -> do
        case replayLookup something mapping of
          Nothing -> alg (runReplayC . hdl) (R (inj something)) ctx
          Just val -> pure (val <$ ctx)
      R other -> alg (runReplayC . hdl) (R other) ctx

class Replayable (e :: (Type -> Type) -> Type -> Type) where
  replayLookup :: e m a -> Map Value Value -> Maybe a
  default replayLookup :: forall m a. (GenericK (e m a), GToJSONK (RepK (e m a)) 'LoT0, GenericK a, FromJSON a) => e m a -> Map Value Value -> Maybe a
  replayLookup e m = case M.lookup (gtoJSON' e) m of
    Nothing -> Nothing
    Just x -> case fromJSON @a x of
      Error _ -> Nothing -- TODO error message
      Success a -> pure a

type GRP (l :: LoT (Type -> Type)) = FromJSON (HeadLoT l)

--class Recordable (e :: (Type -> Type) -> Type -> Type) where
  --recordValues :: e m a -> a -> (Value,Value)
  --default recordValues :: forall m a. (GenericK (e m a), GToJSONK (RepK (e m a)) 'LoT0, ToJSON a) => e m a -> a -> (Value,Value)
  --recordValues e a = (gtoJSON' e, toJSON a)

--instance Recordable Test where
  --recordValues e@(Foo _ _) a = (toJSON e, toJSON a)
  --recordValues e@(Bar _) a = (toJSON e, toJSON a)
  --recordValues e@(Baz _) a = (toJSON e, toJSON a)
  --recordValues e@(Quux _) a = (toJSON e, toJSON a)

newtype TestC m a = TestC { runTestC :: m a }
  deriving (Functor, Applicative, Monad)

data Test (m :: Type -> Type) a where
  Foo :: String -> Bool -> Test m Int
  Bar :: String -> Test m String
  Baz :: String -> Test m Int
  Quux :: Bool -> Test m Int
  XX :: Bool -> Bool -> Bool -> Test m Int

$(deriveGenericK ''Test)

--instance FromJSON (Test m a) where
  --parseJSON = gfromJSON'

-------------

instance Replayable Test where
  replayLookup = undefined

instance Algebra sig m => Algebra (Test :+: sig) (TestC m) where
  alg hdl sig ctx = TestC $ do
    case sig of
      L (Foo s t) -> pure (length s <$ ctx)
      R other -> alg (runTestC . hdl) other ctx

foo :: Has Test sig m => String -> m Int
foo s = send (Foo s False)

testy :: Int
testy = run . runTestC . runReader (M.singleton "hello" "6") . runReplayC @Test $ foo "hello"

testy2 :: Int
testy2 = run . runTestC . runReader M.empty . runRecordC @Test $ foo "hello"

$(deriveRecordable ''Test)
