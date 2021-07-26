{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
--   Module     : Text.Compdoc
--   License    : MIT
--   Stability  : experimental
--
-- Provides functionality for transforming a `Pandoc` into a composite record.
module Text.Compdoc
  ( FContent,
    fContent,
    Compdoc (..),
    readMarkdown',
    readMarkdownFile,
    runPandocPureDefault,
    pandocToCompdoc,
    contentBlock,
    writeBlocksDefault,
    flattenMeta,
  )
where

import Composite.Aeson
import Composite.Aeson.Throw
import Composite.Record
import Composite.TH
import Data.Aeson
import Data.Vinyl ((<+>))
import Path
import RIO
import Text.Pandoc
import Text.Pandoc.Readers
import Text.Pandoc.Throw

withLensesAndProxies
  [d|
    type FContent = "content" :-> Text
    |]

-- | A Compdoc is a Record with at least an FContent field.
--
-- @since 0.3.0.0
newtype Compdoc a = Compdoc {unCompdoc :: Record (FContent ': a)}

-- | Write a list of `Block`s to `Text` using `WriterOptions` defaulting to the empty string
-- in the case of error.
--
-- @since 0.1.0.0
writeBlocksDefault :: WriterOptions -> [Block] -> Text
writeBlocksDefault wopts x = runPandocPureDefault "" (writeHtml5String wopts $ Pandoc mempty x)

-- | Run a `PandocPure` operation with a default value in the event of failure.
--
-- @since 0.1.0.0
runPandocPureDefault :: a -> PandocPure a -> a
runPandocPureDefault x = either (const x) id . runPure

-- | Read a markdown file from disk, supplying a `JsonFormat` for the metadata.
--
-- @since 0.1.0.0
readMarkdownFile ::
  (MonadIO m, MonadThrow m, Show e, Typeable e) =>
  ReaderOptions ->
  WriterOptions ->
  JsonFormat e (Record a) ->
  Path b File ->
  m (Compdoc a)
readMarkdownFile ropts wopts f srcPath =
  readFileUtf8 (toFilePath srcPath) >>= readMarkdown' ropts wopts f

-- | Read some `Pandoc` markdown as `Text` as a `Record (Compdoc a)` supplying a `JsonFormat` for the metadata.
--
-- @since 0.1.0.0
readMarkdown' :: (Show e, Typeable e, MonadThrow m) => ReaderOptions -> WriterOptions -> JsonFormat e (Record a) -> Text -> m (Compdoc a)
readMarkdown' ropts wopts f x = runPandocPureThrow (Text.Pandoc.Readers.readMarkdown ropts x) >>= pandocToCompdoc writeHtml5String wopts f

-- | Transform a `Pandoc` to a `Compdoc` supplying a `JsonFormat for the metadata.
--
-- @since 0.1.0.0
pandocToCompdoc :: (Typeable e, Show e, MonadThrow m) => (WriterOptions -> Pandoc -> PandocPure Text) -> WriterOptions -> JsonFormat e (Record a) -> Pandoc -> m (Compdoc a)
pandocToCompdoc writer wopts f (Pandoc meta xs) = do
  k <- flattenMeta (writer wopts) meta >>= parseValue' f
  return $ Compdoc $ contentBlock wopts xs <+> k

-- | Create the tail of a `Compdoc` which is just an `FContent` field.
--
-- @since 0.1.0.0
contentBlock :: WriterOptions -> [Block] -> Record (FContent : '[])
contentBlock wopts x = writeBlocksDefault wopts x :*: RNil

-- | Flatten pandoc metadata to an aeson value.
--
-- @since 0.1.0.0
flattenMeta :: MonadThrow m => (Pandoc -> PandocPure Text) -> Meta -> m Value
flattenMeta writer (Meta meta) = toJSON <$> traverse go meta
  where
    go :: MonadThrow m => MetaValue -> m Value
    go (MetaMap m) = toJSON <$> traverse go m
    go (MetaList m) = toJSONList <$> traverse go m
    go (MetaBool m) = pure $ toJSON m
    go (MetaString m) = pure $ toJSON m
    go (MetaInlines m) = toJSON <$> (runPandocPureThrow . writer . Pandoc mempty . (: []) . Plain $ m)
    go (MetaBlocks m) = toJSON <$> (runPandocPureThrow . writer . Pandoc mempty $ m)
