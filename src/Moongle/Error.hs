{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Moongle.Error
  ( MoongleError (..),
    RegistryError (..),
    DatabaseError (..),
    ParserError (..),
    throwMoongleError,
    handleRegistryError,
    handleDatabaseError,
    handleParserError,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static

-- | 统一的错误类型，提供更好的错误分类和处理
data MoongleError
  = RegistryErr RegistryError
  | DatabaseErr DatabaseError
  | ParserErr ParserError
  | ConfigErr Text
  | NetworkErr Text
  | FileSystemErr Text
  | ValidationErr Text
  deriving (Show, Eq)

instance Exception MoongleError

-- | 注册表相关错误
data RegistryError
  = RegistryNotFound Text
  | RegistryParseError Text
  | PackageNotFound Text
  | PackageDownloadError Text
  | InvalidPackageStructure Text
  deriving (Show, Eq)

-- | 数据库相关错误
data DatabaseError
  = ConnectionFailed Text
  | QueryFailed Text
  | TransactionFailed Text
  | SchemaError Text
  | DataIntegrityError Text
  deriving (Show, Eq)

-- | 解析器相关错误
data ParserError
  = SyntaxError Text
  | SemanticError Text
  | TypeCheckError Text
  | UnsupportedConstruct Text
  deriving (Show, Eq)

-- | 抛出 MoongleError
throwMoongleError :: (Error MoongleError :> es) => MoongleError -> Eff es a
throwMoongleError = throwError

-- | 处理注册表错误
handleRegistryError :: (Error MoongleError :> es) => Text -> Eff es a
handleRegistryError msg = throwMoongleError (RegistryErr (RegistryNotFound msg))

-- | 处理数据库错误
handleDatabaseError :: (Error MoongleError :> es) => Text -> Eff es a
handleDatabaseError msg = throwMoongleError (DatabaseErr (ConnectionFailed msg))

-- | 处理解析器错误
handleParserError :: (Error MoongleError :> es) => Text -> Eff es a
handleParserError msg = throwMoongleError (ParserErr (SyntaxError msg))

-- | 错误消息格式化
formatError :: MoongleError -> Text
formatError = \case
  RegistryErr err -> "Registry Error: " <> formatRegistryError err
  DatabaseErr err -> "Database Error: " <> formatDatabaseError err
  ParserErr err -> "Parser Error: " <> formatParserError err
  ConfigErr msg -> "Configuration Error: " <> msg
  NetworkErr msg -> "Network Error: " <> msg
  FileSystemErr msg -> "File System Error: " <> msg
  ValidationErr msg -> "Validation Error: " <> msg

formatRegistryError :: RegistryError -> Text
formatRegistryError = \case
  RegistryNotFound url -> "Registry not found at: " <> url
  RegistryParseError msg -> "Failed to parse registry: " <> msg
  PackageNotFound pkg -> "Package not found: " <> pkg
  PackageDownloadError msg -> "Failed to download package: " <> msg
  InvalidPackageStructure msg -> "Invalid package structure: " <> msg

formatDatabaseError :: DatabaseError -> Text
formatDatabaseError = \case
  ConnectionFailed msg -> "Failed to connect to database: " <> msg
  QueryFailed msg -> "Database query failed: " <> msg
  TransactionFailed msg -> "Database transaction failed: " <> msg
  SchemaError msg -> "Database schema error: " <> msg
  DataIntegrityError msg -> "Data integrity error: " <> msg

formatParserError :: ParserError -> Text
formatParserError = \case
  SyntaxError msg -> "Syntax error: " <> msg
  SemanticError msg -> "Semantic error: " <> msg
  TypeCheckError msg -> "Type check error: " <> msg
  UnsupportedConstruct msg -> "Unsupported construct: " <> msg