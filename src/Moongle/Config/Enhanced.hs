{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.Config.Enhanced 
  ( Config (..)
  , DatabaseConfig (..)
  , RegistryConfig (..)
  , LoggingConfig (..)
  , ServerConfig (..)
  , defaultConfig
  , loadConfigFromFile
  , validateConfig
  , configFromEnv
  -- Lenses
  , database
  , registry
  , logging
  , server
  , dbHost
  , dbPort
  , dbName
  , dbUser
  , dbPassword
  , registryUrl
  , mooncakesBaseUrl
  , moongleStoragePath
  , parallel
  , logLevel
  , logFormat
  , serverHost
  , serverPort
  ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Moongle.Error

-- | 数据库配置
data DatabaseConfig = DatabaseConfig
  { _dbHost :: Text
  , _dbPort :: Int
  , _dbName :: Text
  , _dbUser :: Text
  , _dbPassword :: Text
  } deriving (Show, Generic)

instance FromJSON DatabaseConfig where
  parseJSON = withObject "DatabaseConfig" $ \o -> DatabaseConfig
    <$> o .: "host"
    <*> o .: "port"
    <*> o .: "name"
    <*> o .: "user"
    <*> o .: "password"

instance ToJSON DatabaseConfig where
  toJSON DatabaseConfig{..} = object
    [ "host" .= _dbHost
    , "port" .= _dbPort
    , "name" .= _dbName
    , "user" .= _dbUser
    , "password" .= _dbPassword
    ]

-- | 注册表配置
data RegistryConfig = RegistryConfig
  { _registryUrl :: Text
  , _mooncakesBaseUrl :: Text
  , _moongleStoragePath :: Text
  , _parallel :: Int
  } deriving (Show, Generic)

instance FromJSON RegistryConfig where
  parseJSON = withObject "RegistryConfig" $ \o -> RegistryConfig
    <$> o .: "registryUrl"
    <*> o .: "mooncakesBaseUrl"
    <*> o .: "moongleStoragePath"
    <*> o .: "parallel"

instance ToJSON RegistryConfig where
  toJSON RegistryConfig{..} = object
    [ "registryUrl" .= _registryUrl
    , "mooncakesBaseUrl" .= _mooncakesBaseUrl
    , "moongleStoragePath" .= _moongleStoragePath
    , "parallel" .= _parallel
    ]

-- | 日志配置
data LoggingConfig = LoggingConfig
  { _logLevel :: Text
  , _logFormat :: Text
  } deriving (Show, Generic)

instance FromJSON LoggingConfig where
  parseJSON = withObject "LoggingConfig" $ \o -> LoggingConfig
    <$> o .: "level"
    <*> o .: "format"

instance ToJSON LoggingConfig where
  toJSON LoggingConfig{..} = object
    [ "level" .= _logLevel
    , "format" .= _logFormat
    ]

-- | 服务器配置
data ServerConfig = ServerConfig
  { _serverHost :: Text
  , _serverPort :: Int
  } deriving (Show, Generic)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> ServerConfig
    <$> o .: "host"
    <*> o .: "port"

instance ToJSON ServerConfig where
  toJSON ServerConfig{..} = object
    [ "host" .= _serverHost
    , "port" .= _serverPort
    ]

-- | 主配置类型
data Config = Config
  { _database :: DatabaseConfig
  , _registry :: RegistryConfig
  , _logging :: LoggingConfig
  , _server :: ServerConfig
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "database"
    <*> o .: "registry"
    <*> o .: "logging"
    <*> o .: "server"

instance ToJSON Config where
  toJSON Config{..} = object
    [ "database" .= _database
    , "registry" .= _registry
    , "logging" .= _logging
    , "server" .= _server
    ]

-- 生成透镜
makeLenses ''DatabaseConfig
makeLenses ''RegistryConfig
makeLenses ''LoggingConfig
makeLenses ''ServerConfig
makeLenses ''Config

-- | 默认配置
defaultConfig :: Config
defaultConfig = Config
  { _database = DatabaseConfig
      { _dbHost = "localhost"
      , _dbPort = 5432
      , _dbName = "moongle"
      , _dbUser = "postgres"
      , _dbPassword = ""
      }
  , _registry = RegistryConfig
      { _registryUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user"
      , _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com"
      , _moongleStoragePath = "./storage"
      , _parallel = 32
      }
  , _logging = LoggingConfig
      { _logLevel = "info"
      , _logFormat = "text"
      }
  , _server = ServerConfig
      { _serverHost = "0.0.0.0"
      , _serverPort = 8080
      }
  }

-- | 从文件加载配置
loadConfigFromFile :: MonadIO m => FilePath -> m (Either MoongleError Config)
loadConfigFromFile path = liftIO $ do
  result <- Yaml.decodeFileEither path
  case result of
    Left parseErr -> pure $ Left $ ConfigErr $ "Failed to parse config file: " <> T.pack (show parseErr)
    Right config -> pure $ Right config

-- | 验证配置
validateConfig :: Config -> Either MoongleError Config
validateConfig config = do
  validateDatabaseConfig (config ^. database)
  validateRegistryConfig (config ^. registry)
  validateServerConfig (config ^. server)
  pure config
  where
    validateDatabaseConfig dbConfig = do
      when (T.null (dbConfig ^. dbHost)) $ 
        Left $ ConfigErr "Database host cannot be empty"
      when (dbConfig ^. dbPort <= 0) $ 
        Left $ ConfigErr "Database port must be positive"
      when (T.null (dbConfig ^. dbName)) $ 
        Left $ ConfigErr "Database name cannot be empty"

    validateRegistryConfig regConfig = do
      when (T.null (regConfig ^. registryUrl)) $ 
        Left $ ConfigErr "Registry URL cannot be empty"
      when (regConfig ^. parallel <= 0) $ 
        Left $ ConfigErr "Parallel jobs must be positive"

    validateServerConfig srvConfig = do
      when (T.null (srvConfig ^. serverHost)) $ 
        Left $ ConfigErr "Server host cannot be empty"
      when (srvConfig ^. serverPort <= 0) $ 
        Left $ ConfigErr "Server port must be positive"

-- | 从环境变量加载配置（覆盖默认值）
configFromEnv :: MonadIO m => Config -> m Config
configFromEnv baseConfig = liftIO $ do
  dbHost' <- maybe (baseConfig ^. database . dbHost) T.pack <$> lookupEnv "MOONGLE_DB_HOST"
  dbPort' <- maybe (baseConfig ^. database . dbPort) read <$> lookupEnv "MOONGLE_DB_PORT"
  dbName' <- maybe (baseConfig ^. database . dbName) T.pack <$> lookupEnv "MOONGLE_DB_NAME"
  dbUser' <- maybe (baseConfig ^. database . dbUser) T.pack <$> lookupEnv "MOONGLE_DB_USER"
  dbPassword' <- maybe (baseConfig ^. database . dbPassword) T.pack <$> lookupEnv "MOONGLE_DB_PASSWORD"
  
  registryUrl' <- maybe (baseConfig ^. registry . registryUrl) T.pack <$> lookupEnv "MOONGLE_REGISTRY_URL"
  parallel' <- maybe (baseConfig ^. registry . parallel) read <$> lookupEnv "MOONGLE_PARALLEL"
  
  serverHost' <- maybe (baseConfig ^. server . serverHost) T.pack <$> lookupEnv "MOONGLE_SERVER_HOST"
  serverPort' <- maybe (baseConfig ^. server . serverPort) read <$> lookupEnv "MOONGLE_SERVER_PORT"
  
  pure $ baseConfig
    & database . dbHost .~ dbHost'
    & database . dbPort .~ dbPort'
    & database . dbName .~ dbName'
    & database . dbUser .~ dbUser'
    & database . dbPassword .~ dbPassword'
    & registry . registryUrl .~ registryUrl'
    & registry . parallel .~ parallel'
    & server . serverHost .~ serverHost'
    & server . serverPort .~ serverPort'