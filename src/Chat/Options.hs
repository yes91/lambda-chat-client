module Chat.Options
  ( Options(..)
  , getOpts
  ) where

import Network.Socket (HostName, ServiceName)

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { host      :: HostName
  , port      :: ServiceName }

options :: Parser Options
options = Options
      <$> argument str
          ( metavar "HOSTNAME"
         <> help "The host name of the chat server. Can be an IP address or a domain name." )
      <*> option str
          ( long "port"
         <> short 'p'
         <> help "The port at which the chat service is running."
         <> showDefault
         <> value "3000"
         <> metavar "PORT" )

fullOpts :: ParserInfo Options
fullOpts = info (options <**> helper)
         ( fullDesc
        <> progDesc "Connect to a chat server at HOSTNAME"
        <> header "lambda-chat - a Haskell client for the lambda-chat protocol." )

getOpts :: IO Options
getOpts = execParser fullOpts
