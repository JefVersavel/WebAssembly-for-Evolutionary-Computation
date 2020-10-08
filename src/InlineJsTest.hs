import Data.Aeson
import GHC.Generics
import Language.JavaScript.Inline

data DNSRecord = DNSRecord
  { address :: String
  , family :: Int
  } deriving (FromJSON, Generic, Show)

dnsLookup :: String -> IO [DNSRecord]
dnsLookup hostname =
  withJSSession
    defJSSessionOpts
    [block|
        const dns = (await import("dns")).promises;
        return dns.lookup($hostname, {all: true});
    |]
