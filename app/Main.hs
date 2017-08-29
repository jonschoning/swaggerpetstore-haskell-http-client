{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
-- import qualified Data.Aeson.Types as A

import qualified Network.HTTP.Client as NH

import qualified SwaggerPetstore as S

-- import Data.Foldable
import Data.Monoid

main :: IO ()
main = do
  let config = S.withStdoutLogging S.newConfig -- { S.configLoggingFilter = S.debugLevelFilter }
  mgr <- NH.newManager NH.defaultManagerSettings

  -- addPet
  let addPetRequest = S.addPet S.MimeJSON (S.mkPet "name" ["url1", "url2"])
  addPetResponse <- S.dispatchLbs mgr config addPetRequest S.MimeJSON
  let Just pet = A.decode (NH.responseBody addPetResponse)
      Just petId = S.petId pet

  -- deletePet
  let deletePetRequest = S.deletePet petId
  _ <- S.dispatchLbs mgr config deletePetRequest S.MimeJSON

  -- findPetsByStatus
  let findPetsByStatusRequest = S.findPetsByStatus ["available","pending","sold"]
  findPetsByStatusResult <- S.dispatchMime' mgr config findPetsByStatusRequest S.MimeJSON
  case findPetsByStatusResult of
    Left (S.MimeError e _) -> putStrLn $ "findPetsByStatusResult: " <> e
    Right r -> putStrLn $ "findPetsByStatusResult: found " <> (show . length) r <> " pets"
      
  
  return ()
