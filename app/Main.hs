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

  -- getPetByid
  let getPetByIdRequest = S.getPetById petId
  getPetByIdRequestResult <- S.dispatchMime mgr config getPetByIdRequest S.MimeJSON
  case S.mimeResult getPetByIdRequestResult of
    Right r -> putStrLn $ "getPetById: found pet: " <> show r
    _ -> return ()

  -- findPetsByStatus
  let findPetsByStatusRequest = S.findPetsByStatus ["available","pending","sold"]
  findPetsByStatusResult <- S.dispatchMime mgr config findPetsByStatusRequest S.MimeJSON
  case S.mimeResult findPetsByStatusResult of
    Right r -> putStrLn $ "findPetsByStatus: found " <> (show . length) r <> " pets"
    _ -> return ()
      
  -- findPetsByTags
  let findPetsByTagsRequest = S.findPetsByTags ["name","tag1"]
  findPetsByTagsResult <- S.dispatchMime mgr config findPetsByTagsRequest S.MimeJSON
  case S.mimeResult findPetsByTagsResult of
    Right r -> putStrLn $ "findPetsByTags: found " <> (show . length) r <> " pets"
    _ -> return ()

  -- updatePet
  let updatePetRequest = S.updatePet S.MimeJSON $ pet
        { S.petStatus   = Just "available"
        , S.petCategory = Just (S.Category (Just 3) (Just "catname"))
        }
  _ <- S.dispatchLbs mgr config updatePetRequest S.MimeJSON
  
  -- updatePetWithForm
  let updatePetWithFormRequest = S.updatePetWithForm S.MimeFormUrlEncoded petId
        `S.applyOptionalParam` S.Name "petName"
        `S.applyOptionalParam` S.Status "pending"
  _ <- S.dispatchLbs mgr config updatePetWithFormRequest S.MimeJSON

  -- uploadFile
  let uploadFileRequest = S.uploadFile S.MimeMultipartFormData petId
        `S.applyOptionalParam` S.File "package.yaml"
        `S.applyOptionalParam` S.AdditionalMetadata "a package.yaml file"
  uploadFileRequestResult <- S.dispatchMime mgr config uploadFileRequest S.MimeJSON
  case S.mimeResult uploadFileRequestResult of
    Right r -> putStrLn $ "uploadFile: " <> show r
    _ -> return ()

  -- getInventory
  let getInventoryRequest = S.getInventory
  getInventoryRequestRequestResult <- S.dispatchMime mgr config getInventoryRequest S.MimeJSON
  case S.mimeResult getInventoryRequestRequestResult of
    Right r -> putStrLn $ "getInventoryRequest: found " <> (show . length) r <> " results"
    _ -> return ()


  -- deletePet
  let deletePetRequest = S.deletePet petId
  _ <- S.dispatchLbs mgr config deletePetRequest S.MimeJSON

  -- deleteOrder
  let deleteOrderRequest = S.deleteOrder 2
  _ <- S.dispatchLbs mgr config deleteOrderRequest S.MimeJSON

  return ()
