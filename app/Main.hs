{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-orphans #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Lens.Micro as L
import qualified Data.Text as T
import qualified Network.HTTP.Client as NH

import qualified SwaggerPetstore as S

import Data.Monoid ((<>))

main :: IO ()
main = do
  mgr <- NH.newManager NH.defaultManagerSettings
  let config = S.withStdoutLogging S.newConfig -- { S.configLoggingFilter = S.debugLevelFilter }

  putStrLn "******** CONFIG ********"
  putStrLn (show config)

  putStrLn "******** Pet operations ********"
  runPet mgr config

  putStrLn "******** Store operations ********"
  runStore mgr config

  putStrLn "******** User operations ********"
  runUser mgr config

  putStrLn "******** END ********"

  return ()

runPet :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runPet mgr config = do
  -- addPet
  let addPetRequest = S.addPet S.MimeJSON (S.mkPet "name" ["url1", "url2"])
  addPetResponse <- S.dispatchLbs mgr config addPetRequest S.MimeJSON
  let Just pet = A.decode (NH.responseBody addPetResponse)
      Just petId = S.petId pet

  -- getPetByid
  let getPetByIdRequest = S.getPetById petId
  getPetByIdRequestResult <- S.dispatchMime mgr config getPetByIdRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getPetById: found pet: " <> show r) getPetByIdRequestResult 

  -- findPetsByStatus
  let findPetsByStatusRequest = S.findPetsByStatus ["available","pending","sold"]
  findPetsByStatusResult <- S.dispatchMime mgr config findPetsByStatusRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "findPetsByStatus: found " <> (show . length) r <> " pets") findPetsByStatusResult 
      
  -- findPetsByTags
  let findPetsByTagsRequest = S.findPetsByTags ["name","tag1"]
  findPetsByTagsResult <- S.dispatchMime mgr config findPetsByTagsRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "findPetsByTags: found " <> (show . length) r <> " pets") findPetsByTagsResult 

  -- updatePet
  let updatePetRequest = S.updatePet S.MimeJSON $ pet
        { S.petStatus   = Just "available"
        , S.petCategory = Just (S.Category (Just 3) (Just "catname"))
        }
  _ <- S.dispatchLbs mgr config updatePetRequest S.MimeXML
  
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
  mapM_ (\r -> putStrLn $ "uploadFile: " <> show r) uploadFileRequestResult 

  -- deletePet
  let deletePetRequest = S.deletePet petId
        `S.applyOptionalParam` S.ApiUnderscorekey "api key"
  _ <- S.dispatchLbs mgr config deletePetRequest S.MimeJSON

  return ()

-- declare that 'placeOrder' can recieve a JSON content-type request
instance S.Consumes S.PlaceOrder S.MimeJSON 

runStore :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runStore mgr config = do

  -- getInventory
  let getInventoryRequest = S.getInventory
  getInventoryRequestRequestResult <- S.dispatchMime mgr config getInventoryRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getInventoryRequest: found " <> (show . length) r <> " results") getInventoryRequestRequestResult

  -- placeOrder
  -- now <- TI.getCurrentTime
  let placeOrderRequest = S.placeOrder S.MimeJSON (S.mkOrder { S.orderId = Just 21, S.orderQuantity = Just 210 }) --, S.orderShipDate = Just now})
  placeOrderResult <- S.dispatchMime mgr config placeOrderRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "placeOrderResult: " <> show r) placeOrderResult

  let orderId = maybe 10 id $ either (const Nothing) (S.orderId) (S.mimeResult placeOrderResult)

  -- getOrderByid
  let getOrderByIdRequest = S.getOrderById orderId
  getOrderByIdRequestResult <- S.dispatchMime mgr config getOrderByIdRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getOrderById: found order: " <> show r) getOrderByIdRequestResult 

  -- deleteOrder
  let deleteOrderRequest = S.deleteOrder 2
  _ <- S.dispatchLbs mgr config deleteOrderRequest S.MimeJSON

  return ()


instance S.Consumes S.CreateUser S.MimeJSON
instance S.Consumes S.UpdateUser S.MimeJSON

instance S.Consumes S.CreateUsersWithArrayInput S.MimeJSON
instance S.Consumes S.CreateUsersWithListInput S.MimeJSON
instance S.Produces S.CreateUsersWithArrayInput S.MimeNoContent
instance S.Produces S.CreateUsersWithListInput S.MimeNoContent

runUser :: NH.Manager -> S.SwaggerPetstoreConfig -> IO ()
runUser mgr config = do

  let username = "hsusername"
  -- createUser
  let user = S.mkUser { S.userId = Just 21, S.userUsername = Just username } 
  let createUserRequest = S.createUser S.MimeJSON user
  _ <- S.dispatchLbs mgr config createUserRequest S.MimeJSON

  -- createUsersWithArrayInput
  let users = take 8 $ drop 1 $ iterate (L.over S.userUsernameT (<> "*") . L.over S.userIdT (+1)) user
  let createUsersWithArrayInputRequest = S.createUsersWithArrayInput S.MimeJSON users
  _ <- S.dispatchLbs mgr config createUsersWithArrayInputRequest S.MimeNoContent

  -- createUsersWithArrayInput
  let createUsersWithListInputRequest = S.createUsersWithListInput S.MimeJSON users
  _ <- S.dispatchLbs mgr config createUsersWithListInputRequest S.MimeNoContent

  -- getUserByName
  let getUserByNameRequest = S.getUserByName username
  getUserByNameResult <- S.dispatchMime mgr config getUserByNameRequest S.MimeJSON
  mapM_ (\r -> putStrLn $ "getUserByName: found user: " <> show r) getUserByNameResult 

  -- loginUser
  let loginUserRequest = S.loginUser username "password1"
  loginUserResult <- S.dispatchLbs mgr config loginUserRequest S.MimeJSON
  BCL.putStrLn $ "loginUser: " <> (NH.responseBody loginUserResult)

  -- updateUser
  let updateUserRequest = S.updateUser S.MimeJSON username (user { S.userEmail = Just "xyz@example.com" })
  _ <- S.dispatchLbs mgr config updateUserRequest S.MimeJSON

  -- logoutUser
  _ <- S.dispatchLbs mgr config S.logoutUser S.MimeJSON
  
  -- deleteUser
  let deleteUserRequest = S.deleteUser username
  _ <- S.dispatchLbs mgr config deleteUserRequest S.MimeJSON

  return ()
