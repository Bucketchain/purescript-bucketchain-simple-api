module Test.Main where

import Prelude

import Bucketchain.Stream (convertToString)
import Bucketchain.Test (request, requestWithBody)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Foreign.Object (singleton, lookup, fromFoldable)
import Main (server)
import Node.HTTP (Server, listen, close)
import Node.HTTP.Client as C
import Test.Assert (assert)

main :: Effect Unit
main = do
  s <- server
  listen s opts $ runAff_ (handleAff s) do
    notPOSTTest
    notFoundTest
    successTest
    failureTest
    bodySuccessTest
    bodyFailureTest
    authSuccessTest
    authFailureTest
    errorTest
    batchTest
  where
    opts =
      { hostname: "localhost"
      , port: 3000
      , backlog: Nothing
      }

handleAff :: Server -> Either Error Unit -> Effect Unit
handleAff _ (Left err) = do
  log $ message err
  assert false
handleAff s _ = close s $ pure unit

notPOSTTest :: Aff Unit
notPOSTTest = do
  res <- request opts
  liftEffect $ assert $ C.statusCode res == 404
  where
    opts = C.port := 3000
        <> C.method := "GET"
        <> C.path := "/successTest"

notFoundTest :: Aff Unit
notFoundTest = do
  res <- request opts
  liftEffect $ assert $ C.statusCode res == 404
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/unknown"

successTest :: Aff Unit
successTest = do
  res <- requestWithBody opts "{\"test\":\"test\"}"
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "[{\"rawBody\":\"{\\\"test\\\":\\\"test\\\"}\",\"path\":\"successTest\",\"num\":777,\"name\":\"Item 1\",\"id\":1}]"
    assert $ C.statusCode res == 200
    assert $ Just "application/json; charset=utf-8" == (lookup "content-type" $ C.responseHeaders res)
    assert $ Just "CustomValue" == (lookup "x-custom" $ C.responseHeaders res)
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/successTest"
        <> C.headers := headers
    headers =
      C.RequestHeaders $ singleton "Content-Type" "application/json"

failureTest :: Aff Unit
failureTest = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "{\"core\":[\"This is error test\"]}"
    assert $ C.statusCode res == 503
    assert $ Just "CustomValue2" == (lookup "x-custom" $ C.responseHeaders res)
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/failureTest"

bodySuccessTest :: Aff Unit
bodySuccessTest = do
  res <- requestWithBody opts "{\"name\":\"test\"}"
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "{\"name\":\"test\"}"
    assert $ C.statusCode res == 201
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/bodyTest"
        <> C.headers := headers
    headers =
      C.RequestHeaders $ singleton "Content-Type" "application/json"

bodyFailureTest :: Aff Unit
bodyFailureTest = do
  res <- requestWithBody opts "{\"namae\":\"test\"}"
  liftEffect $ assert $ C.statusCode res == 400
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/bodyTest"
        <> C.headers := headers
    headers =
      C.RequestHeaders $ singleton "Content-Type" "application/json"

authSuccessTest :: Aff Unit
authSuccessTest = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "{\"name\":\"authuser\"}"
    assert $ C.statusCode res == 200
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/authTest"
        <> C.headers := headers
    headers =
      C.RequestHeaders $ singleton "X-Test-Auth" "authuser"

authFailureTest :: Aff Unit
authFailureTest = do
  res <- request opts
  liftEffect $ assert $ C.statusCode res == 401
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/authTest"

errorTest :: Aff Unit
errorTest = do
  res <- request opts
  liftEffect $ assert $ C.statusCode res == 500
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/errorTest"

batchTest :: Aff Unit
batchTest = do
  res <- requestWithBody opts batchBody
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "[{\"status\":503,\"headers\":{\"X-Custom\":\"CustomValue2\",\"content-type\":\"application/json; charset=utf-8\"},\"body\":{\"core\":[\"This is error test\"]}},{\"status\":201,\"headers\":{\"content-type\":\"application/json; charset=utf-8\"},\"body\":{\"name\":\"Other Item 1\"}},{\"status\":200,\"headers\":{\"content-type\":\"application/json; charset=utf-8\"},\"body\":{\"name\":\"authuser\"}},{\"status\":404,\"headers\":{\"Content-Type\":\"application/json; charset=utf-8\"},\"body\":null},{\"status\":500,\"headers\":{\"content-type\":\"application/json; charset=utf-8\"},\"body\":{\"message\":\"Internal server error\"}}]"
    assert $ C.statusCode res == 200
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/batch"
        <> C.headers := headers
    headers =
      C.RequestHeaders $ fromFoldable
        [ Tuple "Content-Type" "application/json"
        , Tuple "X-Test-Auth" "authuser"
        ]

batchBody :: String
batchBody = "[{\"path\":\"/failureTest\"},{\"path\":\"/bodyTest\",\"body\":{\"name\":\"Other Item 1\"}},{\"path\":\"/authTest\"},{\"path\":\"/notFound\"},{\"path\":\"/errorTest\"}]"
