module Main where

import Prelude

import Data.HashMap (HashMap, empty, singleton)
import Data.NonEmpty (NonEmpty(..))
import Data.String (length)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.QuickCheck (Result(..), arbitrary, mkSeed, quickCheck, quickCheckPure)
import Test.QuickCheck.Gen (Gen, oneOf)

-- https://pursuit.purescript.org/packages/purescript-ordered-collections/1.6.1/docs/Data.Map

newtype GraphQLQuery = GraphQLQuery {
  query :: String,
  variables :: HashMap String String,
  operationName :: String
}

myAwesomeQueryGenerator :: Gen String
myAwesomeQueryGenerator = do
  myCommand <- oneOf $ NonEmpty (pure "foo") (map pure ["bar", "baz"])
  pure ("query { " <> myCommand <> " }")

genGraphQLQuery1 :: Gen GraphQLQuery
genGraphQLQuery1 = do
  query <- myAwesomeQueryGenerator
  variables <- pure (singleton "foo" "bar")
  operationName <- arbitrary
  pure $ GraphQLQuery { operationName, variables, query }

genGraphQLQuery2 :: Gen GraphQLQuery
genGraphQLQuery2 = do
  query <- arbitrary
  variables <- pure empty
  operationName <- arbitrary
  pure $ GraphQLQuery { operationName, variables, query: query <> "foobar" }

genGraphQlQueryTest :: Gen GraphQLQuery -> Gen Result
genGraphQlQueryTest mGen = do
  (GraphQLQuery query) <- mGen
  if (false) then pure Success else pure $ Failed (show query)

main :: Effect Unit
main = do
  let result = quickCheckPure (mkSeed 0) 100 (genGraphQlQueryTest genGraphQLQuery1)
  log $ show result
