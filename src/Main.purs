module Main where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
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

type SimpleGraphQLSchema = NonEmpty Array String

myAwesomeQueryGenerator :: SimpleGraphQLSchema -> Gen String
myAwesomeQueryGenerator schema = do
  myCommand <- oneOf $ map pure schema
  pure ("query { " <> myCommand <> " }")

genGraphQLQuery1 :: SimpleGraphQLSchema -> Gen GraphQLQuery
genGraphQLQuery1 schema = do
  query <- myAwesomeQueryGenerator schema
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
  let result = quickCheckPure (mkSeed 0) 100 (genGraphQlQueryTest (genGraphQLQuery1 (NonEmpty "a" ["b", "c"])))
  log $ show result
