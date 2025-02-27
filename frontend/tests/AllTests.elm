module AllTests exposing (..)

import ApiTests
import FormTests
import IntegrationTests
import MainTests
import PortsTests
import RouteTests
import Test exposing (..)
import TypesTests
import ViewTests


{-
   Main test runner that combines all the test modules
   Run with: npx elm-test
-}


all : Test
all =
    describe "URL Shortener Tests"
        [ ApiTests.suite
        , RouteTests.suite
        , TypesTests.suite
        , MainTests.suite
        , ViewTests.suite
        , PortsTests.suite
        , FormTests.suite
        , IntegrationTests.suite
        ]