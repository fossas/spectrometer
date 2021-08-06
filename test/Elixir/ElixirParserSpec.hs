module Elixir.ElixirParserSpec (
  spec,
) where

import Strategy.Elixir.ElixirParser (
  ElixirAccessor (..),
  ElixirAtom (ElixirAtom),
  ElixirBool (ElixirBool),
  ElixirKeyword (..),
  ElixirText (..),
  ElixirValue (..),
  findKeyword,
  findKeywordWithStringValue,
  parseElixirValue,
 )

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, parse)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> ElixirValue -> Expectation
shouldParseInto = parseMatch parseElixirValue

accessorOf :: Text -> ElixirAccessor -> Bool
accessorOf accessorQuery x = getAccessor x == accessorQuery

spec :: Spec
spec = do
  describe "parseElixirValue" $ do
    it "should parse elixir atom" $ do
      ":hey" `shouldParseInto` Atom (ElixirAtom "hey")
      ":hello" `shouldParseInto` Atom (ElixirAtom "hello")
      ":goodbye" `shouldParseInto` Atom (ElixirAtom "goodbye")

    it "should parse elixir string" $ do
      "\"hippity\"" `shouldParseInto` String (ElixirText "hippity")
      "\"hoppity:\"" `shouldParseInto` String (ElixirText "hoppity:")
      "\"h o p  \"" `shouldParseInto` String (ElixirText "h o p  ")

    it "should parse elixir boolean" $ do
      ":true" `shouldParseInto` Bool (ElixirBool True)
      "true" `shouldParseInto` Bool (ElixirBool True)
      ":false" `shouldParseInto` Bool (ElixirBool False)
      "false" `shouldParseInto` Bool (ElixirBool False)

    it "should parse elixir keyword" $ do
      "status: :true" `shouldParseInto` Keyword (ElixirKeyword (ElixirAccessor "status") (Bool $ ElixirBool True))
      "status: false" `shouldParseInto` Keyword (ElixirKeyword (ElixirAccessor "status") (Bool $ ElixirBool False))
      "msg: \"hello\"" `shouldParseInto` Keyword (ElixirKeyword (ElixirAccessor "msg") (String $ ElixirText "hello"))
      "status: :ok" `shouldParseInto` Keyword (ElixirKeyword (ElixirAccessor "status") (Atom $ ElixirAtom "ok"))

    it "should parse elixir accessor" $ do
      "hungry:" `shouldParseInto` Accessor (ElixirAccessor "hungry")
      "hippo:" `shouldParseInto` Accessor (ElixirAccessor "hippo")

    it "should parse elixir list" $ do
      "[:we, true]"
        `shouldParseInto` List
          [ Atom (ElixirAtom "we")
          , Bool (ElixirBool True)
          ]
      "[:we, \"live half in the day time\"]"
        `shouldParseInto` List
          [ Atom (ElixirAtom "we")
          , String (ElixirText "live half in the day time")
          ]
      "[watch: :things, on: :vcr]"
        `shouldParseInto` List
          [ Keyword (ElixirKeyword (ElixirAccessor "watch") (Atom $ ElixirAtom "things"))
          , Keyword (ElixirKeyword (ElixirAccessor "on") (Atom $ ElixirAtom "vcr"))
          ]

    it "should parse elixir tuple" $ do
      "{with: :me, \"and talk all night\"}"
        `shouldParseInto` Tuple
          [ Keyword (ElixirKeyword (ElixirAccessor "with") (Atom $ ElixirAtom "me"))
          , String (ElixirText "and talk all night")
          ]
      "{:pkg, [:dev]}"
        `shouldParseInto` Tuple
          [ Atom (ElixirAtom "pkg")
          , List
              [ Atom (ElixirAtom "dev")
              ]
          ]

  describe "findKeyword" $ do
    it "should retrieve value of the accessor only" $
      findKeyword (accessorOf "error") [Atom (ElixirAtom "error")] `shouldBe` Nothing

    it "should retrieve value of the accessor" $ do
      findKeyword
        (accessorOf "msg")
        [ Atom (ElixirAtom "ok")
        , Keyword (ElixirKeyword (ElixirAccessor "msg") (Bool $ ElixirBool True))
        ]
        `shouldBe` Just (Bool $ ElixirBool True)

    it "should retrieve value of the accessor in nested list" $ do
      findKeyword
        (accessorOf "msg")
        [ Atom (ElixirAtom "ok")
        , Keyword
            ( ElixirKeyword
                (ElixirAccessor "data")
                ( List
                    [ Keyword (ElixirKeyword (ElixirAccessor "http") (Atom $ ElixirAtom "201"))
                    , Keyword (ElixirKeyword (ElixirAccessor "msg") (Atom $ ElixirAtom "msg-that-is-found"))
                    ]
                )
            )
        ]
        `shouldBe` Just (Atom $ ElixirAtom "msg-that-is-found")

    it "should retrieve value of the accessor in nested tuple" $ do
      findKeyword
        (accessorOf "msg")
        [ Atom (ElixirAtom "ok")
        , Keyword
            ( ElixirKeyword
                (ElixirAccessor "data")
                ( Tuple
                    [ Keyword (ElixirKeyword (ElixirAccessor "http") (Atom $ ElixirAtom "201"))
                    , Keyword (ElixirKeyword (ElixirAccessor "msg") (Atom $ ElixirAtom "32"))
                    ]
                )
            )
        ]
        `shouldBe` Just (Atom $ ElixirAtom "32")

    it "should not retrieve value if not matched" $ do
      findKeyword
        (accessorOf "msg-that-does-not-exist")
        [ Atom (ElixirAtom "ok")
        , Keyword
            ( ElixirKeyword
                (ElixirAccessor "data")
                ( Tuple
                    [ Keyword (ElixirKeyword (ElixirAccessor "http") (Atom $ ElixirAtom "201"))
                    , Keyword (ElixirKeyword (ElixirAccessor "msg") (Atom $ ElixirAtom "32"))
                    ]
                )
            )
        ]
        `shouldBe` Nothing

  describe "findKeywordWithStringValue" $ do
    it "should retrieve value of the accessor only" $
      findKeywordWithStringValue [Atom (ElixirAtom "error")] "error" `shouldBe` Nothing

    it "should not retrieve value of the accessor when corresponding value is not string" $ do
      findKeywordWithStringValue
        [ Atom (ElixirAtom "ok")
        , Keyword (ElixirKeyword (ElixirAccessor "msg") (Bool $ ElixirBool True))
        ]
        "msg"
        `shouldBe` Nothing

    it "should retrieve value of the accessor only when corresponding value is string" $ do
      findKeywordWithStringValue
        [ Atom (ElixirAtom "ok")
        , Keyword (ElixirKeyword (ElixirAccessor "msg") (String (ElixirText "some-description")))
        ]
        "msg"
        `shouldBe` Just ("some-description" :: Text)
