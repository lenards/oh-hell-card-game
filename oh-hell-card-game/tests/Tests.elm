module Tests exposing (all)

import Card exposing (Card(..), Rank(..), Suit(..))
import CardEvaluator
import Expect
import List.Extra exposing (zip)
import Random exposing (Generator)
import Shuffler exposing (shuffleDeck)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Oh, Hell! Tests"
        [ basicHands
        , shuffler
        ]


basicHands : Test
basicHands =
    describe "finding the winner in a hand of cards"
        [ test "No cards in hand, no winner" <|
            \_ ->
                Expect.equal Nothing (CardEvaluator.determineTrickWinner Spades [])
        , test "No trump" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Two
                        , Card Hearts Three
                        , Card Hearts Four
                        , Card Hearts Five
                        ]
                in
                Expect.equal (Just <| Card Hearts Five) (CardEvaluator.determineTrickWinner Spades hand)
        , test "No trump, no out of suit card" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Two
                        , Card Hearts Three
                        , Card Clubs Four
                        , Card Hearts Five
                        ]
                in
                Expect.equal (Just <| Card Hearts Five) (CardEvaluator.determineTrickWinner Spades hand)
        , test "No trump, no out of suit card which is high card" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Two
                        , Card Hearts Three
                        , Card Hearts Four
                        , Card Clubs Five
                        ]
                in
                Expect.equal (Just <| Card Hearts Four) (CardEvaluator.determineTrickWinner Spades hand)
        , test "No trump, no one followed suit" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Jack
                        , Card Clubs Three
                        , Card Diamonds Four
                        , Card Clubs Five
                        ]
                in
                Expect.equal (Just <| Card Hearts Jack) (CardEvaluator.determineTrickWinner Spades hand)
        , test "Trumped hand" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Ten
                        , Card Hearts King
                        , Card Clubs Five
                        , Card Hearts Nine
                        ]
                in
                Expect.equal (Just <| Card Clubs Five) (CardEvaluator.determineTrickWinner Clubs hand)
        , test "Trumped hand, all suits" <|
            \_ ->
                let
                    hand =
                        [ Card Hearts Queen
                        , Card Diamonds Ace
                        , Card Clubs Five
                        , Card Spades Queen
                        ]
                in
                Expect.equal (Just <| Card Clubs Five) (CardEvaluator.determineTrickWinner Clubs hand)
        ]


shuffler : Test
shuffler =
    describe "Shuffler"
        [ testShuffleDeck
        , testMultipleShuffles
        ]


{-| Helper function to run a generator and get the result.
-}
runGenerator : Generator a -> a
runGenerator generator =
    Random.step generator (Random.initialSeed 42) |> Tuple.first


{-| Test to verify that the shuffled deck is different from the original deck.
-}
testShuffleDeck : Test
testShuffleDeck =
    test "Shuffled deck should be different from the original deck" <|
        \_ ->
            let
                originalDeck =
                    Card.createDeck

                shuffledDeck =
                    runGenerator (shuffleDeck originalDeck)
            in
            Expect.notEqual originalDeck shuffledDeck


{-| Calculate the Hamming distance between two lists.
The Hamming distance is the number of positions at which the corresponding elements are different.
-}
hammingDistance : List a -> List a -> Int
hammingDistance list1 list2 =
    List.foldl
        (\( a, b ) acc ->
            if a /= b then
                acc + 1

            else
                acc
        )
        0
        (zip list1 list2)


expectAll : List Bool -> Bool
expectAll =
    List.all identity


{-| Test to verify that multiple shuffles produce different results.
-}
testMultipleShuffles : Test
testMultipleShuffles =
    test "Multiple shuffles should produce different results" <|
        \_ ->
            let
                originalDeck =
                    Card.createDeck

                shuffledDeck1 =
                    runGenerator (shuffleDeck originalDeck)

                shuffledDeck2 =
                    runGenerator (shuffleDeck shuffledDeck1)

                distance1 =
                    hammingDistance originalDeck shuffledDeck1

                distance2 =
                    hammingDistance shuffledDeck1 shuffledDeck2
            in
            Expect.equal True (expectAll [ shuffledDeck1 /= shuffledDeck2, distance1 > 0, distance2 > 0 ])
