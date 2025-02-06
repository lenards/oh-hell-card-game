module Shuffler exposing (shuffleDeck)

import Card exposing (Card)
import Random exposing (Generator)
import Random.List exposing (shuffle)


{-| Shuffles a deck of cards using the Fisher-Yates algorithm.
This function returns a `Generator` that can be used to produce a shuffled list of cards.
-}
shuffleDeck : List Card -> Generator (List Card)
shuffleDeck deck =
    shuffle deck
