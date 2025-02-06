module CardEvaluator exposing (determineTrickWinner)

import Card exposing (Card(..), Rank(..), Suit(..))



-- Function to compare two cards
-- Function to compare two cards


{-| Compare the rank (value) of two cards.
-}
compareCards : Suit -> Suit -> Card -> Card -> Order
compareCards trump leadingSuit (Card suit1 rank1) (Card suit2 rank2) =
    if suit1 == suit2 then
        compare (rankToInt rank1) (rankToInt rank2)

    else if suit1 == trump then
        GT

    else if suit2 == trump then
        LT

    else if suit1 == leadingSuit then
        GT

    else if suit2 == leadingSuit then
        LT

    else
        EQ


{-| Function to determine the winner of a trick

If a hand has been trump, the trick winnner will be one of the trump cards

If a hand has not been trump, the trick winner will be one of the cards that followed suit

Any other cards can be ignored.

-}
determineTrickWinner : Suit -> List Card -> Maybe Card
determineTrickWinner trump cards =
    case cards of
        [] ->
            Nothing

        firstCard :: [] ->
            Just firstCard

        firstCard :: _ ->
            Just <| evaluateTrickCard trump firstCard cards


evaluateTrickCard : Suit -> Card -> List Card -> Card
evaluateTrickCard trump firstCard cards =
    let
        leadingSuit =
            Card.getSuit firstCard

        trumpCards =
            List.filter (\(Card suit _) -> suit == trump) cards

        followedSuitCards =
            List.filter (\(Card suit _) -> suit == leadingSuit) cards

        cardsToEvaluate =
            if List.isEmpty trumpCards then
                followedSuitCards

            else
                trumpCards
    in
    List.foldl (compareTrickCard trump leadingSuit) firstCard cardsToEvaluate


compareTrickCard : Suit -> Suit -> Card -> Card -> Card
compareTrickCard trump leadingSuit currentWinner card =
    case compareCards trump leadingSuit currentWinner card of
        GT ->
            currentWinner

        _ ->
            card



-- Function to get the Int value of a Card's Rank


rankToInt : Rank -> Int
rankToInt rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14



-- Function to get the Int value of a Card


cardToInt : Card -> Int
cardToInt (Card _ rank) =
    rankToInt rank
