module Card exposing
    ( Card(..)
    , Rank(..)
    , Suit(..)
    , cardToString
    , createDeck
    , getSuit
    , prettyPrintCard
    , ranks
    , suits
    )


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Card
    = Card Suit Rank


suits : List Suit
suits =
    [ Hearts, Diamonds, Clubs, Spades ]


ranks : List Rank
ranks =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]



-- Example function to create a card


createCard : Suit -> Rank -> Card
createCard suit rank =
    Card suit rank


getSuit : Card -> Suit
getSuit (Card suit _) =
    suit



-- Function to create a deck of cards


createDeck : List Card
createDeck =
    List.concatMap (\suit -> List.map (\rank -> createCard suit rank) ranks) suits



-- Example function to display a card as a string


cardToString : Card -> String
cardToString (Card suit_ rank_) =
    let
        suitToString suit =
            case suit of
                Hearts ->
                    "Hearts"

                Diamonds ->
                    "Diamonds"

                Clubs ->
                    "Clubs"

                Spades ->
                    "Spades"

        rankToString rank =
            case rank of
                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "Jack"

                Queen ->
                    "Queen"

                King ->
                    "King"

                Ace ->
                    "Ace"
    in
    rankToString rank_ ++ " of " ++ suitToString suit_


prettyPrintCard : Card -> String
prettyPrintCard (Card suit_ rank_) =
    let
        suitToGlyph suit =
            case suit of
                Hearts ->
                    "♥"

                Diamonds ->
                    "♦"

                Clubs ->
                    "♣"

                Spades ->
                    "♠"

        rankToAbreviated rank =
            case rank of
                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"

                Ace ->
                    "A"
    in
    suitToGlyph suit_ ++ rankToAbreviated rank_
