open System
    
    type Suit = Heart | Club | Spade | Diamond
    
    type Rank =
        | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
        member this.ToInt =
            match this with
            | Two -> 2
            | Three -> 3
            | Four -> 4
            | Five -> 5
            | Six -> 6
            | Seven -> 7
            | Eight -> 8
            | Nine -> 9
            | Ten -> 10
            | Jack -> 11
            | Queen -> 12
            | King -> 13
            | Ace -> 14
    
    type HandValues =
        | RoyalFlush 
        | StraightFlush
        | FourOfAKind
        | FullHouse
        | Flush
        | Straight
        | ThreeOfAKind
        | TwoPair
        | Pair
        | HighCard
        
    type Card = Rank * Suit
    
    type DealtHand = Card list
    
    type CommunityCard =
        | Flop of Card list 
        | Turn of Card 
        | River of Card
    
    type Hand = DealtHand * CommunityCard
    
    type Deck = Card list
    
    type Player = {
        Name: string
        Hand: Card option
    }
    
    
    //type Game = {
    //    Deck: Deck
    //    Players: Player list 
    //}
    
    
    type Deal = Deck -> Deck * Card
    
    type ShuffleDeck = Deck -> Deck 
    
    type DrawCard = Deck -> Card -> Card list
    
    //type StartGame = Deck -> Player list -> Game
    
     
//    let suits = [ Heart; Diamond; Spade; Club ]
    
    let aceOfSpades: Card = (Ace,Spade)
    let kingOfSpades: Card = (King,Spade)
    let queenOfSpades: Card =  (Queen,Spade)
    let jackOfSpades: Card =  (Jack,Spade)
    let tenOfSpades: Card =  (Ten,Spade)
    let nineOfSpades: Card =  (Nine,Spade)
    let eightOfSpades: Card =  (Eight,Spade)
    let sevenOfSpades: Card =  (Seven,Spade)
    let sixOfSpades: Card =  (Six,Spade)
    let fiveOfSpades: Card =  (Five,Spade)
    let fourOfSpades: Card =  (Four,Spade)
    let threeOfSpades: Card =  (Three,Spade)
    let twoOfSpades: Card =  (Two,Spade) 
    let aceOfHearts: Card =  (Ace,Heart)
    let kingOfHearts: Card =  (King,Heart)
    let queenOfHearts: Card =  (Queen,Heart)
    let jackOfHearts: Card =  (Jack,Heart)
    let tenOfHearts: Card =  (Ten,Heart)
    let nineOfHearts: Card =  (Nine,Heart)
    let eightOfHearts: Card =  (Eight,Heart)
    let sevenOfHearts: Card =  (Seven,Heart)
    let sixOfHearts: Card =  (Six,Heart)
    let fiveOfHearts: Card =  (Five,Heart)
    let fourOfHearts: Card =  (Four,Heart)
    let threeOfHearts: Card =  (Three,Heart)
    let twoOfHearts: Card = (Two,Heart)
    let aceOfDiamonds: Card =  (Ace,Diamond)
    let kingOfDiamonds: Card =  (King,Diamond)
    let queenOfDiamonds: Card =  (Queen,Diamond)
    let jackOfDiamonds: Card =  (Jack,Diamond)
    let tenOfDiamonds: Card =  (Ten,Diamond)
    let nineOfDiamonds: Card =  (Nine,Diamond)
    let eightOfDiamonds: Card =  (Eight,Diamond)
    let sevenOfDiamonds: Card =  (Seven,Diamond)
    let sixOfDiamonds: Card =  (Six,Diamond)
    let fiveOfDiamonds: Card =  (Five,Diamond)
    let fourOfDiamonds: Card =  (Four,Diamond)
    let threeOfDiamonds: Card =  (Three,Diamond)
    let twoOfDiamonds: Card =  (Two,Diamond)
    let aceOfClubs: Card =  (Ace,Club)
    let kingOfClubs: Card =  (King,Club)
    let queenOfClubs: Card =  (Queen,Club)
    let jackOfClubs: Card =  (Jack,Club)
    let tenOfClubs: Card =  (Ten,Club)
    let nineOfClubs: Card =  (Nine,Club)
    let eightOfClubs: Card =  (Eight,Club)
    let sevenOfClubs: Card =  (Seven,Club)
    let sixOfClubs: Card =  (Six,Club)
    let fiveOfClubs: Card =  (Five,Club)
    let fourOfClubs: Card =  (Four,Club)
    let threeOfClubs: Card =  (Three,Club)
    let twoOfClubs: Card =  (Two,Club)
    
    let allSpades = [aceOfSpades;
                     kingOfSpades;
                     queenOfSpades;
                     jackOfSpades;
                     tenOfSpades;
                     nineOfSpades;
                     eightOfSpades;
                     sevenOfSpades;
                     sixOfSpades;
                     fiveOfSpades;
                     fourOfSpades;
                     threeOfSpades;
                     twoOfSpades
                     ]
    
    let allHearts = [aceOfHearts;
                     kingOfHearts;
                     queenOfHearts;
                     jackOfHearts;
                     tenOfHearts;
                     nineOfHearts;
                     eightOfHearts;
                     sevenOfHearts;
                     sixOfHearts;
                     fiveOfHearts;
                     fourOfHearts;
                     threeOfHearts;
                     twoOfHearts
                     ]
    
    let allDiamonds = [aceOfDiamonds;
                     kingOfDiamonds;
                     queenOfDiamonds;
                     jackOfDiamonds;
                     tenOfDiamonds;
                     nineOfDiamonds;
                     eightOfDiamonds;
                     sevenOfDiamonds;
                     sixOfDiamonds;
                     fiveOfDiamonds;
                     fourOfDiamonds;
                     threeOfDiamonds;
                     twoOfDiamonds
                     ]
    
    let allClubs = [aceOfClubs;
                     kingOfClubs;
                     queenOfClubs;
                     jackOfClubs;
                     tenOfClubs;
                     nineOfClubs;
                     eightOfClubs;
                     sevenOfClubs;
                     sixOfClubs;
                     fiveOfClubs;
                     fourOfClubs;
                     threeOfClubs;
                     twoOfClubs
                     ]
    
    let deck = allSpades @ allClubs @ allDiamonds @ allHearts
    
    let getRandomCard deck =
        let randomNumber = Random().Next(0,(deck |> List.length) - 1)
        deck.[randomNumber]
    
    let removeCardFromDeck deck card = deck |> List.filter(fun x -> not(x = card))
        
    let rec shuffleDeck newDeck deck =
        match deck with
        | [] -> newDeck
        | _ -> let card = getRandomCard deck
               removeCardFromDeck deck card 
               |> shuffleDeck ([card] @ newDeck)
               
    let rec deal deck players acc =
        match players with
        | [] -> acc
        | head::tail -> let card = getRandomCard deck
                        let remianingDeck = removeCardFromDeck deck card
                        deal remianingDeck tail [{head with Hand=Some card}] @ acc
    
    type Result<'TSuccess,'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure
        
    let bind switchfn twoInputTrack =
        match twoInputTrack with
        | Success s -> Success s
        | Failure f -> switchfn f
        
    let (>>=) twoInputTrack switchfn = bind switchfn twoInputTrack
        
    let hasDuplicateRank typeOfPair hand =
         let descCardOrder = hand |> List.sortBy fst |> List.rev
         let pairing = hand |> List.groupBy fst |> List.rev |> List.tryFind(fun (_,y) -> y.Length = typeOfPair) 
         match pairing with
         | None -> None 
         | Some pairing ->
             let pair = pairing |>  snd
             let descCardOrderWithoutPair = descCardOrder |> List.except pair
             descCardOrderWithoutPair.[0..(4 - typeOfPair)] @ pair  |> Some 
        
    let isFourOfAKind  = hasDuplicateRank 4  
    let isThreeOfAKind = hasDuplicateRank 3
    let isPair = hasDuplicateRank 2 
    
    let third (_,_,c) = c
    
    let (|Straight|_|) (hand: Card list) =
        let rankValues =
            hand |> List.sortBy fst |> List.map(fun (x,y)-> (x,y,x.ToInt))
        
        let splitIntoFives = rankValues |> List.windowed 5 |> List.rev
        
        let checkForStraight ranks =
            let lastRank = ranks |> List.last |> third 
            let firstRank = ranks |> List.head |>  third
            let differenceOfRank = lastRank - firstRank
            differenceOfRank = 4
        
        let hasStraight = splitIntoFives |> List.tryFind checkForStraight
        match hasStraight with
        | Some straightHand -> straightHand |> List.map (fun (x,y,_) -> (x,y)) |> Some
        | None -> None 
    
    let (|FourOfAKind|_|) =  isFourOfAKind  
    
    let (|ThreeOfAKind|_|) =   isThreeOfAKind
    
    let (|Pair|_|)  =  isPair  
    
    let (|Flush|_|) hand =
        let groupedBySuit = hand |> List.groupBy snd
        let hasFlush = groupedBySuit |> List.tryFind (fun (_,y) -> y.Length > 4)
        match hasFlush with 
        | Some flushHand ->
            let strongestFlush = flushHand |> snd |> List.sortBy fst |> List.rev
            strongestFlush.[..4] |> Some 
        | None -> None 
    
    let calcBesthand hand =
        match hand with
        | Straight hand & Flush _ when hand.[0] |> fst  = Ten -> (hand, RoyalFlush)
        | Straight hand & Flush _ -> (hand, StraightFlush)
        | FourOfAKind hand -> (hand, FourOfAKind)
        | ThreeOfAKind hand & Pair _ | ThreeOfAKind hand & ThreeOfAKind _-> (hand, FullHouse)
        | Flush hand -> (hand, Flush)
        | Straight hand -> (hand, Straight)
        | ThreeOfAKind hand -> (hand,ThreeOfAKind)
        | Pair hand & Pair _ -> (hand,TwoPair)
        | Pair hand -> (hand, Pair)
        | _ -> let descCardOrder = hand |> List.sortBy fst
               (descCardOrder.[0..4],HighCard)
