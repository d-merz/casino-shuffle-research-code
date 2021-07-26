#riffle, riffle, strip, riffle, cut, then deal


#returns a deck cut at a certain probability
deck_cut_at_prob = function(deck,cut_odds){
  z = cut_odds
  cards_cut = rbinom(n =1, size = length(deck), prob = z)
  
  if (cards_cut == length(deck)){
    return(deck)
  }
  deck_1 = c()
  for (x in 1:length(deck)){
    if (x<= cards_cut){
      deck_1 = append(deck_1,deck[x]) 
    }
  }
  return(deck_1)
}

#returns the rest of the deck
rest_of_deck = function(deck,deck1){
  deck_2 = c()
  if (length(deck1) == length(deck)){
    return(deck_2)
  }
  for (x in (length(deck1)+1):length(deck)){
    deck_2 = append(deck_2,deck[x])
  }
  return(deck_2)
}

#strip shuffles a deck of cards
strip_shuffle = function(deck){
  deck_1 = deck_cut_at_prob(deck,1/3)
  deck_2 = deck_cut_at_prob(rest_of_deck(deck,deck_1),(1/2))
  deck_3 = rest_of_deck(rest_of_deck(deck,deck_1),deck_2)
  return(c(deck_3,deck_2,deck_1))
}

#cuts a deck of cards
deck_cut = function(deck){
  cut_pos = rbinom(n =1, size = length(deck), prob = 1/2)
  cut_deck = c(deck[(cut_pos+1):length(deck)],deck[1:cut_pos])
  return(cut_deck)
}


#runs the shuffle commonly seen at casinos when playing texas holdem
holdem_shuffle = function(deck){
  shuffled_deck = many_riffle_shuffles(1:52,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
holdem_shuffle(1:52)


