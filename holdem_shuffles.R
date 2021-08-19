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
  deck_1 = deck_cut_at_prob(deck,1/4)
  deck_2 = deck_cut_at_prob(rest_of_deck(deck,deck_1),(1/3))
  deck_3 = deck_cut_at_prob(rest_of_deck(rest_of_deck(deck,deck_1),deck_2),(1/2))
  deck_4 = rest_of_deck(rest_of_deck(rest_of_deck(deck,deck_1),deck_2),deck_3)
  return(c(deck_4,deck_3,deck_2,deck_1))
}
strip_shuffle(1:52)

strip3 = function(deck){
  deck_1 = deck_cut_at_prob(deck,1/3)
  deck_2 = deck_cut_at_prob(rest_of_deck(deck,deck_1),(1/2))
  deck_3 = rest_of_deck(rest_of_deck(deck,deck_1),deck_2)
  return(c(deck_3,deck_2,deck_1))
}

print(strip3(1:52))

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


#takes about 2 seconds per shuffle move for an experienced dealer
#6 riffles and a cut takes 14 seconds
better_shuffle = function(deck){
  shuffled_deck = many_riffle_shuffles(1:52,7)
  return(shuffled_deck)
}
  
better_shuffle(1:52)

alt_shuffle = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt_shuffle(1:52)

alt_shuffle2 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,3)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt_shuffle2(1:52)

alt_shuffle3 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,4)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
alt_shuffle4 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,1)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,4)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
alt_shuffle5 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,5)
  shuffled_deck = strip_shuffle(shuffled_deck)

  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt_shuffle6 = function(deck){
  shuffled_deck = strip_shuffle(deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,5)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt_shuf = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,4)
  shuffled_deck = strip_shuffle(shuffled_deck)
  
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


sevriffs = function(deck){
  shuffled_deck = shuffle(deck)
  for (x in 1:6){
    shuffled_deck = shuffle(shuffled_deck)
  }
  return(shuffled_deck)
}

print(sevriffs(1:52))




