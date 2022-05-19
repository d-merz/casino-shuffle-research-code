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

strip_shuffle = function(deck){
    A = rbinom(n =1, size = length(deck), prob = 1/4)
    B = rbinom(n =1, size = (length(deck)-A), prob = 1/3)
    C = rbinom(n =1, size = (length(deck)-A-B), prob = 1/2)
    D = length(deck) - C - B - A
    strip = c(tail(deck,D),deck[(A+B+1):(A+B+C)],deck[(A+1):(A+B)],deck[1:A])
    return(strip)
  }
  
strip_shuffle(1:52)

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



alt_shuf = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,4)
  shuffled_deck = strip_shuffle(shuffled_deck)
  
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


#CS
holdem_shuffle()
#2 riffle, strip, 2 riffle, cut
ex_shuf1() = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
#3 riffle, strip, 1 riffle, cut
ex_shuf2() = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
#3 riffle, strip, 2 riffle, cut
ex_shuf3() = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
#7 riffles







