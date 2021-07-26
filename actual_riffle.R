library(tictoc)

#deck1 and deck2 are the decks created when the original deck is split
#according to way the cards are cut
decks1 = function(deck){
  cards_cut = rbinom(n =1, size = length(deck), prob = 1/2)
  
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

decks2 = function(deck,deck1){
  deck_2 = c()
  if (length(deck1) == length(deck)){
    return(deck_2)
  }
  for (x in (length(deck1)+1):length(deck)){
    deck_2 = append(deck_2,deck[x])
  }
  return(deck_2)
}

#this function performs 1 riffle shuffle.
#a deck is input, it is split accordingly, then riffled accordingly

riffle_shuffle = function(deck){
  
  deck_1 = decks1(deck)
  deck_2 = decks2(deck,deck_1)
  
  A = length(deck_1)
  B = length(deck_2)
  
  riffled_deck = vector()
  for (i in 1:length(deck)){
    random = runif(1)
    odds_left_heap_drop = A/(A+B)
    if (random < odds_left_heap_drop){
      card_added = deck_1[A]
      deck_1 = deck_1[-A]
      A = A - 1
    }else {
      card_added = deck_2[B]
      deck_2 = deck_2[-B]
      B = B -1
    }
    riffled_deck = c(card_added,riffled_deck)
  }
  return(riffled_deck)
}

#take in two variables, the starting deck i.e 1 to 52 and the desired number of shuffles. 
#This function returns the shuffled deck based on these metrics.

many_riffle_shuffles = function(starting_deck,amount_of_shuffles){
  shuffled_deck = riffle_shuffle(starting_deck)
  
  if (amount_of_shuffles == 1){
    return(shuffled_deck)
  }else if (amount_of_shuffles == 0){
    return(starting_deck)
  }
  
  else{
  
    for (x in 1:(amount_of_shuffles-1)){
      
      shuffled_deck = riffle_shuffle(shuffled_deck)
    
  }
  }
  return(shuffled_deck)
}
tic()
many_riffle_shuffles(1:52,7)
toc()

