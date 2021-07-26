library(tictoc)

#odds of a cards position before the shuffle ending up in a certain
#position of the deck
#i.e. the first card in the deck before shuffle has about 3% chance of being
#the third card in the deck after the holdem shuffle is performed
holdem_odds = function(sims,shuffle){
  list_card_count = list()
  vect = integer(52)
  for (x in 1:52){
    list_card_count = append(list_card_count,list(vect))
  }
  for(x in 1:sims){
    deck = shuffle(1:52)
    for(card in 1:52){
      position = match(card,deck)
      list_card_count[[card]][position] = list_card_count[[card]][position] + 1
    }
  }
  percent_list = list()
  for(vec in list_card_count){
    percent_vec = (vec/sims) * 100
    percent_list = append(percent_list,list(percent_vec))
  }
  return(percent_list)
}

tic()
y = holdem_odds(10000,holdem_shuffle)
print(y)
toc()


#odds of a player in a position to get a card based on its position in the deck before shuffle
#i.e the second player dealt has about a 3.5% chance that their first card was 
#the first card in the deck before the holdem shuffle
hold_odds = function(sims,shuffle){
  list_card_count = list()
  vect = integer(52)
  for (x in 1:52){
    list_card_count = append(list_card_count,list(vect))
  }
  for(x in 1:sims){
    deck = shuffle((1:52))
    for(card in 1:52){
      position = match(card,deck)
      list_card_count[[position]][card] = list_card_count[[position]][card] + 1
    }
  }
  percent_list = list()
  for(vec in list_card_count){
    percent_vec = (vec/sims) * 100
    percent_list = append(percent_list,list(percent_vec))
  }
  return(percent_list)
}

hold_odds(1000,holdem_shuffle)

#odds of a player getting dealt either of their cards based on 
#the cards starting position in the deck
#i.e. the third player dealt has about a 2.5% chance that either of his cards
#are the second card in the deck before the holdem shuffle
many_holdem_deals = function(sims,numpl,deck){
  y = hold_odds(sims)
  
  list_card_count = list()
  
  for (i in 1:numpl){
    index = list((unlist(y[i])+unlist(y[numpl+i]))/2)
    list_card_count = append(list_card_count,index)
  }
  
  return(list_card_count)
}
many_holdem_deals(1000,6,1:52)



