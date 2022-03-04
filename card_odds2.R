library(tictoc)
#for n shuffles tracks the position of card x and returns the percent it 
#showed up in each position

card_odds = function(sims,shuffles,card,deck_length){
  card_count = integer(deck_length)
  
  for(x in 1:sims){
    deck = many_riffle_shuffles((1:deck_length),shuffles)
   
    position = match(card,deck)
    
    card_count[[position]] = card_count[[position]] + 1
  }
  return(card_count)
}

percent_by_position = function(sims,shuffles,card_select,deck_length){
  counts = card_odds(sims,shuffles,card_select,deck_length)
  
  list_of_percents = list()
  for (x in counts){
    percent = 100*(x/(sims))
    
    list_of_percents = append(list_of_percents,percent)
  }
  return(list_of_percents)
}

#tracks each card in the deck and returns a list of vectors of the percent the card shows
#up in each position

every_card_odds = function(sims,shuffles,deck_length){
  list_card_count = list()
  vect = integer(deck_length)
  for (x in 1:deck_length){
    list_card_count = append(list_card_count,list(vect))
  }
  for(x in 1:sims){
    deck = many_riffle_shuffles((1:deck_length),shuffles)
    for(card in 1:deck_length){
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
y = every_card_odds(1000,7,52)
print(y)
toc()

