
#runs and stores multiple riffle shuffles
mult_shuffled_decks = function(sims,shuffles){
  list_of_shuffled_decks = list()
  for (x in 1:sims){
    list_of_shuffled_decks = append(list_of_shuffled_decks,list(many_riffle_shuffles(1:52,shuffles)))
  }
  return(list_of_shuffled_decks)
  }

#list that holds a vector of values that are at a point x number of times

cards_sorted_by_position = function(sims,shuffles){
  manydecks = mult_shuffled_decks(sims,shuffles)
  
  position_freq = list()
  for (x in 1:52){
    vect = c()
    for (i in 1:sims){
      vect = append(vect,manydecks[[i]][x])
    
    }
    position_freq = append(position_freq,list(vect))
  }
  return(position_freq)
}



#count of a card in a position

cardcount = function(sims, shuffles,card_select){
  position_freq = cards_sorted_by_position(sims,shuffles)
  
  count_by_position = list()
  for (x in 1:52){
    count_of_card = 0
    count_of_card = sum(position_freq[[x]] == card_select)
    count_by_position = append(count_by_position,count_of_card)
    
  }}
  
  
  return(count_by_position)

#percent of times a card shows up in a position
percent_by_position = function(sims,shuffles,card_select){
  counts = cardcount(sims,shuffles,card_select)
  
  list_of_percents = list()
  for (x in counts){
    percent = 100*(x/(sims))
    
    list_of_percents = append(list_of_percents,percent)
  }
  return(list_of_percents)
}
y = percent_by_position(1000,7,5)
print(y)


