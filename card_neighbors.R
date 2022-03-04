#looking at cards that are likely next to other cards
#percent chance that a card is next to another card. 



#odds of the neighbor of a card being that close to the card after the shuffle
#i.e. any card in the decks neighbor has about a 12.45 % chance of being it's neighbor
#again. 

card_neighbors_counts = function(sims,shuffle_type,cards){
  clumps_odds = list()
  bef =  matrix(0, nrow=52, ncol=52)
  before1 = list()
  for(i in 1:cards){
    before1 = append(before1,list(bef))
  }
  for (i in 1:sims){
    shuf = shuffle_type(1:52)

    for (x in 1:52){
      for (z in 1:cards){
        pos = match(x,shuf)
        if ((pos-z) > 0){
          before = shuf[pos-z]
          before1[[z]][x,before] = before1[[z]][x,before]+1
        }
    }}
  }
  list_of_maxs = list()
  for (i in 1:cards){
    vals = c()
    for (x in 2:52){
      before_val = before1[[i]][x,(x-1)]
      vals = append(vals,before_val)
    }
    list_of_maxs = append(list_of_maxs,list(vals))
  }
  #names(list_of_maxs) = c(1:cards)
  return(list_of_maxs)

}

card_neighbors_counts(10000,holdem_shuffle,52)

#odds of the neighbor of a card being that close to the card after the shuffle
#i.e. any card in the decks neighbor has about a 12.45 % chance of being it's neighbor
#again. 
average_neighbor = function(sims,shuffle,cards){
  maxs = card_neighbors_counts(sims,shuffle,cards)
  before_after_vals = c()
  for (i in 1:cards){
    val = (sum(unlist(maxs[i]))*100)/(sims*51)
    before_after_vals = append(before_after_vals,val)
  }
  names(before_after_vals) = c(1:cards)
  return(before_after_vals)
}
tic()
hold = average_neighbor(100000,holdem_shuffle,6)
riffle = average_neighbor(100000,better_shuffle,6)
alt5 = average_neighbor(100000,alt_shuffle5,6)
toc()

#MSE for values of neighbor cards ending x distance away from neighbor at
#a certain probability. MSE is extremely small. 
#this shows that the average is a good enough approximation

neighbor_ci = function(sims,shuffle,cards){
  maxs = card_neighbors_counts(sims,shuffle,cards)
  before_after_vals = c()
  for (i in 1:cards){
    val = (sum(unlist(maxs[i]))*100)/(sims*51)
    before_after_vals = append(before_after_vals,val)
  }
  names(before_after_vals) = c(1:cards)
  sds = c()
  for(i in 1:cards){
    odds = unlist(maxs[i])*100/sims
    mean = mean(odds)
    sd = sd(odds,na.rm = TRUE)
    sds = append(sds,sd)
  }
  names(sds) = c(1:cards)
  U = c()
  L = c()
  for (i in 1:cards){
    p = before_after_vals[i]
    ci_lower = p - 1.96*(sds[i]/sqrt(sims))
    ci_upper = p + 1.96*(sds[i]/sqrt(sims))
    U = append(U,ci_upper)
    L = append(L,ci_lower)
  } 
  return(list(before_after_vals,U,L))
  
}
tic()
neighbor_ci(100,holdem_shuffle,6)
toc()


neighbor_bar_chart = function(sims,shuffle1,shuffle2,cards){
  holdem = hold
  nums = c()
  for (i in 1:cards){
    nums = append(nums,unlist(holdem[i]))
    nums = append(nums,unlist(riffle[i]))

  }
  
  data <- data.frame(values = nums,
                     group = rep(c(1:cards),
                                 each = 2),
                     subgroup = c("Casino Shuffle","7 riffle shuffles"))

  
  ggplot(data,
         aes(x = group,
             y = values, 
             fill = reorder(subgroup,1:((cards)*2)))) + 
    geom_bar(stat = "identity",
             position = "dodge")+
    labs(title="Probability of neighbor cards being n cards apart",
         x ="Cards apart", y = "Probability",fill = "shuffle")+ 
    geom_abline(intercept = (100/51), slope = (-100/(52*51)),
                color="black", linetype="dashed")+
    geom_text(aes(label = round(values,2)), vjust = -0.2, size = 7, position = position_dodge(0.9))+
    theme(plot.title = element_text(hjust = .5),
          text = element_text(size=20))
}
tic()
neighbor_bar_chart(100,holdem_shuffle,better_shuffle,6)
toc()


neighbor_bar_chart3 = function(sims,shuffle1,shuffle2,shuffle3,cards){
  holdem = hold
  nums = c()
  for (i in 1:cards){
    nums = append(nums,unlist(holdem[i]))
    nums = append(nums,unlist(alt5[i]))
    nums = append(nums,unlist(riffle[i]))
    
  }
  
  data <- data.frame(values = nums,
                     group = rep(c(1:cards),
                                 each = 3),
                     subgroup = c("Casino Shuffle","Alt shuffle","7 riffle shuffles"))
  
  
  ggplot(data,
         aes(x = group,
             y = values, 
             fill = reorder(subgroup,1:((cards)*3)))) + 
    geom_bar(stat = "identity",
             position = "dodge")+
    labs(title="Probability of neighbor cards being n cards apart",
         x ="Cards apart", y = "Probability",fill = "Shuffle")+ 
    geom_abline(intercept = (100/51), slope = (-100/(52*51)),
                color="black", linetype="dashed")+
    geom_text(aes(label = round(values,2)), vjust = -0.2, 
              size = 7, position = position_dodge(0.9))+
    theme(plot.title = element_text(hjust = .5),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15),
          text = element_text(size=20))
}
tic()
neighbor_bar_chart3(100,holdem_shuffle,better_shuffle,alt_shuffle,6)
toc()




alt = average_neighbor(100000,alt_shuffle,6)
alt2 = average_neighbor(100000,alt_shuffle2,6)
alt3 = average_neighbor(100000,alt_shuffle3,6)
alt4 = average_neighbor(100000,alt_shuffle5,6)
alt5 = average_neighbor(100000,alt_shuffle4,6)
alt6 = average_neighbor(100000,alt_shuffle6,6)


nums = c()
for (i in 1:6){
  nums = append(nums,unlist(alt[i]))
  nums = append(nums,unlist(alt2[i]))
  nums = append(nums,unlist(alt4[i]))
  nums = append(nums,unlist(alt6[i]))
  nums = append(nums,unlist(alt5[i]))
  nums = append(nums,unlist(alt3[i]))
}

data <- data.frame(values = nums,
                   group = rep(c(1:6),
                               each = 6),
                   subgroup = c("alt1","alt2","alt3","alt4","alt5","alt6"))


ggplot(data,
       aes(x = group,
           y = values, 
           fill = reorder(subgroup,1:36))) + 
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(title="Probability of neighbor cards being n cards apart",
       x ="Cards apart", y = "Probability",fill = "shuffle")+ 
  geom_abline(intercept = (100/51), slope = (-100/(52*51)),
              color="black", linetype="dashed")+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        text = element_text(size=20))



