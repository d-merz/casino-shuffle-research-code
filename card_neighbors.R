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

card_neighbors_counts(1000,holdem_shuffle,52)

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
x = average_neighbor(1000,holdem_shuffle,51)
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
neighbor_ci(1000,holdem_shuffle,6)
toc()


neighbor_bar_chart = function(sims,shuffle1,shuffle2,cards){
  holdem = neighbor_ci(sims,shuffle1,cards)
  riffle = neighbor_ci(sims,shuffle2,cards)
  nums = c()
  ci_u = c()
  ci_l = c()
  for (i in 1:cards){
    nums = append(nums,unlist(holdem[[1]][i]))
    nums = append(nums,unlist(riffle[[1]][i]))
    #ci_u = append(ci_u,unlist(holdem[[2]][i]))
    #ci_u = append(ci_u,unlist(riffle[[2]][i]))
    #ci_l = append(ci_l,unlist(holdem[[3]][i]))
    #ci_l = append(ci_l,unlist(riffle[[3]][i]))
  }
  
  data <- data.frame(values = nums,
                     group = rep(c(1:cards),
                                 each = 2),
                     subgroup = c("Casino Shuffle","7 riffles"))
                     #U = ci_u,
                     #L = ci_l)
  
  ggplot(data,
         aes(x = group,
             y = values, 
             fill = reorder(subgroup,1:((cards)*2)))) + 
    geom_bar(stat = "identity",
             position = "dodge")+
    labs(title="Probability of neighbor cards being n cards apart",
         x ="Cards apart", y = "Probability",fill = "shuffle")+ 
    #geom_errorbar(aes(ymax = U, ymin = L),width = .5, 
                      #position=position_dodge(.9))+ 
    geom_abline(intercept = (100/51), slope = (-100/(52*51)),
                color="black", linetype="dashed")+
    theme(plot.title = element_text(hjust = .5))
}
tic()
neighbor_bar_chart(10000,alt_shuffle,better_shuffle,51)
toc()