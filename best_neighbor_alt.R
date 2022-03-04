#3 shuffles
alt3_shuffle1 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt3_shuffle2 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,1)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt3_shuffle3 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

alt3_shuffle4 = function(deck){
  shuffled_deck = strip_shuffle(deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,3)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

###################
alt3_1 = average_neighbor(100000,alt3_shuffle1,6)
alt3_2 = average_neighbor(100000,alt3_shuffle2,6)
alt3_3 = average_neighbor(100000,alt3_shuffle3,6)
alt3_4 = average_neighbor(100000,alt3_shuffle4,6)



nums = c()
for (i in 1:6){
  nums = append(nums,unlist(alt3_4[i]))
  nums = append(nums,unlist(alt3_2[i]))
  nums = append(nums,unlist(alt3_1[i]))
  nums = append(nums,unlist(alt3_3[i]))

}

data <- data.frame(values = nums,
                   group = rep(c(1:6),
                               each = 4),
                   subgroup = c("alt3_1","alt3_2","alt3_3","alt3_4"))


ggplot(data,
       aes(x = group,
           y = values, 
           fill = reorder(subgroup,1:24))) + 
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(title="Probability of neighbor cards being n cards apart: 3 total riffles",
       x ="Cards apart", y = "Probability",fill = "shuffle")+ 
  geom_abline(intercept = (100/51), slope = (-100/(52*51)),
              color="black", linetype="dashed")+
  theme(
        plot.title = element_text(size = 20, hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        text = element_text(size=20))+
  scale_fill_brewer(palette= 1)


#4 shuffles
alt4_shuffle1 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


alt4_shuffle2 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,1)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


alt4_shuffle3 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,1)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,3)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
alt4_shuffle4 = function(deck){
  shuffled_deck = strip_shuffle(deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,4)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}
alt4_shuffle5 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,4)
  shuffled_deck = strip_shuffle(shuffled_deck)
  
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}

###################
alt4_1 = average_neighbor(100000,alt4_shuffle1,6)
alt4_2 = average_neighbor(100000,alt4_shuffle2,6)
alt4_3 = average_neighbor(100000,alt4_shuffle3,6)
alt4_4 = average_neighbor(100000,alt4_shuffle4,6)
alt4_5 = average_neighbor(100000,alt4_shuffle5,6)


nums = c()
for (i in 1:6){
  nums = append(nums,unlist(alt4_4[i]))
  nums = append(nums,unlist(alt4_3[i]))
  nums = append(nums,unlist(alt4_1[i]))
  nums = append(nums,unlist(alt4_2[i]))
  nums = append(nums,unlist(alt4_5[i]))
  
}

data <- data.frame(values = nums,
                   group = rep(c(1:6),
                               each = 5),
                   subgroup = c("alt4_1","alt4_2","alt4_3","alt4_4","alt4_5"))


ggplot(data,
       aes(x = group,
           y = values, 
           fill = reorder(subgroup,1:30))) + 
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(title="Probability of neighbor cards being n cards apart: 4 total riffles",
       x ="Cards apart", y = "Probability",fill = "shuffle")+ 
  geom_abline(intercept = (100/51), slope = (-100/(52*51)),
              color="black", linetype="dashed")+
  theme(plot.title = element_text(size = 20, hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        text = element_text(size=20))+
  scale_fill_brewer(palette = 2)


#5 shuffles
alt_shuffle = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,3)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,2)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


alt_shuffle2 = function(deck){
  shuffled_deck = many_riffle_shuffles(deck,2)
  shuffled_deck = strip_shuffle(shuffled_deck)
  shuffled_deck = many_riffle_shuffles(shuffled_deck,3)
  shuffled_deck = deck_cut(shuffled_deck)
  return(shuffled_deck)
}


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

##################
alt = average_neighbor(100000,alt_shuffle,6)
alt2 = average_neighbor(100000,alt_shuffle2,6)
alt3 = average_neighbor(100000,alt_shuffle3,6)
alt4 = average_neighbor(100000,alt_shuffle4,6)
alt5 = average_neighbor(100000,alt_shuffle5,6)
alt6 = average_neighbor(100000,alt_shuffle6,6)

nums = c()
for (i in 1:6){
  nums = append(nums,unlist(alt6[i]))
  nums = append(nums,unlist(alt4[i]))
  nums = append(nums,unlist(alt2[i]))
  nums = append(nums,unlist(alt[i]))
  nums = append(nums,unlist(alt3[i]))
  nums = append(nums,unlist(alt5[i]))
}

data <- data.frame(values = nums,
                   group = rep(c(1:6),
                               each = 6),
                   subgroup = c("alt5_1","alt5_2","alt5_3","alt5_4","alt5_5","alt5_6"))


ggplot(data,
       aes(x = group,
           y = values, 
           fill = reorder(subgroup,1:36))) + 
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(title="Probability of neighbor cards being n cards apart: 5 total riffles",
       x ="Cards apart", y = "Probability",fill = "shuffle")+ 
  geom_abline(intercept = (100/51), slope = (-100/(52*51)),
              color="black", linetype="dashed")+
  theme(plot.title = element_text(size = 20, hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        text = element_text(size=20))+
  scale_fill_brewer(palette = 3)





#final graph comparing gradual change...
#CS, alt3_3, alt4_5, alt5_5, 7 riffs

nums = c()
for (i in 1:6){
  nums = append(nums,unlist(hold[i]))
  nums = append(nums,unlist(alt3_3[i]))
  nums = append(nums,unlist(alt4_5[i]))
  nums = append(nums,unlist(alt5[i]))
  nums = append(nums,unlist(riffle[i]))
  
}

data <- data.frame(values = nums,
                   group = rep(c(1:6),
                               each = 5),
                   subgroup = c("Casino Shuffle",
                                "Alt w. 3 riffles",
                                "Alt w. 4 riffles",
                                "Alt w. 5 riffles",
                                "7 riffles"))


ggplot(data,
       aes(x = group,
           y = values, 
           fill = reorder(subgroup,1:30))) + 
  geom_bar(stat = "identity",
           position = "dodge",)+
  labs(title="Probability of neighbor cards being n cards apart",
       x ="Cards apart", y = "Probability",fill = "shuffle")+ 
  geom_abline(intercept = (100/51), slope = (-100/(52*51)),
              color="black", linetype="dashed")+
  theme(plot.title = element_text(size = 20, hjust = .5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        text = element_text(size=20))+
  scale_fill_manual(values=c("#F8766D", "#6BAED6", "#2CA25F","#8856A7","#00BFC4"))


