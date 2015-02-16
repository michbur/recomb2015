length(three_n_power)


res_table <- do.call(rbind, lapply(three_n_power, function(single_rep)
  unlist(lapply(single_rep, function(single_fraction) 
    (sapply(single_fraction, length) - 1)/c(4, 32, 192)))))
