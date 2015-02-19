length(three_n_power)

#excluding third one, for some weird reason it has 5 n-grams on one positions, don't know why
#check position_ngrams
three_n_power <- three_n_power[-3]

#true significant
power <- Reduce("+", lapply(three_n_power, function(single_rep) {
  #single replication dat
  sr_dat <- do.call(rbind, lapply(single_rep, unlist))
  sr_dat[, c(1, 3, 5)]/matrix(rep(c(4, 32, 192), 6), nrow = 6, byrow = TRUE)
}))/length(three_n_power)

#false significant
false <- Reduce("+", lapply(three_n_power, function(single_rep) {
  #single replication dat
  sr_dat <- do.call(rbind, lapply(single_rep, unlist))
  res <- (sr_dat[, c(2, 4, 6)] - sr_dat[, c(1, 3, 5)])/sr_dat[, c(2, 4, 6)]
  #how to handle NaNs?situation, when we have 0 significant features?
  #current klugde - treat them as 0.
  res[is.nan(res)] <- 0
  res
}))/length(three_n_power)