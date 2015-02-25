library(reshape2)
library(ggplot2)

tfres <- function(x, cn = c("1-ngram", "2-gram", "3-gram"),
                  rn = round(seq(from = 0.25, to = 0.91, length.out = 6), 2)) {
  colnames(x) <- cn
  rownames(x) <- rn
  x <- melt(x)
  #significant proportion, ngram, value
  colnames(x) <- c("sig", "ngram", "value")
  x[["sig"]] <- as.factor(x[["sig"]])
  x
}
  

#excluding third one, for some weird reason it has 5 n-grams on one positions, don't know why
#check position_ngrams
three_n_power <- three_n_power[-3]

#true significant
power <- Reduce("+", lapply(three_n_power, function(single_rep) {
  #single replication dat
  sr_dat <- do.call(rbind, lapply(single_rep, unlist))
  sr_dat[, c(1, 3, 5)]/matrix(rep(c(4, 32, 192), 6), nrow = 6, byrow = TRUE)
}))/length(three_n_power)


#do 1-grams have bigger power
ggplot(tfres(power), aes(x = sig, y = ngram, fill = value)) + geom_tile()


ggplot(tfres(power), aes(x = ngram, y = value, fill = sig)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Important features found (%)") +
  scale_x_discrete("n-gram") +
  scale_fill_discrete("n-gram")



#false significant
#mean which ignores NaNs
mean_nonan <- function(x)
  sum(x[!is.nan(x)])/sum(!is.nan(x))

fs_dat <- sapply(three_n_power, function(single_rep) {
  #single replication dat
  sr_dat <- do.call(rbind, lapply(single_rep, unlist))
  (sr_dat[, c(2, 4, 6)] - sr_dat[, c(1, 3, 5)])/sr_dat[, c(2, 4, 6)]
})

fs_mean <- matrix(apply(fs_dat, 1, mean_nonan), nrow = 6)

#no significant features at all
no_sig <- matrix(apply(fs_dat, 1, function(i)
  sum(is.nan(i))), nrow = 6)

fs <- data.frame(tfres(fs_mean), no_sig = tfres(no_sig)[["value"]])

ggplot(fs, aes(x = ngram, y = sig, fill = no_sig)) + 
  geom_tile() +
  scale_fill_continuous(name = "No significant features", 
                        low = "#0072B2", high =  "#D55E00") +
  geom_point(aes(x = ngram, y = sig, size = value), range = c(5, 12)) +
  scale_size_continuous("No significant features")
