#simulations for QuiPT


library(biogram)
library(pbapply)
#create a vector of targets
targets <- c(rep(1, 2000), rep(0, 2000))
#we will repeat test 60 times (only 60 because of the computational limitations)
three_n_power <- pblapply(1L:60, function(dummy_variable) {
  #generate 4000 totally random 20-nucleotides RNA sequences without any pattern among positive or negative cases
  seqs <- matrix(sample(1L:4, 20*4000, replace = TRUE), nrow = 4000)
  storage.mode(seqs) <- "integer"
  chosen_nuc <- sample(1L:20, 1)
  lapply(seq(from = 0.25, to = 0.91, length.out = 6), function(prop_signif) {  
    #change random nucleotide for positive cases to favor one 
    #nucleotide
    seqs[1L:2000, chosen_nuc] <- sample(1L:4, 2000, 
                                        replace = TRUE, 
                                        prob = sample(c(prop_signif, rep((1 - prop_signif)/3, 3))))
    #test will be performed for n-grams with size 1-3
    lapply(1L:3, function(ngram_size) {
      seqs_ngrams <- count_ngrams(seqs, ngram_size, 1L:4, pos = TRUE)
      #only 1000 repetitions for monte carlo
      ig_test1 <- test_features(targets, seqs_ngrams)
      #calculate how many features are significant
      c(chosen_nuc, names(ig_test1[ig_test1 < 0.01]))
    })
  })
})

save(three_n_power, file = "three_n_power.RData")

