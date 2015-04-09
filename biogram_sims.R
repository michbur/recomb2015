#simulations for QuiPT


library(biogram)
library(pbapply)
#create a vector of targets
targets <- c(rep(1, 2000), rep(0, 2000))
#we will repeat test 60 times (only 60 because of the computational limitations)
three_n_power <- pblapply(1L:200, function(dummy_variable) {
  #generate 4000 totally random 20-nucleotides RNA sequences without any pattern among positive or negative cases
  seqs <- matrix(sample(1L:4, 20*4000, replace = TRUE), nrow = 4000)
  storage.mode(seqs) <- "integer"
  #2L:19 to eliminate border cases
  chosen_nuc <- sample(3L:18, 1)
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
      # unigrams, QuiPT 41.070 s
      # unigrams, without QuiPT 3847.926 s
      signif_ngrams <- cut(ig_test1, breaks = c(0, 1e-02, 1))[[1]]
      #[1] - significant n-grams found on position chosen_nuc
      #[2] - total number of significant n-grams
      if(length(signif_ngrams) > 0) {
        c(length(position_ngrams(signif_ngrams)[[as.character(chosen_nuc)]]),
          length(signif_ngrams))
      } else {
        c(0, 0)
      }
    })
  })
})

#don't overwrite simulations
save(three_n_power, file = "three_n_power.RData")

