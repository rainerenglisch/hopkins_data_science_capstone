
BASE_DIR2="./"

FNAMES_N_GRAMS_PROBS = paste0(BASE_DIR2,c("freq_1_grams_probs.Rds","freq_2_grams_probs.Rds","freq_3_grams_probs.Rds","freq_4_grams_probs.Rds"))
n_grams = list()


for (n_i in 1:4) {
  n_grams=append(n_grams, list(readRDS(FNAMES_N_GRAMS_PROBS[n_i])))
}


# setting indizes for n_grams
setindex(n_grams[[1]], feature)
setindex(n_grams[[1]], gram_1)
setindex(n_grams[[1]], frequency)
indices(n_grams[[1]])

setindex(n_grams[[2]], feature)
setindex(n_grams[[2]], gram_1, gram_2)
setindex(n_grams[[2]], gram_1)
setindex(n_grams[[2]], frequency)
setindex(n_grams[[2]], frequency, gram_1)
setindex(n_grams[[2]], gram_2)
indices(n_grams[[2]])

setindex(n_grams[[3]], feature)
setindex(n_grams[[3]], gram_1, gram_2, gram_3)
setindex(n_grams[[3]], gram_1, gram_2)
setindex(n_grams[[3]], gram_2)
setindex(n_grams[[3]], frequency)
setindex(n_grams[[3]], frequency, gram_1, gram_2)
setindex(n_grams[[3]], gram_2, gram_3)
indices(n_grams[[3]])

setindex(n_grams[[4]], feature)
setindex(n_grams[[4]], gram_1, gram_2, gram_3, gram_4)
setindex(n_grams[[4]], gram_1, gram_2, gram_3)
setindex(n_grams[[4]], gram_2, gram_3)
setindex(n_grams[[4]], frequency)
setindex(n_grams[[4]], frequency, gram_1, gram_2, gram_3)
setindex(n_grams[[4]], gram_2, gram_3, gram_4)
indices(n_grams[[4]])

# fill all na probabilities with the minimum value

for (n_i in 1:4) {
  min_p_kn = 0.9*min(n_grams[[n_i]]$p_kn, na.rm=TRUE)
  n_grams[[n_i]][is.na(p_kn), p_kn := min_p_kn]
}

