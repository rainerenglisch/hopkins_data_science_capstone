library(data.table)
BASE_DIR2="./"
FNAMES_N_GRAMS_PROBS = paste0(BASE_DIR2,c("freq_1_grams_probs.Rds","freq_2_grams_probs.Rds","freq_3_grams_probs.Rds","freq_4_grams_probs.Rds"))
n_grams = list()
for (n_i in 1:4) {
  n_grams=append(n_grams, list(readRDS(FNAMES_N_GRAMS_PROBS[n_i])))
  # remove nas
  #n_grams[[n_i]] = n_grams[[n_i]][!is.na(p_kn), ]
}
#https://drive.google.com/file/d/1nNzW829YTF2cL-KUcgSbY-J3tDWSd_53/view?usp=sharing
#https://drive.google.com/file/d/1hPqaDJ4e4T5xZYjNmuHMZBS0suZ3Kcdy/view?usp=sharing
#https://drive.google.com/u/0/uc?export=download&confirm=Ge4D&id=1hPqaDJ4e4T5xZYjNmuHMZBS0suZ3Kcdy
#https://drive.google.com/u/0/uc?export=download&confirm=NMN2&id=1hPqaDJ4e4T5xZYjNmuHMZBS0suZ3Kcdy
#BASE_DIR2="https://ghcdn.rawgit.org/rainerenglisch/hopkins_data_science_capstone/main/shiny_app_n_grams/"
# BASE_DIR2= "http://raw.githubusercontent.com/rainerenglisch/hopkins_data_science_capstone/main/shiny_app_n_grams/"
# FNAMES_N_GRAMS_PROBS = paste0(BASE_DIR2,c("freq_1_grams_probs.Rds","freq_2_grams_probs.Rds","freq_3_grams_probs.Rds","freq_4_grams_probs.Rds"))
# n_grams = list()
# for (n_i in 1:4) {
#  print(paste0("Reading from fileserver rds for ",n_i))
#  n_grams=append(n_grams, list(readRDS(url(FNAMES_N_GRAMS_PROBS[n_i]))))
#  }





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

# # fill all na probabilities with the minimum value
# for (n_i in 1:4) {
#   min_p_kn = 0.9*min(n_grams[[n_i]]$p_kn, na.rm=TRUE)
#   n_grams[[n_i]][is.na(p_kn), p_kn := min_p_kn]
# }
# 
# freqs_min = c(1,10,100,100)
# memsize=0
# for (n_i in 1:4) {
#   print(n_i)
#   n_grams[[n_i]] = n_grams[[n_i]][frequency >= freqs_min[n_i],]
#   memsize=memsize+object.size(n_grams[[n_i]])
#   print(object.size(n_grams[[n_i]]))
# }
# print(memsize,units = "auto", standard = "SI")
