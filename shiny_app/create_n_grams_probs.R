options(max.print = .Machine$integer.max)

#library("quanteda")
#library("quanteda.textplots")
#library("quanteda.textstats")
#library("dplyr")
#require(readtext)
#library(ggplot2)
library(data.table)
library(stringr)
#library(sqldf)
library(memoise)
#library(foreach)
#library(doParallel)

args = commandArgs(trailingOnly=TRUE)

n_gram_from = 4
n_gram_to = 4

if (length(args)==2) {
  n_gram_from = args[1]
  n_gram_to = args[2]
  print(paste0("Using parameters: n_gram_from: ",n_gram_from,", n_gram_to: ",n_gram_to))
}


BASE_DIR2="./"

FNAMES_N_GRAMS = paste0(BASE_DIR2,c("freq_1_grams.Rds","freq_2_grams.Rds","freq_3_grams.Rds","freq_4_grams.Rds"))
FNAMES_N_GRAMS_PROBS = paste0(BASE_DIR2,c("freq_1_grams_probs.Rds","freq_2_grams_probs.Rds","freq_3_grams_probs.Rds","freq_4_grams_probs.Rds"))
n_grams = list()




for (n_i in 1:4) {
  n_grams=append(n_grams, list(readRDS(FNAMES_N_GRAMS[n_i])))
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

#
# calculate Ds
n_gram_max =n_grams[[length(n_grams)]]
n1 = n_gram_max[frequency == 1,sum(frequency)]#.N]
n2 = n_gram_max[frequency == 2,sum(frequency)]#.N]
n3 = n_gram_max[frequency == 3,sum(frequency)]#.N]
n4 = n_gram_max[frequency == 4,sum(frequency)]#.N]
Y = n1/(n1+2*n2)

D1 = 1 - 2*Y*n2/n1
D2 = 2 - 3*Y*n3/n2
D3 = 3 - 4*Y*n4/n3

#D1 = 0.9
#D2 = 1.8
#D3 = 2.1

D_c = function(count)  {
  D_c = 0
  if (count == 1) {
    D_c = D1
  } else if (count == 2) {
    D_c = D2
  } else if (count>=3) {
    D_c = D3
  }
  return (D_c)
}

count = function(tokens, verbose=FALSE) {
  len_tokens = length(tokens)
  result = 0
  if (len_tokens==0) {
    result = n_grams[[1]][,sum(frequency), verbose = verbose]
  } else if (len_tokens==1) {
    #result = n_grams[[1]][gram_1 == tokens[1],sum(frequency)]#.N]
    result = n_grams[[1]][tokens[1],sum(frequency), on = "gram_1", verbose = verbose]
  } else if (len_tokens==2) {
    #result = n_grams[[2]][gram_1 == tokens[1] & gram_2 == tokens[2],sum(frequency)]#.N]
    result = n_grams[[2]][.(tokens[1], tokens[2]),sum(frequency), on = c("gram_1","gram_2"), verbose = verbose]
  } else if (len_tokens==3) {
    #result = n_grams[[3]][gram_1 == tokens[1] & gram_2 == tokens[2] & gram_3 == tokens[3],sum(frequency)]#.N]
    result = n_grams[[3]][.(tokens[1], tokens[2], tokens[3]),sum(frequency), on = c("gram_1","gram_2","gram_3"), verbose = verbose]
  } else if (len_tokens==4) {
    #result = n_grams[[4]][gram_1 == tokens[1] & gram_2 == tokens[2] & gram_3 == tokens[3] & gram_4 == tokens[4],sum(frequency)]#.N]
    result = n_grams[[4]][.(tokens[1], tokens[2], tokens[3], tokens[4]),sum(frequency), on = c("gram_1","gram_2","gram_3","gram_4"), verbose = verbose]
  }
  if (is.na(result)) {
    result = 0
  }
  #print(paste0("count: ",toString(tokens ), " =", result ))
  return (result+1)
}

mem_count = memoise(count)
# calculate frequency of tokens following history 
count_continuation = function(history, verbose=FALSE) {
  len_hist = length(history)
  result = 0
  if (len_hist==0) {
    result = n_grams[[1]][,sum(frequency)]#.N]
  } else if (len_hist==1) {
    #result = n_grams[[2]][gram_1 == history[1],sum(frequency)]#.N]
    result = n_grams[[2]][ history[1],sum(frequency), on = "gram_1", verbose = verbose]
  } else if (len_hist==2) {
    #result = n_grams[[3]][gram_1 == history[1] & gram_2 == history[2],sum(frequency)]#.N]
    result = n_grams[[3]][.(history[1], history[2]),sum(frequency), on = c("gram_1","gram_2"), verbose = verbose]
  }else if (len_hist==3) {
    #result = n_grams[[4]][gram_1 == history[1] & gram_2 == history[2] & gram_3 == history[3],sum(frequency)]#.N]
    result = n_grams[[4]][.(history[1], history[2], history[3]),sum(frequency), on = c("gram_1","gram_2","gram_3"), verbose = verbose]
  }
  if (is.na(result)) {
    result = 0
  }  
  return (result+1)
}

mem_count_continuation = memoise(count_continuation)
N_c_preceding = function(tokens, verbose=FALSE) {
  len_tokens = length(tokens)
  result = 0
  if (len_tokens==1) {
    #result = n_grams[[2]][gram_2 == tokens[1], .N]
    result = n_grams[[2]][tokens[1], .N, on = "gram_2", verbose=verbose]
  } else if (len_tokens==2) {
    #result = n_grams[[3]][gram_2 == tokens[1] & gram_3 == tokens[2], .N]
    result = n_grams[[3]][.(tokens[1], tokens[2]), .N, on = c("gram_2","gram_3"), verbose=verbose]
  } else if (len_tokens==3) {
    #result = n_grams[[4]][gram_2 == tokens[1] & gram_3 == tokens[2] & gram_4 == tokens[3], .N]
    result = n_grams[[4]][.(tokens[1], tokens[2], tokens[3]), .N, on=c("gram_2","gram_3","gram_4"), verbose=verbose]
  }
  if (is.na(result)) {
    result = 0
  }  
  return (result + 1)
}

N_c_around = function(tokens, verbose=FALSE) {
  len_tokens =length(tokens)
  result = 0
  if (len_tokens==0) {
    result = n_grams[[2]][,.N]
  } else if (len_tokens==1) {
    #result = n_grams[[3]][gram_2 == tokens[1],.N]
    result = n_grams[[3]][tokens[1],.N,on="gram_2",verbose=verbose]
  } else if (len_tokens==2) {
    #result = n_grams[[4]][gram_2 == tokens[1] & gram_3 == tokens[2],.N]
    result = n_grams[[4]][.(tokens[1], tokens[2]),.N, on=c("gram_2","gram_3"),verbose=verbose]
  }
  if (is.na(result)) {
    result = 0
  }  
  return (result + 1)
}

# count of n-grams with frequency starting with history
N_c = function(freq,history , comparison = "equals", verbose=FALSE) {
  #print(paste0("N_c(", freq, ", ", history, ", ", comparison, ")"))
  len_hist = length(history)
  if (len_hist==0) {
    if (comparison=="equals" ){
      #result = n_grams[[1]][frequency == freq,.N]#.sum(frequency)]#
      result = n_grams[[1]][ freq,.N, on = "frequency", verbose=verbose]#.sum(frequency)]#
    } else {
      #result = n_grams[[1]][frequency >= freq,.N]#.sum(frequency)]#.N]
      count_all = n_grams[[1]][,.N, verbose=verbose]
      less_freq = n_grams[[1]][.(1:(freq-1)),.N, on="frequency", verbose=verbose]
      result = count_all - less_freq
    }
  } else if (len_hist==1) {
    if (comparison=="equals" ){
      #result = n_grams[[2]][frequency == freq & gram_1 == history[1], .N]#sum(frequency)]#.N]
      result = n_grams[[2]][.(freq &  history[1]), .N, on=c("frequency","gram_1"),verbose=verbose]#sum(frequency)]#.N]
    } else {
      #result = n_grams[[2]][frequency >= freq & gram_1 == history[1], .N]#sum(frequency)]#.N]
      count_all = n_grams[[2]][history[1], .N,on="gram_1", verbose=verbose]
      less_freq = n_grams[[2]][.(1:(freq-1),history[1]), .N,on=c("frequency","gram_1"), verbose=verbose]
      result = count_all - less_freq
      
    }
  } else if (len_hist==2) {
    if (comparison == "equals") {
      #result = n_grams[[3]][frequency == freq & gram_1 == history[1] & gram_2 == history[2], .N]#sum(frequency)]#.N]
      result = n_grams[[3]][.( freq, history[1], history[2]), .N, on=c("frequency","gram_1","gram_2"), verbose=verbose]
    } else {
      #result = n_grams[[3]][frequency >= freq & gram_1 == history[1] & gram_2 == history[2], .N]#sum(frequency)]#.N]
      count_all = n_grams[[3]][.(history[1],history[2]), .N,on=c("gram_1","gram_2"), verbose=verbose]
      less_freq = n_grams[[3]][.(1:(freq-1),history[1],history[2]), .N,on=c("frequency","gram_1","gram_2"), verbose=verbose]
      result = count_all - less_freq
    }
  } else if (len_hist==3) {
    if (comparison == "equals") {
      #result = n_grams[[4]][frequency == freq & gram_1 == history[1] & gram_2 == history[2] & gram_3 == history[3], .N]#sum(frequency)]#.N]
      result = n_grams[[4]][.( freq, history[1], history[2], history[3]), .N, on=c("frequency","gram_1","gram_2","gram_3"), verbose=verbose]
    } else {
      #result = n_grams[[4]][frequency >= freq & gram_1 == history[1] & gram_2 == history[2] & gram_3 == history[3], .N]#sum(frequency)]#.N]
      count_all = n_grams[[4]][.(history[1],history[2],history[3]), .N,on=c("gram_1","gram_2","gram_3"), verbose=verbose]
      less_freq = n_grams[[4]][.(1:(freq-1),history[1],history[2],history[3]), .N,on=c("frequency","gram_1","gram_2","gram_3"), verbose=verbose]
      result = count_all - less_freq
      
    }
  }
  if (is.na(result)) {
    result = 0
  }  
  return (result+1)
}

mem_N_c = memoise(N_c)
#mem_N_c = N_c



lambda = function(history) {
  l = D1*mem_N_c(1,history) #N_c(1,history)
  l = l + D2*mem_N_c(2,history) # N_c(2,history)
  l = l + D3*mem_N_c(3,history, comparison="greater") # N_c(3,history, comparison="greater")
  l = l/mem_count_continuation(history)
  return (l)
}
mem_lambda = memoise(lambda)

p_kn = function(tokens, smoothing = FALSE) {
  #print(paste0("p_kn(", toString(tokens), ")"))
  count_tokens = mem_count(tokens)
  tokens_history = tokens[1:length(tokens)-1]
  if (!smoothing) {
    result = count_tokens - D_c(count_tokens)
    result = result / mem_count_continuation(tokens_history)
  } else {
    #print("Smoothing!")
    count_prec = N_c_preceding(tokens)
    total_grams=N_c_around(tokens_history)
    result = max(count_prec - D_c(count_tokens),0) / total_grams
  }
  
  #print(paste0(toString(tokens),": ",result))
  
  # call recursive if at least one token would be left
  if (length(tokens) > 2) {
    recursive_result = mem_lambda(tokens_history) 
    #print(paste0("lambda: ",recursive_result))
    recursive_result = recursive_result * p_kn(tokens[2:length(tokens)],smoothing=smoothing)
    
    result = result + recursive_result
  }
  return (result)
}

evaluate = function(history_tokens,follow_tokens) {
  for (t in follow_tokens) {
    seq = c(history_tokens, t)
    print(seq)
    print(paste0("prob: ",p_kn(seq, smoothing=FALSE)))
    print(paste0("prob(smoothing): ",p_kn(seq, smoothing=TRUE)))
  }
}  

# decrease size of n_grams
#freq_max = 4
#for (n_i in 3:4) {
#  for (freq_i in 1:freq_max) {
#    n_rows_before = n_grams[[n_i]][,.N]
#    n_rows_freq_1 = n_grams[[n_i]][frequency==freq_i,.N]
#    print(paste0("Reducing ",n_i,"-gram ",sprintf(n_rows_freq_1/n_rows_before*100,fmt = '%#.1f'),"% from ",
#                 n_rows_before, " to ",n_rows_before-n_rows_freq_1," grams"))
#    n_grams[[n_i]] = n_grams[[n_i]][frequency > freq_i]
#  }
#}

#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster(4)#cores[1]-1) #not to overload your computer
#registerDoParallel(cl)


#freqs_min = c(1,3,4,5)
freqs_min = c(1,10,100,100)

#https://stackoverflow.com/questions/25431307/r-data-table-apply-function-to-rows-using-columns-as-arguments
do_parallel = FALSE

# calculate p_kn for all n_grams for 1 to 4
for (n_i in n_gram_from:n_gram_to) { #1:4) {
#foreach(n_i=1:2, .inorder=FALSE, .verbose=TRUE) %dopar% {
  all_nrows_grams = nrow(n_grams[[n_i]])
  nrows_grams = n_grams[[n_i]][frequency >= freqs_min[n_i],.N]
  print(paste0("calculating probs for ",n_i,"_grams with ",nrows_grams," rows."))
  #for (row in 1:nrow(n_grams[[n_i]])) {
  if (do_parallel) {

  if (n_i ==1) {
    #n_grams[[n_i]][, p_kn := p_kn(as.matrix(gram_1), smoothing=FALSE), by=1:nrows_grams] #frequency < freqs_min[n_i],
    n_grams[[n_i]][frequency >= freqs_min[n_i], p_kn:=lapply(gram_1, function(x) p_kn(as.matrix(x),smoothing=FALSE))]
    
  } else if (n_i == 2) {
    #dt[,other_gear:=mapply(function(x, y) setdiff(x, y), x=gearsL, y=gear)]

    n_grams[[n_i]][frequency >= freqs_min[n_i], 
                   p_kn:=mapply(function(g1,g2) p_kn(c(as.matrix(g1),as.matrix(g2)),smoothing=FALSE), 
                               g1=gram_1, g2=gram_2)]
    
    #n_grams[[n_i]][frequency < freqs_min[n_i], p_kn := p_kn(c(gram_1,gram_2), smoothing=FALSE)]#, by=frequency < freqs_min[n_i]]# by=1:nrows_grams
  } else if (n_i == 3) {
    #n_grams[[n_i]][frequency < freqs_min[n_i], p_kn := p_kn(c(gram_1,gram_2,gram_3), smoothing=FALSE)]#,  by=frequency < freqs_min[n_i]]# by=1:nrows_grams
    n_grams[[n_i]][frequency >= freqs_min[n_i], 
                   p_kn:=mapply(function(g1,g2,g3) p_kn(c(as.matrix(g1),as.matrix(g2),as.matrix(g3)),smoothing=FALSE), 
                                g1=gram_1, g2=gram_2, g3=gram_3)]
  } else if (n_i ==4) {
    #n_grams[[n_i]][frequency < freqs_min[n_i], p_kn := p_kn(c(gram_1,gram_2,gram_3,gram_4), smoothing=FALSE)]#),  by=frequency < freqs_min[n_i]]# by=1:nrows_grams
    n_grams[[n_i]][frequency >= freqs_min[n_i], 
                   p_kn:=mapply(function(g1,g2,g3,g4) p_kn(c(as.matrix(g1),as.matrix(g2),as.matrix(g3),as.matrix(g4)),smoothing=FALSE), 
                                g1=gram_1, g2=gram_2, g3=gram_3, g4=gram_3)]
  }
  } else {
    #not parallel
  n_gram_freq_min=n_grams[[n_i]][frequency >= freqs_min[n_i],]
  n_gram_freq_min_nrows = n_gram_freq_min[,.N]
  for (row_i in 1:n_gram_freq_min_nrows) {
    if (row_i %%  round(n_gram_freq_min_nrows/1000) == 1) {
      print(paste0(Sys.time(),", ",sprintf(row_i / n_gram_freq_min_nrows*100, fmt = '%#.4f') ,"% processed with with min freq.", freqs_min[n_i]))
      saveRDS(n_grams[[n_i]], file =FNAMES_N_GRAMS_PROBS[n_i])
    }
    row = n_gram_freq_min[row_i,]#

    if (row$frequency < freqs_min[n_i]) {
      next
    }
    if (n_i == 1) {
      #gram = as.matrix(n_grams[[n_i]][row,  c("gram_1")])
      gram = as.matrix(row$gram_1)
   } else if (n_i == 2) {
      #gram = as.matrix(n_grams[[n_i]][row,  c("gram_1","gram_2")])
      gram = c(as.matrix(row$gram_1),as.matrix(row$gram_2))
    } else if (n_i == 3) {
      #gram = as.matrix(n_grams[[n_i]][row,  c("gram_1","gram_2","gram_3")])
      gram = c(as.matrix(row$gram_1),as.matrix(row$gram_2,row$gram_3))
    } else if (n_i ==4) {
      #gram = as.matrix(n_grams[[n_i]][row,  c("gram_1","gram_2","gram_3","gram_4")])
      gram = c(as.matrix(row$gram_1),as.matrix(row$gram_2),as.matrix(row$gram_3),as.matrix(row$gram_4))
    }
    p = p_kn(gram, smoothing=FALSE)
    n_grams[[n_i]][feature==row$feature, "p_kn"] = p
  
  }
  }
  saveRDS(n_grams[[n_i]], file =FNAMES_N_GRAMS_PROBS[n_i])
  
}
#stopCluster(cl)

n_grams = list()
for (n_i in 1:4) {
  n_grams=append(n_grams, list(readRDS(FNAMES_N_GRAMS_PROBS[n_i])))
}
