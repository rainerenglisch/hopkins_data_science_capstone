library("quanteda")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")
require(readtext)
library(ggplot2)
library(data.table)
library(stringr)
library(sqldf)
library(memoise)

# thoughts:
# - for n-grams with n> 1 pad beginning of sentence with <BOS> and at the end <EOS>
# - is it possible to calculate unigram from bigram? => no!
# - token for unknown word <UNK> needed?
# - most common metric for evaluating a language model is probability the model assigns to *test data*,
# or the derivative measures of cross-entropy and perplexity (not include the beginning of sentence token)


BASE_DIR = "./data/en_US/"
FNAMES = c("en_US.twitter.txt","en_US.blogs.txt","en_US.news.txt")
BASE_DIR2="./4_task/"
FNAMES_TOKENS = paste0(BASE_DIR2,c("en_US.twitter.tokens_train.Rds","en_US.blogs.tokens_train.Rds","en_US.news.tokens_train.Rds"))
FNAMES_TOKENS_TEST = paste0(BASE_DIR2,c("en_US.twitter.tokens_test.Rds","en_US.blogs.tokens_test.Rds","en_US.news.tokens_test.Rds"))
FNAMES_N_GRAMS = paste0(BASE_DIR2,c("freq_1_grams.Rds","freq_2_grams.Rds","freq_3_grams.Rds","freq_4_grams.Rds"))
TRAIN_TEST_RATIO = 0.8
n_grams = list()
FNAMES_CORPUS_TEST = paste0(BASE_DIR2, "corpus_test.Rds")
doc_test_all = NULL

#get_ngram_fname = function (n) {
#  fname = paste0("./4_task/freq_",n,"_grams.Rds")
#  return (fname)
#}

# function creates a ngram model
# parameter: tokens: that form basis for n_gram model
#            n: which n-gram model to return
#            n_freq_n_grams: maximum number of n-grams

create_n_gram_model = function(toks, n = 2, top_n_grams=5000000){
  # create two grams
  print(paste0("creating ",n,"-grams"))
  n_grams = tokens_ngrams(toks, n = n)
  # create two gram dfm 
  print(paste0("creating dfm for ",n,"-grams"))
  dfm_n_grams = dfm(n_grams)
  print("calculating frequencies from dfm")
  freq_n_grams = textstat_frequency(dfm_n_grams,n=top_n_grams)
  grams = sapply(freq_n_grams$feature, str_split, pattern="_")
  # create separate column for each gram
  for (i in 1:n) {
    freq_n_grams = cbind(freq_n_grams, gram = sapply(grams, `[[`, i))
    colnames = colnames(freq_n_grams)[colnames(freq_n_grams) == "gram"] = paste0("gram_", i)
  }
  
  return (freq_n_grams)
}

print(paste0("Preprocessing: creating tokens"))
i=0
for (fname in FNAMES) {
  i=i+1
  if (!file.exists(FNAMES_TOKENS[i])) {
    print(paste0("Processing ", fname))
    # read plain text
    print(paste0(BASE_DIR,fname))
    raw_doc = readtext(paste0(BASE_DIR,fname))
    # create quanteda corpus
    doc = corpus(raw_doc)
    raw_doc = NULL
    # transform to document per sentence
    doc = corpus_reshape(doc, to ="sentences")
    resampled_index = quanteda:::resample(1:length(doc), replace = FALSE)
    doc_train = doc[resampled_index[1:(length(doc)*TRAIN_TEST_RATIO-1)]]
    doc_test = doc[resampled_index[(length(doc)*TRAIN_TEST_RATIO):length(doc)]]
    if (is.null(doc_test_all))
      doc_test_all = doc_test
    else
      doc_test_all= doc_test_all + doc_test
    #doc=corpus_sample(doc, size = 1000)
    toks_train = tokens(doc_train, padding= T,
                  remove_punct =T,  
                  remove_symbols = T,
                  remove_numbers = T,
                  remove_url = T)
    toks_test = tokens(doc_test, padding= T,
                        remove_punct =T,  
                        remove_symbols = T,
                        remove_numbers = T,
                        remove_url = T)
    
    doc_train = doc_test = NULL
    gc()
    saveRDS(toks_train, file =FNAMES_TOKENS[i])
    saveRDS(toks_test, file =FNAMES_TOKENS_TEST[i])
    
  }
}

toks_test_all = tokens(doc_test_all, padding= T,
                       remove_punct =T,  
                       remove_symbols = T,
                       remove_numbers = T,
                       remove_url = T)
# to do save it

if (!file.exists(FNAMES_CORPUS_TEST)) {
  saveRDS(doc_test_all, file =FNAMES_CORPUS_TEST)
  } else {
  doc_test_all = readRDS(FNAMES_CORPUS_TEST)
  }
doc_test_all=NULL
gc()


# iterate over n grams from 1 to 3
for (n_i in 1:4) {#3) {
  dt_freq_n_grams = NULL
  print(paste0("creating ",n_i,"-gram!"))
  # check if n gram file already exists
  if (!file.exists(FNAMES_N_GRAMS[n_i])) {
    # if not create n gram file
    # iterate over tokens of the corpuses
    for (fname_tokens in FNAMES_TOKENS) {
      print(paste0("Processing ", fname_tokens))
      toks=readRDS(fname_tokens)
      # createn n gram model
      temp_dt_freq_n_grams = as.data.table(create_n_gram_model(toks,n=n_i),stringsAsFactors = FALSE) #,top_n_grams=NULL
      # subselect data table
      #temp_dt_freq_n_grams = subset(temp_dt_freq_n_grams, select=-c(rank,group))
      temp_dt_freq_n_grams = temp_dt_freq_n_grams[, c("rank","group"):= NULL]
      #temp_dt_freq_n_grams =sqldf("SELECT feature, frequency, docfreq, gram_1, gram_2, gram_3, gram_4 FROM temp_dt_freq_n_grams")
      # if n gram table is not empty then append
      if (is.null(dt_freq_n_grams)){
          dt_freq_n_grams = temp_dt_freq_n_grams
        }
        else{
          dt_freq_n_grams = rbind(dt_freq_n_grams,temp_dt_freq_n_grams)
          #dt_freq_n_grams=sqldf("SELECT feature, SUM(frequency) AS frequency, SUM(docfreq) AS docfreq, min(gram_1) AS gram_1, min(gram_2) as gram_2, min(gram_3) AS gram_3, min(gram_4) as gram_4 FROM dt_freq_n_grams GROUP BY feature")
          min_sql = ""
          for (g_i in 1:n_i) {
            min_sql = paste0(min_sql, ", min(gram_",g_i,") as gram_",g_i)
          }
          #                                min(gram_1) AS gram_1, min(gram_2) as gram_2, min(gram_3) AS gram_3, min(gram_4) as gram_4 \
          dt_freq_n_grams=sqldf(paste0("SELECT feature, SUM(frequency) AS frequency, SUM(docfreq) AS docfreq ",
                                min_sql,
                                " FROM dt_freq_n_grams GROUP BY feature"))
        }
        temp_dt_freq_n_grams = NULL
        gc()
      }
      # save data frame
      saveRDS(as.data.table(dt_freq_n_grams), file =FNAMES_N_GRAMS[n_i])#, compress=FALSE)
      n_grams = append(n_grams, list(dt_freq_n_grams))
      #dt_freq_n_grams= NULL
    } else {
      n_grams=append(n_grams, list(readRDS(FNAMES_N_GRAMS[n_i])))
      #n_grams=append(n_grams, as.data.table(readRDS(FNAMES_N_GRAMS[n_i])))
      
    }
}

# setting indizes for n_grams
setindex(n_grams[[1]], gram_1)
setindex(n_grams[[1]], frequency)
indices(n_grams[[1]])

setindex(n_grams[[2]], gram_1, gram_2)
setindex(n_grams[[2]], gram_1)
setindex(n_grams[[2]], frequency)
setindex(n_grams[[2]], frequency, gram_1)
setindex(n_grams[[2]], gram_2)
indices(n_grams[[2]])

setindex(n_grams[[3]], gram_1, gram_2, gram_3)
setindex(n_grams[[3]], gram_1, gram_2)
setindex(n_grams[[3]], gram_2)
setindex(n_grams[[3]], frequency)
setindex(n_grams[[3]], frequency, gram_1, gram_2)
setindex(n_grams[[3]], gram_2, gram_3)
indices(n_grams[[3]])

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

lambda = function(history) {
  l = D1*N_c(1,history)
  l = l + D2*N_c(2,history)
  l = l + D3*N_c(3,history, comparison="greater")
  l = l/count_continuation(history)
  return (l)
}

p_kn = function(tokens, smoothing = FALSE) {
  count_tokens = count(tokens)
  tokens_history = tokens[1:length(tokens)-1]
  if (!smoothing) {
    result = count_tokens - D_c(count_tokens)
    result = result / count_continuation(tokens_history)
  } else {
    #print("Smoothing!")
    count_prec = N_c_preceding(tokens)
    total_grams=N_c_around(tokens_history)
    result = max(count_prec - D_c(count_tokens),0) / total_grams
  }
  
  #print(paste0(toString(tokens),": ",result))
  
  # call recursive if at least one token would be left
  if (length(tokens) > 2) {
    recursive_result = lambda(tokens_history) 
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

history_tokens = c("live", "and","i'd")
follow_tokens = c("give","sleep","die","eat")
evaluate(history_tokens,follow_tokens)

history_tokens = c("me", "about","his")
follow_tokens = c("marital","financial","horticultural","spiritual")
evaluate(history_tokens,follow_tokens)

history_tokens = c("arctic", "monkeys","this")
follow_tokens = c("morning","month","weekend","decade")
evaluate(history_tokens,follow_tokens)


history_tokens = c("helps", "reduce","your")
follow_tokens = c("happiness","hunger","stress","sleepiness")
evaluate(history_tokens,follow_tokens)


history_tokens = c("to", "take","a")
follow_tokens = c("minute","look","picture","walk")
evaluate(history_tokens,follow_tokens)

history_tokens = c("to","settle","the")
follow_tokens = c("case","matter","incident","account")
evaluate(history_tokens,follow_tokens)

history_tokens = c("groceries","in","each")
follow_tokens = c("arm","finger","toe","hand")
evaluate(history_tokens,follow_tokens)

history_tokens = c("bottom","to","the")
follow_tokens = c("middle","center","side","top")
evaluate(history_tokens,follow_tokens)

history_tokens = c("bruises","from","playing")
follow_tokens = c("weekly","outside","inside","daily")
evaluate(history_tokens,follow_tokens)

history_tokens = c("of","adam","sandler's")
follow_tokens = c("movies","pictures","novels","stories")
evaluate(history_tokens,follow_tokens)
#------------------------
  
doc_test_all = readRDS(FNAMES_CORPUS_TEST)

#to do calculate perplexity on train set and test set
probability_sentence = function(sentence, max_hist=4, smoothing=FALSE) {
  prob_list = c()
  for (i in 1:length(tok_sent)) {
    token_seq = tok_sent[max(i-max_hist+1,1):i]
    print(token_seq)
    p_i = p_kn(token_seq, smoothing=smoothing)
    prob_list = c(prob_list,p_i)
    print(paste0(i,": ",p_i))
  }
  print(prob_list)
  return (prod(prob_list))
}

probability_corpus = function(corpus, max_hist=4, smoothing=FALSE) {
  #prob_list = c()
  prob = 0
  # iterate sentences
  for (sentence in corpus) {
    print(sentence)
    # tokenize sentence
    tok_sent = tokens(sentence, padding= T,
                      remove_punct =T,  
                      remove_symbols = T,
                      remove_numbers = T,
                      remove_url = T)[[1]]
    print(paste0("tok_sent: ",toString(tok_sent)))
    p_i = probability_sentence(tok_sent, max_hist, smoothing)
    #prob_list = c(prob_list,p_i)
    prob = prob + log2(p_i)
  }
  #print(prob_list)
  print(prob)
  #return (prod(prob_list))
  #return (sum(log2(prob_list)))#
  return (prob)
}

cross_entropy_corpus = function(corpus, max_hist=4, smoothing=FALSE) {
  prob_corpus = probability_corpus(corpus, max_hist=max_hist, smoothing=smoothing) 
  corpus_token_length = length(tokens(corpus, padding= T,
                                      remove_punct =T,  
                                      remove_symbols = T,
                                      remove_numbers = T,
                                      remove_url = T))
  #entropy = - log2(prob_corpus) / corpus_token_length
  entropy = - (prob_corpus) / corpus_token_length
  print(paste0("entropy: ",entropy))
  return (entropy)
}
perplexity_corpus = function(corpus, max_hist=4, smoothing=FALSE) {
  entropy = cross_entropy_corpus(corpus, max_hist, smoothing)
  perplexity = `^`(2,entropy)
  print(paste0("perplexity: ",perplexity))
  return (perplexity)
}
perplexity_corpus(doc_test_all[1:10])#,max_hist=3)

cross_entropy_corpus(doc_test_all[1:100])

probability_corpus(doc_test_all[1:3])
