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


BASE_DIR = "../data/en_US/"
FNAMES = c("en_US.twitter.txt","en_US.blogs.txt","en_US.news.txt")
BASE_DIR2="./"
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
