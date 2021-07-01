library("spacyr")
spacy_initialize(model = "en_core_web_sm")

fileName <- './Coursera-SwiftKey/final/en_US/en_US.blogs.txt'
txt=readChar(fileName,1000000) #file.info(fileName)$size)

# process documents and obtain a data.table
parsedtxt <- spacy_parse(txt)#,max_length=file.info(fileName)$size)
parsedtxt
# calculate number of words
num_words = nrow(parsedtxt)
# calculate number of stop

tokens = spacy_tokenize(txt)
#tokens
# retrieve number of tokens
num_tokens= lengths(tokens)
spacy_finalize()
