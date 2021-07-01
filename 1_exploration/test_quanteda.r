library("quanteda")
require(readtext)

filename_blogs <- './Coursera-SwiftKey/final/en_US/en_US.blogs.txt'

raw_docs_blogs <- readtext(filename_blogs) #,cache = FALSE)
docs_blogs = corpus(raw_docs_blogs)
docs_blogs = corpus_reshape(docs_blogs, to ="sentences")
ndoc(docs_blogs)
head(docs_blogs)
