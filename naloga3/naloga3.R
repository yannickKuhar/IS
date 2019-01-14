library(tm)
library(NLP)
library(openNLP)
library(kernlab)
library(gtools)

setwd("D:/FRI/3.letnik/IS/naloga3")

source <- DirSource("essay")
source$filelist <- mixedsort(source$filelist)
corpus <- Corpus(source)

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)

Vocabulary <- read.table("vocabulary.txt")
Structure <- read.table("structure.txt")

sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()

avg_sent_len <- function(text, sent_ann, word_ann) {
  
  # Count the sentences
  a1 <- annotate(text, sent_ann)
  num_sents <- length(a1)
  
  # Count the words
  a2 <- annotate(text, word_ann, a1)
  
  # a2 also contains the sentence annotation, so we subtract them to obtain the 
  # correct number of words
  num_words <- length(a2) - length(a1)
  
  return(num_words/num_sents)
}
doc_sent_num <- function(text, sent_ann) {
  
  # Count the sentences
  a1 <- annotate(text, sent_ann)
  num_sents <- length(a1)
  
  return(num_sents)
}
doc_word_num <- function(text, sent_ann, word_ann) {
  
  a1 <- annotate(text, sent_ann)
  num_sents <- length(a1)
  
  a2 <- annotate(text, word_ann, a1)
  
  num_words <- length(a2) - length(a1)
  
  return(num_words)
}

sent_lens <- lapply(corpus, avg_sent_len, sent_ann, word_ann)
doc_words <- lapply(corpus, doc_word_num, sent_ann, word_ann)
doc_sents <- lapply(corpus, doc_sent_num, sent_ann)

# plot(x = c(1:723), y=sent_lens, type="h", main = "x=document_id y=avg_sen_len")
# plot(x = c(1:723), y=doc_sents, type="h", main = "x=document_id y=avg_sen_len")
# plot(x = c(1:723), y=doc_words, type="h", main = "x=document_id y=avg_sen_len")







