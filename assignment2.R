library(tm)
library(NLP)
library(openNLP)
library(kernlab)
library(CORElearn)
library(e1071)
library(rpart)
library(gtools)
#################### 1. ####################

setwd("D:/FRI/3.letnik/IS/")
source <- DirSource("essay")
source$filelist <- mixedsort(source$filelist)
corpus <- Corpus(source)

# Do the basic preprocessing, as in the previous exercises
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)

# Add the vocabulary and structure grades.
Vocabulary <- read.table("vocabulary.txt")
Structure <- read.table("structure.txt")

vocabulary_test <- c(Vocabulary$V1[1:217])
vocabulary_learn <- c(Vocabulary$V1[218:723])

structure_test <- c(Structure$V1[1:217])
structure_learn <- c(Structure$V1[218:723])

# Initialize a sentence and word tokenizer
sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()
post_ann <- Maxent_POS_Tag_Annotator()

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
doc_rare_word <- function(dataset) {
  
  rare_list <- c()
  
  for (i in c(1:723)) {
    rare_list <- c(rare_list, length(which(dataset[i,]$v < 0.002, arr.ind = T)))
  }
  
  return(rare_list)
}
CA <- function(observed, predicted){
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}
CA2 <- function(observed, predicted) {
  
  acc <- 0
  for (i in 1:length(predicted)) {
    if (predicted[i] == observed[i]) {
      acc <- acc + 1
    }
  }
  acc <- acc / length(predicted)
  
  cat(paste("Accuracy:", acc, "\n"))
}

sent_lens <- lapply(corpus, avg_sent_len, sent_ann, word_ann)
doc_words <- lapply(corpus, doc_word_num, sent_ann, word_ann)
doc_sents <- lapply(corpus, doc_sent_num, sent_ann)

# plot(x = c(1:723), y=sent_lens, type="h", main = "x=document_id y=avg_sen_len")
# plot(x = c(1:723), y=doc_sents, type="h", main = "x=document_id y=sen_len")
# plot(x = c(1:723), y=doc_words, type="h", main = "x=document_id y=word_num_len")

corpus <- tm_map(corpus, removePunctuation)

# Convert the dataset into a matrix
dataset <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

popular_words <- unlist(findMostFreqTerms(dataset,n=1));
rare_list <- doc_rare_word(dataset)

# plot(x = c(1:723), y=rare_list, type="h", main = "x=document_id y=rare_words_num")

# We make a dictionary of most common words that follow a certain word.
# findAssocs(dataset, c("word"), c(0.9))

############################################

#################### 2. ####################

sent_lens_vec <- unlist(sent_lens)
doc_sents_vec <- unlist(doc_sents)
doc_words_vec <- unlist(doc_words)

sent_lens_test <- sent_lens_vec[1:217]
doc_sents_test <- doc_sents_vec[1:217]
doc_words_test <- doc_words_vec[1:217]
rare_list_test <- rare_list[1:217]

sent_lens_learn <- sent_lens_vec[218:723]
doc_sents_learn <- doc_sents_vec[218:723]
doc_words_learn <- doc_words_vec[218:723]
rare_list_learn <- rare_list[218:723]

# science_dict <- read.table("science.txt")

# Colums:
# - AvgSenLen
# - SenCount
# - WordCount
# - RareWordCount
# - Class(Score)
data_learn <- cbind(c(sent_lens_learn), c(doc_sents_learn), c(doc_words_learn), c(rare_list_learn), c(vocabulary_learn))
data_learn_frame <- data.frame(data_learn)
# data_test <- cbind(c(sent_lens_test), c(doc_sents_test), c(doc_words_test), c(rare_t))

data_test <- cbind(c(sent_lens_test), c(doc_sents_test), c(doc_words_test), c(rare_list_test))
data_test_frame <- data.frame(data_test)

# SVM (0.687)
sm <- svm(X5 ~ ., data = data_learn_frame)
predicted <- predict(sm, data_test_frame, type="class")
predicted <- round(predicted)
CA(predicted, vocabulary_test)

# NAIVE BAYES CLASSIFIER (0.15)
cm.nb <- CoreModel(X5 ~ ., data = data_learn_frame, model="bayes")
predicted <- predict(cm.nb, data_test_frame, type="class")
predicted <- round(predicted)
CA(predicted, vocabulary_test)

# RANDOM FOREST (0.11)
cm.rf <- CoreModel(X5 ~ ., data = data_learn_frame, model="rf")
predicted <- predict(cm.rf, data_test_frame, type="class")
predicted <- round(predicted)
CA(predicted, vocabulary_test)

# KSVM (0.691)
ksm <- ksvm(X5 ~ ., data = data_learn_frame,kernel = "rbfdot")
predicted <- predict(ksm, data_test_frame)
predicted <- round(predicted)
CA(predicted, vocabulary_test)

############################################

#################### 3. ####################

# install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at/", type="source")

word_type_matrix <- function(corpus, sent_ann, word_ann, post_ann) {
  
  word_type_mtx <- c()
  
  for(i in c(1:length(corpus))) {
    
    # Anotate the data.
    text <- corpus[[i]]$content
    a <- annotate(text, sent_ann)
    a2 <- annotate(text, word_ann, a)
    a3 <- annotate(text, post_ann, a2)
    
    # Get list of word types.
    a3_f <- data.frame(a3)
    word_type_list <- list(a3_f[5])
    
    wd_types <- c()
    for (i in c(1:length(word_type_list[[1]]$features))) {
      
      wd_types <- c(wd_types, word_type_list[[1]]$features[[i]]$POS)  
    }
    
    # Count each word type.
    nouns <- length(which(wd_types == "NN")) + length(which(wd_types == "NNS")) + length(which(wd_types == "NNP")) + length(which(wd_types == "NNPS"))
    adjectives <- length(which(wd_types == "JJ")) + length(which(wd_types == "JJS")) + length(which(wd_types == "JJR"))
    verbs <- length(which(wd_types == "VB")) + length(which(wd_types == "VBG")) + length(which(wd_types == "VBD")) + length(which(wd_types == "VBN")) + length(which(wd_types == "VBP")) +  length(which(wd_types == "VBZ"))
    adverbs <- length(which(wd_types == "RB")) + length(which(wd_types == "RBS")) + length(which(wd_types == "RBR"))
    preposition <- length(which(wd_types == "IN"))
    pronouns <- length(which(wd_types == "PRP"))
    
    word_type_mtx <- rbind(word_type_mtx, c(nouns, adjectives, verbs, adverbs, preposition, pronouns))
  }
  
  return(word_type_mtx)
}

wtm <- word_type_matrix(corpus, sent_ann, word_ann, post_ann)
wtmtmp <- wtm
# wtm <- cbind(wtmtmp, c(sent_lens), c(doc_sents), c(doc_words))

wtm_test <- wtm[1:217,]
wtm_learn <- cbind(wtm[218:723,], c(structure_learn))

wtm_test_frame <- data.frame(wtm_test)
wtm_test_frame$X1  <- unlist(wtm_test_frame$X1)
wtm_test_frame$X2  <- unlist(wtm_test_frame$X2)
wtm_test_frame$X3  <- unlist(wtm_test_frame$X3)
wtm_test_frame$X4  <- unlist(wtm_test_frame$X4)
wtm_test_frame$X5  <- unlist(wtm_test_frame$X5)
wtm_test_frame$X6  <- unlist(wtm_test_frame$X6)
wtm_test_frame$X7  <- unlist(wtm_test_frame$X7)
wtm_test_frame$X8  <- unlist(wtm_test_frame$X8)
wtm_test_frame$X9  <- unlist(wtm_test_frame$X9)

wtm_learn_frame <- data.frame(wtm_learn)
wtm_learn_frame$X1  <- unlist(wtm_learn_frame$X1)
wtm_learn_frame$X2  <- unlist(wtm_learn_frame$X2)
wtm_learn_frame$X3  <- unlist(wtm_learn_frame$X3)
wtm_learn_frame$X4  <- unlist(wtm_learn_frame$X4)
wtm_learn_frame$X5  <- unlist(wtm_learn_frame$X5)
wtm_learn_frame$X6  <- unlist(wtm_learn_frame$X6)
wtm_learn_frame$X7  <- unlist(wtm_learn_frame$X7)
wtm_learn_frame$X8  <- unlist(wtm_learn_frame$X8)
wtm_learn_frame$X9  <- unlist(wtm_learn_frame$X9)
wtm_learn_frame$X10 <- unlist(wtm_learn_frame$X10)

# SVM (0.631)
sm <- svm(X7 ~ ., data = wtm_learn_frame)
predicted <- predict(sm, wtm_test_frame , type="class")
predicted <- round(predicted)
CA(predicted, structure_test)

# KSVM (0.636)
ksm <- ksvm(X7 ~ ., data = wtm_learn_frame, kernel = "rbfdot")
predicted <- predict(ksm, wtm_test_frame)
predicted <- round(predicted)
CA(predicted, structure_test)

# NAIVE BAYES CLASSIFIER (0.23)
cm.nb <- CoreModel(X7 ~ ., data = wtm_learn_frame, model="bayes")
predicted <- predict(cm.nb, wtm_test_frame, type="class")
predicted <- round(predicted)
CA(predicted, structure_test)

# RANDOM FOREST (0.15)
cm.rf <- CoreModel(X7 ~ ., data = wtm_learn_frame, model="rf")
predicted <- predict(cm.rf, wtm_test_frame, type="class")
predicted <- round(predicted)
CA(predicted, structure_test)

############################################

#################### 4. ####################

stopWords <- stopwords("en")
fileName <- "namyths.txt"

mycorpus <- readChar(fileName, file.info(fileName)$size)
mycorpus <- removePunctuation(mycorpus)
mycorpus <- removeNumbers(mycorpus)
mycorpus <- tolower(mycorpus)
mycorpus <- removeWords(mycorpus, stopWords)
mycorpus <- stemDocument(mycorpus)

sent0 <- "Today I will work on project"
sent1 <- "I don't have time"
sent2 <- "In the past"
sent3 <- "The future"

finish_sent <- function(sent, mycorpus, mode) {
  
  text <- mycorpus
  text <- strsplit(text, split=' ')
  text <- text[[1]]  
  
  lookfor <- strsplit(sent, split=' ')
  lookfor <- lookfor[[1]]
  lookfor <- lookfor[length(lookfor)]
  
  occurances <- list()
  
  for(i in 2:length(text)) {
    word0 <- text[i - 1]
    word1 <- text[i]
    word2 <- text[i + 1]
    word3 <- text[i + 2]
    
    if(mode == "back") {
      
      occurances[[word1]] <- c(word0, occurances[[word1]]) 
    }
    else {
      occurances[[word1]] <- c(occurances[[word1]], word2, word3) 
    }
    
  }
  
  if(mode == "back") {
    
    sorted_table <- sort(table(occurances[[lookfor]]), decreasing=T)  
    
    predict <- names(sorted_table[1])
    
    tmp_sent <- gsub(lookfor, predict, sent)
    
    return(cat(tmp_sent, lookfor, "."))  
  }
  
  sorted_table <- sort(table(occurances[[lookfor]]), decreasing=T)  
  
  predict <- names(sorted_table[1])
  
  return(cat(sent, predict, "."))
}

############################################