setwd("C:\\Users\\ram.p\\Documents\\R\\capstone")

library(NLP)
library(tm)
library(fpc)
library(ggplot2)
library(stringi)
library(data.table)
library(SnowballC)
library(RWeka)
library(dplyr)
library(cld3)


data.blog =  readLines("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\en_US\\en_US.blogs.txt",encoding = "UTF-8", skipNul = TRUE)
data.twitter =  readLines("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\en_US\\en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE)
data.news = readLines("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\en_US\\en_US.news.txt",encoding = "UTF-8", skipNul = TRUE)

blog_size=file.size("final\\en_US\\en_US.blogs.txt")
twitter_size=file.size("final\\en_US\\en_US.twitter.txt")
news_size=file.size("final\\en_US\\en_US.news.txt")
size_matrix = matrix(c(length(data.blog),length(data.twitter),length(data.news),
                       sum(nchar(data.blog)),sum(nchar(data.twitter)),sum(nchar(data.news)),
                       round((blog_size/1024^2),1),round((twitter_size/1024^2),1),round((news_size/1024^2),1),
                       stri_stats_latex(data.blog)[[4]],stri_stats_latex(data.twitter)[[4]],stri_stats_latex(data.news)[[4]]),
                     byrow = FALSE,nrow=3,ncol=4,dimnames = list(c("blogs","twitter","news"),c("No. of Lines","No. Of Characters","File Size in Mb","Word count")))

data_blog = iconv(data.blog,"latin1","ASCII",sub = "")
data_twitter = iconv(data.twitter,"latin1","ASCII",sub = "")
data_news = iconv(data.news,"latin1","ASCII",sub = "")

sample_blog = sample(data_blog,round(0.01*length(data_blog)))
sample_twitter = sample(data_twitter,round(0.01*length(data_twitter)))
sample_news = sample(data_news,round(0.01*length(data_news)))
size_matrix2 = matrix(c(length(sample_blog),length(sample_twitter),length(sample_news),
                       stri_stats_latex(sample_blog)[[4]],stri_stats_latex(sample_twitter)[[4]],stri_stats_latex(sample_news)[[4]]),
                     byrow = FALSE,nrow=3,ncol=2,dimnames = list(c("sample blogs","sample twitter","sample news"),c("No. of Lines","No. Of words")))

set.seed(100)
merged_data = c(sample_blog,sample_twitter,sample_news)
corpus_data = VCorpus(VectorSource(merged_data))

preprocess=function(x)
{
  x = tm_map(x, removePunctuation)
  x = tm_map(x, removeNumbers)
  x = tm_map(x, stripWhitespace)
  x = tm_map(x, content_transformer(tolower))
  x = tm_map(x, PlainTextDocument, lazy = TRUE)
  return(x)
}

preprocess_data=preprocess(corpus_data)

saveRDS(object = preprocess_data, file = sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\preprocessed.rds"))

data.clean=readRDS("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\preprocessed.rds")
dataVS <- VectorSource(data.clean)
dataCorpus <- VCorpus(dataVS)

generateNGram <- function(corpus, level = 1) {
  options(mc.cores=1)
  tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = level, max = level))
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
  freq <- slam::row_sums(tdm)
  freq <- freq[order(-freq)]
  freq <- data.frame(word = names(freq), freq = freq)
}


tetraGram <- generateNGram(dataCorpus, 4)
# Split NGram in frequencies table
tetraGramSplit <- within(tetraGram, word <- data.frame(do.call('rbind', strsplit(as.character(word), " ", fixed = T))))
rownames(tetraGramSplit) <- 1:nrow(tetraGramSplit)
tetraGramSplit$word1 <- tetraGramSplit$word$X1
tetraGramSplit$word2 <- tetraGramSplit$word$X2
tetraGramSplit$word3 <- tetraGramSplit$word$X3
tetraGramSplit$word4 <- tetraGramSplit$word$X4
tetraGramSplit <- tetraGramSplit %>% select(word1, word2, word3, word4, freq)

saveRDS(object = tetraGramSplit, file = sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\gram.rds"))

data.ngram <- readRDS(sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\gram.rds"))


model <- list()

model$w1w2w3 <- data.ngram %>%
  group_by(word1, word2, word3) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word1, word2, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w2w3 <- data.ngram %>%
  select(word2, word3, word4, freq) %>%
  group_by(word2, word3, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word2, word3) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word2, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w3 <- data.ngram %>%
  select(word3, word4, freq) %>%
  group_by(word3, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word3) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word3, word4, desc(prob)) %>%
  as.data.frame()

model$w1w3 <- data.ngram %>%
  select(word1, word3, word4, freq) %>%
  group_by(word1, word3, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word1, word3) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word1, word3, word4, desc(prob)) %>%
  as.data.frame()

model$w1w2 <- data.ngram %>%
  select(word1, word2, word4, freq) %>%
  group_by(word1, word2, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word1, word2) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word1, word2, word4, desc(prob)) %>%
  as.data.frame()

model$w1 <- data.ngram %>%
  select(word1, word4, freq) %>%
  group_by(word1, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word1) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word1, word4, desc(prob)) %>%
  as.data.frame()

model$w2 <- data.ngram %>%
  select(word2, word4, freq) %>%
  group_by(word2, word4) %>%
  summarise_all(funs(sum(freq))) %>%
  group_by(word2) %>%
  mutate(freqTotal = sum(freq)) %>%
  group_by(word4, add = TRUE) %>%
  mutate(prob = freq / freqTotal) %>%
  arrange(word2, word4, desc(prob)) %>%
  as.data.frame()

model$w4 <- data.ngram %>%
  select(word4, freq) %>%
  group_by(word4) %>%
  summarise(freq = n()) %>%
  mutate(prob = freq / sum(freq)) %>%
  arrange(word4, desc(prob)) %>%
  as.data.frame()

saveRDS(object = model, file = sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\model.rds"))



calculateSimpleGoodTuring <- function(model){
  
  freqTable <- table(model$freq)
  
  SGT_DT <- data.frame(
    r=as.numeric(names(freqTable)),
    n=as.vector(freqTable),
    Z=vector("numeric",length(freqTable)),
    logr=vector("numeric",length(freqTable)),
    logZ=vector("numeric",length(freqTable)),
    r_star=vector("numeric",length(freqTable)),
    p=vector("numeric",length(freqTable)))
  
  num_r <- nrow(SGT_DT)
  
  for (j in 1:num_r) {
    if(j == 1) {
      r_i <- 0
    } else {
      r_i <- SGT_DT$r[j-1]
    }
    if(j == num_r) {
      r_k <- SGT_DT$r[j]
    } else {
      r_k <- SGT_DT$r[j+1]
    }
    SGT_DT$Z[j] <- 2 * SGT_DT$n[j] / (r_k - r_i)
  }

  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y <- r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else {
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      if(length(n_r_plus_1) == 0 ) {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      } else {
        n_r <- SGT_DT$n[j]
        x<-(r_plus_1) * n_r_plus_1/n_r
        if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
          SGT_DT$r_star[j] <- x
        } else {
          SGT_DT$r_star[j] <- y
          use_y = TRUE
        }
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)
}

predictNextWord <- function(testSentence, model, sgt, validResultsList=NULL) {
  
  options("scipen"=100, "digits"=8)
  
  testSentenceList <- unlist(strsplit(testSentence," "))
  noOfWords <- length(testSentenceList)
  
  resultDF <- data.frame(word4 = factor(), probAdj = numeric())
  
  predictNGram(resultDF, "w1w2w3", sgt$w1w2w3, validResultsList,
               model$w1w2w3 %>% filter(word1 == testSentenceList[noOfWords-2],
                                       word2 == testSentenceList[noOfWords-1],
                                       word3 == testSentenceList[noOfWords]))
  
  predictNGram(resultDF, "w2w3", sgt$w2w3, validResultsList,
               model$w2w3 %>% filter(word2 == testSentenceList[noOfWords-1],
                                     word3 == testSentenceList[noOfWords]))
  
  predictNGram(resultDF, "w3", sgt$w3, validResultsList,
               model$w3 %>% filter(word3 == testSentenceList[noOfWords]))
  
  predictNGram(resultDF, "w1w2", sgt$w1w2, validResultsList,
               model$w1w2 %>% filter(word1 == testSentenceList[noOfWords-2],
                                     word2 == testSentenceList[noOfWords-1]))
  
  predictNGram(resultDF, "w1w3", sgt$w1w3, validResultsList,
               model$w1w3 %>% filter(word1 == testSentenceList[noOfWords-2],
                                     word3 == testSentenceList[noOfWords]))
  
  predictNGram(resultDF, "w1", sgt$w1, validResultsList,
               model$w1 %>% filter(word1 == testSentenceList[noOfWords-2]))
  
  return(resultDF %>% arrange(desc(probAdj)))
  
}

predictNGram <- function(resultDF, labelName, sgt, validResultsList, subGram) {
  if(nrow(subGram) > 0 & !(nrow(resultDF) > 0)) {
    #print(labelName)
    subGram$probAdj <- sapply(subGram$freq, FUN = function(x) sgt$p[sgt$r == x])
    subGram <- subGram %>% select(word4, probAdj)
    if(!is.null(validResultsList) & nrow(subGram) > 0) {
      subGram <- subGram %>% filter(word4 %in% validResultsList)
    }
    eval.parent(substitute(resultDF <- subGram))
  }
}

cleanSentence <- function(testSentence) {
  testSentence <- stripWhitespace(testSentence)
  testSentence <- tolower(testSentence)
  testSentence <- removeNumbers(testSentence)
  testSentence <- removePunctuation(testSentence, preserve_intra_word_dashes = TRUE)
  return(testSentence)
}

predictWord <- function(sentence) {
  sentence <- cleanSentence(sentence)
  sentenceList <- unlist(strsplit(sentence," "))
  noOfWords <- length(sentenceList)
  if(noOfWords >= 3) {
    return(predictNextWord(paste(
      sentenceList[noOfWords-2],
      sentenceList[noOfWords-1],
      sentenceList[noOfWords]), predictor.model, predictor.sgt))
  } else if(noOfWords == 2) {
    return(predictNextWord(paste(
      "-",
      sentenceList[noOfWords-1],
      sentenceList[noOfWords]), predictor.model, predictor.sgt))
  } else if(noOfWords == 1) {
    return(predictNextWord(paste(
      "-",
      "-",
      sentenceList[noOfWords]), predictor.model, predictor.sgt))
  }
}

variables <- ls()
if(sum(variables == "model") == 0) {
  model <- readRDS(sprintf("%s/%s.rds", data_clean_dir, data_model_file))
  variables <- ls()
}

sgt <- list()
sgt$w1w2w3 <- calculateSimpleGoodTuring(model$w1w2w3)
sgt$w2w3 <- calculateSimpleGoodTuring(model$w2w3)
sgt$w3 <- calculateSimpleGoodTuring(model$w3)
sgt$w1w3 <- calculateSimpleGoodTuring(model$w1w3)
sgt$w1w2 <- calculateSimpleGoodTuring(model$w1w2)
sgt$w1 <- calculateSimpleGoodTuring(model$w1)
sgt$w2 <- calculateSimpleGoodTuring(model$w2)
sgt$w4 <- calculateSimpleGoodTuring(model$w4)


saveRDS(object = sgt, file = sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\sgt.rds"))


predictor <- list()
predictor.model <- model
predictor.sgt <- sgt
predictor.predictWord <- predictWord

#source("5_predicting.R")

test.file <- c()
for(source in c("blogs", "news", "twitter")) {
  
  file <- readLines(sprintf("C:\\Users\\ram.p\\Documents\\R\\capstone\\final\\en_US\\en_US.%s.txt",source), warn = F)
  file_lines <- length(file)
  file_sample <- ceiling(file_lines * 0.0001)
  test.file <- file[sample(1:file_lines, file_sample, replace = F)]
  rm(file)
  # Remove phrases that not are in english
  #test.file <- test.file[detect_Language(test.file)$detectedLanguage == "ENGLISH"]
  # Create a corpus
  dataVS <- VectorSource(test.file)
  testCorpus <- VCorpus(dataVS)
  # Transform to lower
  testCorpus <- tm_map(testCorpus, content_transformer(tolower))
  # Remove ponctuation
  testCorpus <- tm_map(testCorpus, removePunctuation)
  # Remove numbers
  testCorpus <- tm_map(testCorpus, removeNumbers)
  # Remove extra spaces
  testCorpus <- tm_map(testCorpus, stripWhitespace)
  
  test.clean <- c()
  for(i in 1:length(test.file)) {
    test.clean <- c(test.clean, testCorpus[[i]]$content)
  }
  
  totalWords <- 0
  rightWords <- 0
  for(i in 1:length(test.clean)) {
    sentence <- unlist(strsplit(test.clean[i]," "))
    n <- length(sentence)
    if(n > 3) {
      for(i in 1:(n - 3)) {
        wordsPredicted <- predictor.predictWord(sprintf("%s %s %s", sentence[i], sentence[i + 1], sentence[i + 2]))
        totalWords <- totalWords + 1
        if(sentence[i + 3] %in% head(wordsPredicted$word4)) {
          rightWords <- rightWords + 1
        }
      }
    }
  }
  
  print(sprintf("Predicted for %s in %s documents with %s of accuracy.",
                source,
                file_sample,
                round((rightWords / totalWords) * 100, 2)))
}





