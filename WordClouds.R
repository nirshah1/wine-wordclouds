---
  title: "Wine Word Clouds"
author: "Nirav Shah"
date: "24 Aug 2018"
output:
  html_document:
  number_sections: TRUE
toc: TRUE
fig_height: 4
fig_width: 7
code_folding: hide
---
  
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(DT)
library(kableExtra)
  
wines <- read_csv("C:/Users/anila/Documents/Fall2018/Research/WineData.csv")  
dim(wines)

#WorldCloud Code

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  inspect(corpus)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[50]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,random.order=FALSE)
  
}  

makeWordCloud(wines[["designation"]][1:1000])

wines_matrix = as.data.frame(as.matrix(wines))

makeWordCloud(wines_matrix[["description"]][wines_matrix["country"] == "US"])






