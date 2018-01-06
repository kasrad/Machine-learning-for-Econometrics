#Week 6 Lab Session
library(data.table) # load and transform data
library(tidyverse) # pipes, plots and other nice stuff
library(tm) # stop words
library(text2vec) # heavy lifting of text analysis
library(LDAvis) # visualize topic models

  
# load data from past weeks:
amazon_sentences <- fread( "amazon_cells_labelled.txt", header=FALSE, sep="\t")
amazon_sentences$Source <- "Amazon"
yelp_sentences <- fread( "yelp_labelled.txt", header=FALSE, sep="\t")
yelp_sentences$Source <- "Yelp"
imdb_sentences <- fread( "imdb_labelled.txt", header=FALSE, sep="\t")
imdb_sentences$Source <- "Imdb"

# select which documents to include in our analysis try out different (combinations of sets and see how results vary)
alltext <- rbind(yelp_sentences)

# convert to data.table
alltext <- alltext %>%
  mutate(id = rownames(alltext)) %>%
  rename(review = V1, sentiment = V2)
# rename cols
setDT(alltext)
setkey(alltext, id)

set.seed(1211) #to reproduce results

# train test split 
# Use alltext[J(train_ids)] / alltext[J(test_ids)] for each set
all_ids <- alltext$id
train_ids <- sample(all_ids, round(nrow(alltext)*0.8))
test_ids <- setdiff(all_ids, train_ids)

# this week we show you the text2vec approach which has a nice implementation of LDA
  
tokens = alltext[J(train_ids)]$review %>% 
tolower %>% 
word_tokenizer
it = itoken(tokens, ids = alltext[J(train_ids)]$id, progressbar = FALSE)

# we still use tm package's stopwords though
stop_words <- stopwords('en')
# account for typo's by also adding those words without ' as stopwords and where they are separated by a space
new_stop_words <- stop_words
for(word in stop_words){
  split <- strsplit(word,"'")[[1]]
  if(length(split) > 1){
    for(w in split){
      new_stop_words <- c(new_stop_words,w)
    }
    new_stop_words <- c(new_stop_words,paste(split,collapse=''))
  }
}
new_stop_words <- unique(new_stop_words)

# create vocabulary
v = create_vocabulary(it,stopwords = new_stop_words) %>% 
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)

# create the dtm
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")
#####LDA
# Create your first LDA Model
n_topics = 20
lda_model = LDA$new(n_topics = n_topics, doc_topic_prior = 1/n_topics,
                    topic_word_prior = 1/length(v[[1]]))

# calc doc topic matrix
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)


doc_id <- 39
title <- print(alltext[train_ids[doc_id]]$review)

barplot(doc_topic_distr[doc_id, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1),
        names.arg = 1:ncol(doc_topic_distr))

# to inspect the full topic word matrix use : lda_model$topic_word_distribution
lda_model$get_top_words(n = 10, topic_number = c(9,20), lambda = 1)

lda_model$plot()

# create the dtm of the test set
new_dtm = itoken(alltext[J(test_ids)]$review, tolower, word_tokenizer, ids = alltext[J(test_ids)]$id) %>% 
  create_dtm(vectorizer, type = "dgTMatrix")

# estimate the document topic distribution of this new text based on the old model
new_doc_topic_distr = lda_model$transform(new_dtm)

# calculate perplexity
perp <- perplexity(new_dtm,
                   topic_word_distribution = lda_model$topic_word_distribution,
                   doc_topic_distribution = new_doc_topic_distr)


print(perp)

#Question 1
perp_matrix <- matrix(ncol = 2, nrow = length(seq(5,100,5)))
colnames(perp_matrix) <- c('n_topics','perplexity')

for (i in seq(5, 100, 5)) {
  n_topics = i
  lda_model = LDA$new(n_topics = n_topics, doc_topic_prior = 1/n_topics,
                      topic_word_prior = 1/length(v[[1]]))
  
  # calc doc topic matrix
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)
  
  
  
  # estimate the document topic distribution of this new text based on the old model
  new_doc_topic_distr = lda_model$transform(new_dtm)
  
  # calculate perplexity
  perp_matrix[i/5, 1] <- i
  perp_matrix[i/5, 2] <-
    perplexity(new_dtm,
               topic_word_distribution = lda_model$topic_word_distribution,
               doc_topic_distribution = new_doc_topic_distr)
  print(i)
}

perp_matrix

#Question 2


# Create a function which calculates the number of non-zero values
# in the document topic matrix for different values of the doc_topic_prior.
# Plot your results. Interpret and explain your findings

doc_prior_matrix <- matrix(ncol = 3, nrow = length(seq(5,100,5)))
colnames(doc_prior_matrix) <- c('n_topics','doc_topic_prior','non-zero-values')

for (i in seq(5, 100, 5)) {
  n_topics = i
  lda_model = LDA$new(n_topics = n_topics, doc_topic_prior = 1/n_topics,
                      topic_word_prior = 1/length(v[[1]]))
  
  # calc doc topic matrix
  doc_topic_distr = 
    lda_model$fit_transform(x = dtm, n_iter = 1000, 
                            convergence_tol = 0.001, n_check_convergence = 25, 
                            progressbar = FALSE)
  
  
  number <- sum(doc_topic_distr == 0)/(dim(doc_topic_distr)[1]*dim(doc_topic_distr)[2])
  
  # estimate the document topic distribution of this new text based on the old model
  
  # calculate perplexity
  doc_prior_matrix[i/5, 1] <- i
  doc_prior_matrix[i/5, 2] <- 1/i
  doc_prior_matrix[i/5, 3] <- number
    
  print(i)
}

doc_prior_matrix
