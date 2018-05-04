# This code will showcase the distributional
# semantics with the aid of PTA full texts
# and text2vec package
#
# Dmitriy Skougarevskiy, 4 May 2018

# Load/install dependencies
packages <- c("data.table", "stringi", "stringr", "lubridate", "countrycode", "Matrix", "tm", "textcat", "stringdist", "text2vec", "stats")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
	
	install.packages(setdiff(packages, rownames(installed.packages())))
	
}

capture.output(lapply(packages, library, character.only = T), file = "NUL")

# Go to the declared working directory
setwd(Sys.getenv('TOTA_COURSE_REPO'))

##################
# Prepare DTM and TCM from PTA full texts

# Load PTA texts with metadata
# from https://github.com/memoryfull/tota_course_euspb/blob/master/data/pta_texts.rdata
load("data/pta_texts.rdata")

# Define a function to preprocess all the texts
text_preparator <- function(x) {
	tolower(gsub("[0-9]", "", x))
}

# Create an iterator over tokens
it_vec <- itoken(pta_texts[language == "en"]$rta_text,				# texts to consider
					ids = pta_texts[language == "en"]$identifier,	# ids of texts
					preprocessor = text_preparator,					# apply this function to texts before tokenisation 
					tokenizer = word_tokenizer,						# which function to use to tokenise elements
					progressbar = T)

# Create a vocabulary of unigrams from the corpus
vocab_vec <- create_vocabulary(it_vec, ngram = c(1, 1), stopwords = text_preparator(tm::stopwords("english")))
str(vocab_vec)

# Remove terms that occur less than 5 times or less than in 2 documents
vocab_vec_pruned <- prune_vocabulary(vocab_vec, term_count_min = 5, doc_count_min = 2)
str(vocab_vec_pruned)

# An object to define vocabulary vectoriser
vectorizer_vec <- vocab_vectorizer(vocab_vec_pruned)

# Construct a document-term matrix as a sparse object
dtm_pta <- create_dtm(it_vec, vectorizer_vec, type = "dgCMatrix")
  
# Apply tf-idf weighting to the document-term matrix
model_tfidf <- TfIdf$new()
dtm_tfidf <- model_tfidf$fit_transform(dtm_pta)

# Create a term-co-occurrence matrix
tcm_pta <- create_tcm(it_vec, vectorizer_vec, type = "dgCMatrix", skip_grams_window = 5)

##################
# Term-count matrix analysis

# Words that co-occur most with "investment"
sort(tcm_pta["investment",], decreasing = T)[1:20]

# Now try to change the skip_grams_window parameter in create_tcm
# and see how it affects the results

# What if we apply LSA to this matrix?
lsa_model <- LatentSemanticAnalysis$new(50)
tcm_lsa <- lsa_model$fit_transform(tcm_pta)

# Compute Cosine distances between TCM->LSA word vectors
lsa_word_vectors_dist <- dist2(tcm_lsa, method = "cosine")

# What are the words closest to selected words?
sort(lsa_word_vectors_dist["investment",], decreasing = F)[1:20]
sort(lsa_word_vectors_dist["tax",], decreasing = F)[1:20]

##################
# Compute GloVe word embeddings (https://nlp.stanford.edu/projects/glove/)

# Set up a model
glove_pta_model <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab_vec_pruned, x_max = 10, learning_rate = 0.20)

# Fit the model for 50 epochs and store word vectors
word_vectors_pta_main <- glove_pta_model$fit_transform(tcm_pta, n_iter = 50)
word_vectors_pta_context <- glove_pta_model$components

# We sum the word and context vectors to arrive at word vectors trained at PTA full texts
word_vectors_pta <- word_vectors_pta_main + t(word_vectors_pta_context)

# Compute Cosine distances between GloVe word vectors
glove_word_vectors_dist <- dist2(word_vectors_pta, method = "cosine")

# What are the words closest to selected words:
sort(glove_word_vectors_dist["investment",], decreasing = F)[1:20]
sort(glove_word_vectors_dist["tax",], decreasing = F)[1:20]

##################
# Operations with word embeddings

# Contrast LSA and GloVe vectors
data.table(lsa_investment = names(sort(lsa_word_vectors_dist["investment",], decreasing = F)[1:20]),
			glove_investment = names(sort(glove_word_vectors_dist["investment",], decreasing = F)[1:20]),
			lsa_tax = names(sort(lsa_word_vectors_dist["tax",], decreasing = F)[1:20]),
			glove_tax = names(sort(glove_word_vectors_dist["tax",], decreasing = F)[1:20])
		)

## What is trade without protection?
# Subtract meaning of word trade from meaning of word protection
trade_without_protection <- word_vectors_pta["trade",] - word_vectors_pta["protection",]

# Compute distances between word "trade - protection" and all words in the vocabulary
trade_without_protection_distances <- unlist(as.data.frame(t(dist2(word_vectors_pta, t(as.matrix(trade_without_protection)), method = "cosine"))))

# Words closest to "trade - protection" in meaning
trade_without_protection_closest <- sort(trade_without_protection_distances, decreasing = F)[1:20]
trade_without_protection_closest

## What is trade with protection?
sort(unlist(as.data.frame(t(dist2(word_vectors_pta, t(as.matrix(word_vectors_pta["trade",] + word_vectors_pta["protection",])), method = "cosine")))), decreasing = F)[1:20]


##################
# Aggregate word embeddings at PTA level

# Word vectors to data.table 
word_vectors_pta_dt <- data.table(term = rownames(word_vectors_pta), as.data.table(word_vectors_pta))
setkeyv(word_vectors_pta_dt, "term")

# Convert document-term matrix to a long data.table
summ <- summary(dtm_pta)
dtm_pta_long <- data.table(identifier = rownames(dtm_pta)[summ$i], term = colnames(dtm_pta)[summ$j], freq = summ$x)
setkeyv(dtm_pta_long, c("identifier", "term"))
rm(summ)

# Merge word embeddings
terms_ptas_with_word_vectors <- merge(dtm_pta_long, word_vectors_pta_dt, by = c("term"), all.x = F, all.y = F, sort = F)
gc()
dim(terms_ptas_with_word_vectors)

# Average frequency-weighted word vectors at PTA level
pta_vectors_tfweighted <- terms_ptas_with_word_vectors[, lapply(.SD, weighted.mean, w = freq), by = "identifier", .SDcols = 4:ncol(terms_ptas_with_word_vectors)]

## Apply PCA to the semantic space
pta_vectors_pca <- prcomp(pta_vectors_tfweighted[,2:ncol(pta_vectors_tfweighted)])
pta_vectors_coords <- as.data.table(pta_vectors_pca$x[,1:2])
pta_vectors_coords[, identifier := pta_vectors_tfweighted$identifier]
pta_vectors_coords[, c("rta_parties_original", "rta_name") := pta_texts[match(pta_vectors_coords$identifier, pta_texts$identifier), c("rta_parties_original", "rta_name")]]

## Plot the first two components
plot(PC2 ~ PC1, data = pta_vectors_coords)
text(pta_vectors_coords$PC2 ~ pta_vectors_coords$PC1, labels = pta_vectors_coords$rta_name, pos = 4, cex = 0.6)

## Focus on USA
plot(PC2 ~ PC1, data = pta_vectors_coords[grepl("USA", rta_parties_original)])
text(pta_vectors_coords[grepl("USA", rta_parties_original)]$PC2 ~ pta_vectors_coords[grepl("USA", rta_parties_original)]$PC1, labels = pta_vectors_coords[grepl("USA", rta_parties_original)]$rta_name, pos = 4, cex = 0.3)


##### Contrast the reduced semantic space with LSA PCA reduction from before
model_tfidf <- TfIdf$new()
dtm_tfidf <- model_tfidf$fit_transform(dtm_pta)
lsa_model_pta <- LatentSemanticAnalysis$new(50)
pta_lsa <- lsa_model_pta$fit_transform(dtm_tfidf)

pta_lsa_pca <- prcomp(pta_lsa)
pta_lsa_coords <- as.data.table(pta_lsa_pca$x[,1:2])
pta_lsa_coords[, identifier := rownames(dtm_tfidf)]
pta_lsa_coords[, c("rta_parties_original", "rta_name") := pta_texts[match(pta_lsa_coords$identifier, pta_texts$identifier), c("rta_parties_original", "rta_name")]]

## Plot the first two components
plot(PC2 ~ PC1, data = pta_lsa_coords)
text(pta_lsa_coords$PC2 ~ pta_lsa_coords$PC1, labels = pta_lsa_coords$rta_name, pos = 4, cex = 0.3)

## Focus on USA
plot(PC2 ~ PC1, data = pta_lsa_coords[grepl("USA", rta_parties_original)])
text(pta_lsa_coords[grepl("USA", rta_parties_original)]$PC2 ~ pta_lsa_coords[grepl("USA", rta_parties_original)]$PC1, labels = pta_lsa_coords[grepl("USA", rta_parties_original)]$rta_name, pos = 4, cex = 0.6)

# Homework: imagine interesting word summation/subtractions
# and study them, e.g. what is tax - avoidance?
