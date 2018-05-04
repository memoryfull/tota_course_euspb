# This code will create a document-term
# matrix from PTA full texts with the
# aid of text2vec package
#
# Dmitriy Skougarevskiy, 25 April 2018

# Load/install dependencies
packages <- c("data.table", "stringi", "stringr", "lubridate", "countrycode", "Matrix", "tm", "textcat", "stringdist", "text2vec", "glmnet")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
	
	install.packages(setdiff(packages, rownames(installed.packages())))
	
}

capture.output(lapply(packages, library, character.only = T), file = "NUL")

# Go to the declared working directory
setwd(Sys.getenv('TOTA_COURSE_REPO'))

# Load PTA texts with metadata, pre-packed into
# a single data.table from ToTA. Please download it
# from https://github.com/memoryfull/tota_course_euspb/blob/master/data/pta_texts.rdata
# or clone the repo accordingly
load("data/pta_texts.rdata")

##################
# A quick out-of-box example: determine text language
# from its n-gram profile with the aid of textcat package:
# (https://www.jstatsoft.org/article/view/v052i06)

## Examine English- and French-language profiles
barplot(textcat::TC_char_profiles$english, horiz = T,las = 1, cex.names = 0.3)
barplot(textcat::TC_char_profiles$german, horiz = T,las = 1, cex.names = 0.3)

# Apply to texts in ToTA
pta_texts[, language_inferred := textcat(rta_text)]

# Confusion matrix
table(pta_texts$language, pta_texts$language_inferred) # precision = recall = 1

##################
# Examine n-grams by hand (pick two short agreements as an example)

# Tokenize agreements into words (unigrams)
tokens_temp <- text2vec::word_tokenizer(pta_texts[rta_id %in% c(4, 51)]$rta_text)

# Unigram occurrence profiles
unigrams_profile_1 <- table(tokens_temp[[1]])
unigrams_profile_2 <- table(tokens_temp[[2]])

# Unigram occurrence profiles, all in lowercase
# and without numbers
tokens_temp_reduced <- word_tokenizer(tolower(gsub("[0-9]", "", pta_texts[rta_id %in% c(4, 51)]$rta_text)))

unigrams_reduced_profile_1 <- table(tokens_temp_reduced[[1]])
unigrams_reduced_profile_2 <- table(tokens_temp_reduced[[2]])

# Words unique to each document
setdiff(names(unigrams_reduced_profile_1), names(unigrams_reduced_profile_2))
setdiff(names(unigrams_reduced_profile_2), names(unigrams_reduced_profile_1))

# Add non-occuring words as zero counts
all_unigrams <- union(names(unigrams_reduced_profile_1), names(unigrams_reduced_profile_2))

unigrams_reduced_profile_with_zeros_1 <- table(factor(tokens_temp_reduced[[1]], levels = all_unigrams))
unigrams_reduced_profile_with_zeros_2 <- table(factor(tokens_temp_reduced[[2]], levels = all_unigrams))

# Now we have comparable vectors, pack them in a document-term
# matrix
dtm_example <- as.matrix(rbind(unigrams_reduced_profile_with_zeros_1, unigrams_reduced_profile_with_zeros_2))
str(dtm_example)

# We may also exclude so-called stopwords from consideration
stopwords_defined <- tolower(gsub("[0-9]", "", tm::stopwords("english")))
dtm_example <- dtm_example[, !(colnames(dtm_example) %in% stopwords_defined)]
str(dtm_example)

# Compute cosine similarity between the
# unigram profiles of the two agreements
dtm_example%*%t(dtm_example)/(sqrt(rowSums(dtm_example^2) %*% t(rowSums(dtm_example^2)))) # 0.412

# What if we operate at character level?
qgrams(tolower(gsub("[0-9]", "", pta_texts[rta_id %in% c(4)]$rta_text)), q = 5)

stringdist::stringdistmatrix(tolower(gsub("[0-9]", "", pta_texts[rta_id %in% c(4, 51)]$rta_text)), tolower(gsub("[0-9]", "", pta_texts[rta_id %in% c(4, 51)]$rta_text)), method = "cosine", q = 5) # 0.552, much lower

##################
# Proceed with constructing a proper document-term matrix
# from all the PTAs in English language

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

# Finally, construct a document-term matrix as a sparse object
dtm_pta <- create_dtm(it_vec, vectorizer_vec, type = "dgCMatrix")
dtm_pta
str(dtm_pta)

# Apply tf-idf weighting to the document-term matrix
model_tfidf <- TfIdf$new()
dtm_tfidf <- model_tfidf$fit_transform(dtm_pta)
dtm_tfidf
str(dtm_tfidf)

##################
# Examples of basic analysis:

######
## Try to replicate the above by-hand computation of cosine distance
## Since our DTM is indexed by identifier, consider them
identifiers_of_interest <- pta_texts[rta_id %in% c(4, 51)]$identifier
round(sim2(dtm_pta[identifiers_of_interest,], method = "cosine"), 3) # 0.136, why is it so different?

######
## Which unigrams are characteristic of American treaty language?
## Consider a cross-validated LASSO

## Response variable is equal to one if treaty was signed by the US
us_treaty <- ifelse(grepl("USA",
							pta_texts[match(rownames(dtm_tfidf), pta_texts$identifier)]$rta_parties_original),
					1, 0)
					
## Fit LASSO
us_grams_glmnet <- cv.glmnet(x = dtm_tfidf, y = us_treaty, family = "binomial", alpha = 1, nfolds = 5)

## Plot regularisation path
plot(us_grams_glmnet)

## Examine coefficients from the model with lambda within one standard error from the lowest one
unigrams_characteristic_of_us <- as.matrix(coef(us_grams_glmnet, s = us_grams_glmnet$lambda.1se))
unigrams_characteristic_of_us <- data.table(unigram = rownames(unigrams_characteristic_of_us), coef = c(unigrams_characteristic_of_us))
setorderv(unigrams_characteristic_of_us, "coef", -1)
unigrams_characteristic_of_us[1:30]

# Clearly, this exercise should be run at article/para level
# to yield meaningful results

######
## Unsupervised treatment of DTM: LSA reduction

## Compute 50-dimensional semantic space of PTAs
## from the tf-idf-weighted DTM	
lsa_model <- LatentSemanticAnalysis$new(50)
pta_lsa <- lsa_model$fit_transform(dtm_tfidf)

## Apply PCA to the semantic space
pta_lsa_pca <- prcomp(pta_lsa)
pta_lsa_coords <- as.data.table(pta_lsa_pca$x[,1:2])
pta_lsa_coords[, identifier := rownames(dtm_tfidf)]
pta_lsa_coords[, c("rta_parties_original", "rta_name") := pta_texts[match(pta_lsa_coords$identifier, pta_texts$identifier), c("rta_parties_original", "rta_name")]]

## Plot the first two components
plot(PC2 ~ PC1, data = pta_lsa_coords)
text(pta_lsa_coords$PC2 ~ pta_lsa_coords$PC1, labels = pta_lsa_coords$rta_name, pos = 4, cex = 0.4)

## Focus on USA
plot(PC2 ~ PC1, data = pta_lsa_coords[grepl("USA", rta_parties_original)])
text(pta_lsa_coords[grepl("USA", rta_parties_original)]$PC2 ~ pta_lsa_coords[grepl("USA", rta_parties_original)]$PC1, labels = pta_lsa_coords[grepl("USA", rta_parties_original)]$rta_name, pos = 4, cex = 0.4)


##################
# Homework: what are the architectural grounds for this discussion:
# https://github.com/dselivanov/text2vec/issues/146 ? Hint: compare
# tidytext's approach to text mining (https://www.tidytextmining.com)
# with that of text2vec.
