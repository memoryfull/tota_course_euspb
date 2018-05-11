# This code will add information on PTA
# full texts into the gravity model
# and text2vec package
# NB: code requires at least 7GB of RAM
# and several CPU hours to run
#
# Dmitriy Skougarevskiy, 10 May 2018

# Load/install dependencies
packages <- c("data.table", "stringi", "stringr", "lubridate", "countrycode", "Matrix", "tm", "textcat", "stringdist", "text2vec", "stats", "lfe", "readxl")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
	
	install.packages(setdiff(packages, rownames(installed.packages())))
	
}

capture.output(lapply(packages, library, character.only = T), file = "NUL")

# Go to the declared working directory
setwd(Sys.getenv('TOTA_COURSE_REPO'))

##################
# PREPARATORY STEP 1
# Prepare DTM- and TCM-based reductions of PTA full texts

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

# Remove terms that occur less than 5 times or less than in 2 documents
vocab_vec_pruned <- prune_vocabulary(vocab_vec, term_count_min = 5, doc_count_min = 2)

# An object to define vocabulary vectoriser
vectorizer_vec <- vocab_vectorizer(vocab_vec_pruned)

# Construct a document-term matrix as a sparse object
dtm_pta <- create_dtm(it_vec, vectorizer_vec, type = "dgCMatrix")
  
# Apply tf-idf weighting to the document-term matrix
model_tfidf <- TfIdf$new()
dtm_tfidf <- model_tfidf$fit_transform(dtm_pta)

# LSA reduction of DTM
model_tfidf <- TfIdf$new()
dtm_tfidf <- model_tfidf$fit_transform(dtm_pta)
lsa_model_pta <- LatentSemanticAnalysis$new(50)
pta_lsa <- lsa_model_pta$fit_transform(dtm_tfidf)

# PCA reduction of DTM LSA
pta_lsa_pca <- prcomp(pta_lsa)
pta_lsa_coords <- as.data.table(pta_lsa_pca$x[,1:2])
pta_lsa_coords[, identifier := rownames(dtm_tfidf)]

# Create a term-co-occurrence matrix
tcm_pta <- create_tcm(it_vec, vectorizer_vec, type = "dgCMatrix", skip_grams_window = 5)
 
# LSA reduction of TCM 
lsa_model <- LatentSemanticAnalysis$new(50)
tcm_lsa <- lsa_model$fit_transform(tcm_pta)
tcm_lsa_dt <- as.data.table(tcm_lsa)
tcm_lsa_dt[, term := rownames(tcm_lsa)]
setcolorder(tcm_lsa_dt, c("term", names(tcm_lsa_dt)[!grepl("term", names(tcm_lsa_dt))]))
tcm_lsa <- copy(tcm_lsa_dt)
rm(tcm_lsa_dt)

# GloVe reduction of TCM
glove_pta_model <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab_vec_pruned, x_max = 10, learning_rate = 0.20)

# Fit the model for 50 epochs and store word vectors
word_vectors_pta_main <- glove_pta_model$fit_transform(tcm_pta, n_iter = 50)
word_vectors_pta_context <- glove_pta_model$components

# We sum the word and context vectors to arrive at word vectors trained at PTA full texts
word_vectors_pta <- word_vectors_pta_main + t(word_vectors_pta_context)

# Word vectors to data.table 
word_vectors_pta_dt <- data.table(term = rownames(word_vectors_pta), as.data.table(word_vectors_pta))
setkeyv(word_vectors_pta_dt, "term")

# Convert document-term matrix to a long data.table
summ <- summary(dtm_pta)
dtm_pta_long <- data.table(identifier = rownames(dtm_pta)[summ$i], term = colnames(dtm_pta)[summ$j], freq = summ$x)
setkeyv(dtm_pta_long, c("identifier", "term"))
rm(summ)

# Merge word embeddings from GloVe or TCM LSA
terms_ptas_with_word_vectors <- merge(dtm_pta_long, word_vectors_pta_dt, by = c("term"), all.x = F, all.y = F, sort = F)
terms_ptas_with_tcm_vectors <- merge(dtm_pta_long, tcm_lsa, by = c("term"), all.x = F, all.y = F, sort = F)

# Average frequency-weighted word vectors at PTA level
pta_vectors_tfweighted <- terms_ptas_with_word_vectors[, lapply(.SD, weighted.mean, w = freq), by = "identifier", .SDcols = 4:ncol(terms_ptas_with_word_vectors)]
pta_tcm_tfweighted <- terms_ptas_with_tcm_vectors[, lapply(.SD, weighted.mean, w = freq), by = "identifier", .SDcols = 4:ncol(terms_ptas_with_tcm_vectors)]

# Apply PCA to the GloVe-reduced vector space
pta_vectors_pca <- prcomp(pta_vectors_tfweighted[,2:ncol(pta_vectors_tfweighted)])
pta_vectors_coords <- as.data.table(pta_vectors_pca$x[,1:2])
pta_vectors_coords[, identifier := pta_vectors_tfweighted$identifier]

# Apply PCA to the TCM-reduced vector space
pta_tcm_pca <- prcomp(pta_tcm_tfweighted[,2:ncol(pta_tcm_tfweighted)])
pta_tcm_coords <- as.data.table(pta_vectors_pca$x[,1:2])
pta_tcm_coords[, identifier := pta_tcm_tfweighted$identifier]


# Organise all computed representations in one data.table
## DTM LSA
pta_embeddings <- as.data.table(pta_lsa)
pta_embeddings[, identifier := rownames(pta_lsa)]
setcolorder(pta_embeddings, c("identifier", names(pta_embeddings)[!grepl("identifier", names(pta_embeddings))]))
names(pta_embeddings)[2:ncol(pta_embeddings)] <- gsub("V", "dtm_lsa_", names(pta_embeddings)[2:ncol(pta_embeddings)])

## DTM LSA -> PCA
pta_embeddings <- merge(pta_embeddings, pta_lsa_coords, by = "identifier", sort = F, all.x = T, all.y = F)
names(pta_embeddings)[52:ncol(pta_embeddings)] <- gsub("PC", "dtm_pca_", names(pta_embeddings)[52:ncol(pta_embeddings)])

## TCM-tf-weighted LSA
pta_embeddings <- merge(pta_embeddings, pta_tcm_tfweighted, by = "identifier", sort = F, all.x = T, all.y = F)
names(pta_embeddings)[54:ncol(pta_embeddings)] <- gsub("V", "tcm_lsa_", names(pta_embeddings)[54:ncol(pta_embeddings)])

## TCM-tf-weighted LSA -> PCA
pta_embeddings <- merge(pta_embeddings, pta_tcm_coords, by = "identifier", sort = F, all.x = T, all.y = F)
names(pta_embeddings)[104:ncol(pta_embeddings)] <- gsub("PC", "tcm_pca_", names(pta_embeddings)[104:ncol(pta_embeddings)])

## GloVe-tf-weighted
pta_embeddings <- merge(pta_embeddings, pta_vectors_tfweighted, by = "identifier", sort = F, all.x = T, all.y = F)
names(pta_embeddings)[106:ncol(pta_embeddings)] <- gsub("V", "glove_", names(pta_embeddings)[106:ncol(pta_embeddings)])

## GloVe-tf-weighted -> PCA
pta_embeddings <- merge(pta_embeddings, pta_vectors_coords, by = "identifier", sort = F, all.x = T, all.y = F)
names(pta_embeddings)[156:ncol(pta_embeddings)] <- gsub("PC", "glove_pca_", names(pta_embeddings)[156:ncol(pta_embeddings)])


##################
# PREPARATORY STEP 2
# Add the computed reductions to the trade flows data

# Use Correlates of War trade data
# http://www.correlatesofwar.org/data-sets/bilateral-trade

# Download and unzip to data folder
if(!file.exists("data/COW_Trade_4.0.zip")) {
	download.file(url = "http://www.correlatesofwar.org/data-sets/bilateral-trade/cow_trade_4.0/at_download/file", destfile = "data/COW_Trade_4.0.zip", mode = "wb")
}

# We need just one CSV with dyadic trade
if(!file.exists("data/Dyadic_COW_4.0.csv")) {
	unzip(zipfile = "data/COW_Trade_4.0.zip", files = "COW_Trade_4.0/Dyadic_COW_4.0.csv", exdir = "data/")
	file.copy("data/COW_Trade_4.0/Dyadic_COW_4.0.csv", "data/Dyadic_COW_4.0.csv")
	unlink("data/COW_Trade_4.0/", recursive = T)
}

# Read-in the file, missing values are coded as "-9"
dyadic_trade <- fread("data/Dyadic_COW_4.0.csv", logical01 = F, na.strings = c("", "-9"))

# To make the data set bilateral we need to mirror it
setorderv(dyadic_trade, c("ccode1", "ccode2"), order = c(1, 1))

first_part <- dyadic_trade[, c("ccode1", "ccode2", "year", "flow1")]
setnames(first_part, c("flow1"), c("imports"))
second_part <- dyadic_trade[, c("ccode1", "ccode2", "year", "flow2")]
setnames(second_part, c("ccode1", "ccode2", "flow2"), c("ccode2", "ccode1", "imports"))

bilateral_trade <- rbind(first_part, second_part, fill = T, use.names = T)
rm(first_part, second_part)
setnames(bilateral_trade, c("ccode1", "ccode2"), c("importer_cow", "exporter_cow"))

# Correlates of War codes into World Bank codes
bilateral_trade[, importer_wb := countrycode(importer_cow, origin = "cown", destination = "wb_api3c")]
bilateral_trade[, exporter_wb := countrycode(exporter_cow, origin = "cown", destination = "wb_api3c")]

# Data beautification
bilateral_trade <- bilateral_trade[year >= 1960 & year <= 2014 & !is.na(exporter_wb) & !is.na(importer_wb)]
setnames(bilateral_trade, c("exporter_wb", "importer_wb"), c("exporter", "importer"))
bilateral_trade[, c("importer_cow", "exporter_cow") := NULL]

# Add country pair-year -> PTA identifier mapping
unzip(zipfile = "data/trade_flow_pta_identifier_mapping.csv.zip", files = "trade_flow_pta_identifier_mapping.csv", exdir = "data/")
trade_flow_pta_identifier_mapping <- fread("data/trade_flow_pta_identifier_mapping.csv")
unlink("data/trade_flow_pta_identifier_mapping.csv")
bilateral_trade <- merge(bilateral_trade, trade_flow_pta_identifier_mapping, by = c("exporter", "importer", "year"), all.x = T, all.y = F, sort = F)
bilateral_trade[, identifier := as.character(identifier)]

# Add PTA embeddings
bilateral_trade_with_embeddings <- merge(bilateral_trade, pta_embeddings, by = "identifier", all.x = T, all.y = F, sort = F)

# Countries to factors
countries <- sort(unique(c(unique(bilateral_trade_with_embeddings$exporter), unique(bilateral_trade_with_embeddings$importer))))
bilateral_trade_with_embeddings[, exporter := factor(exporter, levels = countries)]
bilateral_trade_with_embeddings[, importer := factor(importer, levels = countries)]
bilateral_trade_with_embeddings[, year := as.factor(year)]

# Log of imports
bilateral_trade_with_embeddings[, ln_imports := log(imports + .0001)]

# Remove NAs in terms of trade flows
bilateral_trade_with_embeddings <- bilateral_trade_with_embeddings[!is.na(imports)]

# Clean-up
rm(list=ls()[!(ls() %in% c( "bilateral_trade_with_embeddings", "pta_texts"))])
gc()

##################
# ESTIMATING GRAVITY with PTA texts

# Define a model that accounts for multilateral resistance terms, and exporter/importer-year dummies
baseline_model <- as.formula(ln_imports ~ year | importer + exporter + importer:exporter)

# Now add various reductions of PTAs
dtm_lsa_model <- as.formula(paste0("ln_imports ~ ", paste0("dtm_lsa_", 1:50, collapse = " + "), "| importer + exporter + year + importer:exporter"))
dtm_pca_model <- as.formula(paste0("ln_imports ~ ", paste0("dtm_pca_", 1:2, collapse = " + "), "| importer + exporter + year + importer:exporter"))
tcm_lsa_model <- as.formula(paste0("ln_imports ~ ", paste0("tcm_lsa_", 1:50, collapse = " + "), "| importer + exporter + year + importer:exporter"))
tcm_pca_model <- as.formula(paste0("ln_imports ~ ", paste0("tcm_pca_", 1:2, collapse = " + "), "| importer + exporter + year + importer:exporter"))
glove_model <- as.formula(paste0("ln_imports ~ ", paste0("glove_", 1:50, collapse = " + "), "| importer + exporter + year + importer:exporter"))
glove_pca_model <- as.formula(paste0("ln_imports ~ ", paste0("glove_pca_", 1:2, collapse = " + "), "| importer + exporter + year + importer:exporter"))

# Estimate the models (conditional on trade flow covered by a PTA)
## Baseline
gravity_baseline <- felm(baseline_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 50 variables from LSA reduction of DTM matrix of PTA full texts
gravity_dtm_lsa <- felm(dtm_lsa_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 2 variables from LSA reduction of DTM matrix further reduced to two variables with PCA
gravity_dtm_pca <- felm(dtm_pca_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 50 variables from LSA reduction of TCM matrix of PTA full texts that was tf-averaged
gravity_tcm_lsa <- felm(tcm_lsa_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 2 variables from LSA reduction of TCM matrix of PTA full texts that was tf-averaged
## and further reduced to two variables with PCA
gravity_tcm_pca <- felm(tcm_pca_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 50 variables from GloVe reduction of TCM matrix of PTA full texts that was tf-averaged
gravity_glove <- felm(glove_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

## With 50 variables from GloVe reduction of TCM matrix of PTA full texts that was tf-averaged
## and further reduced to two variables with PCA
gravity_glove_pca <- felm(glove_pca_model, data = bilateral_trade_with_embeddings[!is.na(identifier)])
gc()

# Compute RMSE across the models
models_list <- c("baseline", "dtm_lsa", "dtm_pca", "tcm_lsa", "tcm_pca", "glove", "glove_pca")

model_evaluation <- data.table()

for(m in models_list) {
	#m <- "baseline"
	object <- get(paste0("gravity_", m))
	
	rmse <- sqrt(sum(object$residuals^2)/(gravity_baseline$N - gravity_baseline$p))
	model_evaluation <- rbind(model_evaluation, data.table(model = m, rmse = rmse))
}
 
#
# model_evaluation
#       model     rmse
#1:  baseline 1.579021
#2:   dtm_lsa 1.519052
#3:   dtm_pca 1.535121
#4:   tcm_lsa 1.516889
#5:   tcm_pca 1.534912
#6:     glove 1.515493
#7: glove_pca 1.534912
(1-min(model_evaluation$rmse)/max(model_evaluation$rmse))*100 # + 4.02%

# How would trade between two nations look like had there been an agreement
# similar in design to NAFTA between them?
glove_coefficients <- as.matrix(gravity_glove$coefficients)

nafta_vector <- as.matrix(bilateral_trade_with_embeddings[identifier == 112, paste0("glove_", 1:50)][1])

nafta_vector %*% glove_coefficients

# What about Canada-USA FTA?
cufta_vector <- as.matrix(bilateral_trade_with_embeddings[identifier == 186, paste0("glove_", 1:50)][1])

cufta_vector %*% glove_coefficients

# Why does NAFTA seem to have a lower effect here?

# In principle we can rank all agreements:
agreement_rank <- data.table()

for(id in unique(bilateral_trade_with_embeddings$identifier)) {
	pta_vector <- as.matrix(bilateral_trade_with_embeddings[identifier == id, paste0("glove_", 1:50)][1])
	
	agreement_rank <- rbind(agreement_rank, data.table(identifier = id, value = (exp(as.numeric(pta_vector %*% glove_coefficients))-1)*100))

}

agreement_rank <- agreement_rank[!is.na(value)]
setorderv(agreement_rank, "value", -1)
agreement_rank[, name := pta_texts[match(agreement_rank$identifier, pta_texts$identifier)]$rta_name]
agreement_rank[, date := pta_texts[match(agreement_rank$identifier, pta_texts$identifier)]$rta_date_signed]

agreement_rank[1:20]
agreement_rank[(nrow(agreement_rank)-20):nrow(agreement_rank)]

# This rank might seem a bit divorced from reality 
# Assumption: equate to zeros PTA characteristics for flows not under PTAs
bilateral_trade_with_embeddings[is.na(identifier), names(bilateral_trade_with_embeddings)[6:ncol(bilateral_trade_with_embeddings)-1] := 0]

# Estimate full model on all trade flows. NB: takes 7GB of RAM
# and ~30 minutes of CPU time
#gravity_glove_full <- felm(glove_model, data = bilateral_trade_with_embeddings)
#save(gravity_glove_full, file = "~/gravity_glove_full.rdata", compress = "xz")
load("~/gravity_glove_full.rdata")

# Redefine text coefficients and re-compute the above ranking
glove_coefficients <- as.matrix(gravity_glove_full$coefficients)

agreement_rank[grepl("US", name)]
agreement_rank[grepl("NAFTA", name)]
