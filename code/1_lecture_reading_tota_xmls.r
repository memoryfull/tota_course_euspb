# This code will query selected ToTA XMLs
# from GitHub and conduct basic processing
#
# Dmitriy Skougarevskiy, 4 April 2018

# Load/install dependencies
packages <- c("data.table", "stringi", "stringr", "lubridate", "httr", "xml2", "countrycode")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
	
	install.packages(setdiff(packages, rownames(installed.packages())))
	
}

capture.output(lapply(packages, library, character.only = T), file='NUL')

# Declare working directory beforehand in an environment variable
# TOTA_COURSE_REPO = "path_to_your_repository"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect

# Go to the declared working directory
setwd(Sys.getenv('TOTA_COURSE_REPO'))

#############
# Download selected XMLs from master branch of ToTA
# to our local data/ folder -- create it beforehand:
dir.create("data/")

# NB: download is done for illustrative purposes;
# use git clone to access the entire repository
# in any production code.

# List of PTA IDs of interest to access
# 449 - English-language Trans-Pacific Partnership (signed version)
# 270 - Hungary-Latvia FTA (1999)
ptas_of_interest <- c(449, 270)

lapply(ptas_of_interest, function(x) {
		
		temp <- xml2::read_xml(paste0("https://raw.githubusercontent.com/mappingtreaties/tota/master/xml/pta_", x, ".xml"))
		write_xml(temp, file = paste0("data/pta_", x, ".xml"))
		rm(temp)
		
	}
)

#############
# Process the metadata of downloaded XMLs

# Load the Trans-Pacific Partnership text
tpp_xml <- read_xml("data/pta_449.xml")

# Examine it
tpp_xml
str(tpp_xml)
xml2::xml_structure(tpp_xml)

# When was it signed?
xml2::xml_find_first(tpp_xml, "/treaty/meta/date_signed")  # still a node

## Convert to character: option 1
xml2::xml_text(xml_find_first(tpp_xml, "/treaty/meta/date_signed"))

## Convert to character: option 2, more verbose
tpp_date_signed <- as.character(xml_find_first(tpp_xml, "/treaty/meta/date_signed/text()")) 

# Which year was that?
## If dates are well-formed (our case)
as.numeric(substr(tpp_date_signed, 1, 4))

## If dates are not well-formed and all we know is
## that they are in year-month-day order
as.numeric(lubridate::year(lubridate::ymd(tpp_date_signed)))
as.numeric(year(ymd("2016 February 3")))

# Who were the signatories?
## Access the inner text of the  nodes
tpp_parties_nodes <- xml_find_all(tpp_xml, "/treaty/meta/parties_original/partyisocode/text()")

## To list with the aid of as_list (not to confuse with as.list)
tpp_parties <- xml2::as_list(tpp_parties_nodes)

## Flatten the list to a character vector
tpp_parties <- base::unlist(tpp_parties)

# What are the full names of the signatories?
## Convert from ISO 3166-1 alpha-3 country codes
## to full names
tpp_parties_full_names <- countrycode::countrycode(tpp_parties, origin = "iso3c", destination = "country.name.en")

## If you are a Francophone
countrycode(tpp_parties, origin = "iso3c", destination = "country.name.fr")

#############
# Load the PTA texts of downloaded XMLs

# Extract all articles of TPP Chapter 9 - Investment
# with XPath
xml_find_all(tpp_xml, "/treaty/body/chapter[contains(@name,'Investment')]/article")
xml_find_all(tpp_xml, "/treaty/body/chapter[contains(@name,'Investment')]/article/text()")

# Convert to list with attributes indicating article names
tpp_investment_articles_list <- as_list(xml_find_all(tpp_xml, "/treaty/body/chapter[contains(@name,'Investment')]/article"))

str(tpp_investment_articles_list)

# Can organise article texts in a data.table
tpp_investment_articles <- data.table(article_text = unlist(tpp_investment_articles_list))

# But how to obtain the names and numbers of articles
# that were stored as node attributes?
attr(tpp_investment_articles_list[[1]], "name") # this works (note the `[[`)

# Vectorise and add the resulting column
tpp_investment_articles[, article_name := lapply(tpp_investment_articles_list, attr, "name")]

# Same for article number
tpp_investment_articles[, article_number := lapply(tpp_investment_articles_list, attr, "number")]

# What to do if one of the articles has no attribute?
# Replace 
# <article number="ARTICLE 9.1" name="Definitions" article_identifier="40107">
# with 
# <article number="ARTICLE 9.1" article_identifier="40107">
# in data/pta_449.xml, re-read the XML and see for yourself.

# Now undo the changes in data/pta_449.xml and write
# <article number="ARTICLE 9.1" name="Definitions & Test" article_identifier="40107">
# instead. Try to parse the xml: read_xml("data/pta_449.xml")

# Your R will crash. This is due to an ill-formed XML:
# ampersand is a special symbol and should be written as &amp;
# You can validate your XML at https://validator.w3.org to
# locate the error.

#############
# Examine the PTA texts

# Rely on the aforementioned data.table 
tpp_investment_articles

# Count numbers of words
tpp_investment_articles[, word_count := stringr::str_count(gsub("\\s+", " ", gsub("\n|\r", " ", article_text)), " ")]

# Log-density of word counts
plot(density(log(tpp_investment_articles$word_count)))

# Which articles is on MFN?
tpp_investment_articles[grepl("treatment", article_text)]

# Compare the complexity of provisions of two agreements
hun_lva_xml <- read_xml("data/pta_270.xml")

tpp_article_names <- xml_text(xml_find_all(tpp_xml, "/treaty/body/chapter/article/@name"))
hun_lva_article_names <- xml_text(xml_find_all(hun_lva_xml, "/treaty/body/chapter/article/@name")) 

length(tpp_article_names); length(hun_lva_article_names)

# Homework: store all ToTA article texts in one data.table where each row is one article
# and all treaty metadata is preserved in respective columns
