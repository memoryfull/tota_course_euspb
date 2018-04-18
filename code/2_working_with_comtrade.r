# This code will query COMTRADE API
# and estimate basic gravity models
# NB: code relies on https://comtrade.un.org/data/Doc/api/ex/r
#
# Dmitriy Skougarevskiy, 4 April 2018

# Load/install dependencies
packages <- c("data.table", "stringi", "stringr", "lubridate", "httr", "xml2", "countrycode", "rjson", "wbstats", "lfe", "readxl")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
	
	install.packages(setdiff(packages, rownames(installed.packages())))
	
}

capture.output(lapply(packages, library, character.only = T), file='NUL')

# Go to the declared working directory
setwd(Sys.getenv('TOTA_COURSE_REPO'))

#############
# Query a JSON with all reporters of trade flows
reporters_json <- fromJSON(file = "http://comtrade.un.org/data/cache/partnerAreas.json")

# Convert to data table with UN reporter Code and country full name
reporters <- as.data.table(t(sapply(reporters_json$results,rbind)))
reporters <- reporters[, lapply(.SD, unlist)]
names(reporters) <- c("uncode", "full_name")

# Add ISO3 codes
reporters[, iso := countrycode(uncode, origin = "un", destination = "iso3c")]

# There are quite a few problems due to country dissolution or changes in definitions:
reporters[is.na(iso)]

# Fix some of them by hand
reporters[ uncode == "58", iso := "BEL"] # Belgium-Le
reporters[ uncode == "200", iso := "CZE"] # Czechoslovakia
reporters[ uncode == "842", iso := "USA"] # USA
reporters[ uncode == "841", iso := "USA"] # USA
reporters[ uncode == "757", iso := "CHE"] # Switzerland
reporters[ uncode == "381", iso := "ITA"] # Italy
reporters[ uncode == "699", iso := "IND"] # India
reporters[ uncode == "251", iso := "FRA"] # France
reporters[ uncode == "280", iso := "DEU"] # Germany


#############
# Create a function to query reporter-partner trade flows
# Adopted from https://comtrade.un.org/data/Doc/api/ex/r
# Returns a data table with selected trade data
# Example usage get.Comtrade(maxrec=50000, type="C", freq="A", px="HS", ps="now", r= "757", p= "251", rg="all", cc="TOTAL")

get.Comtrade <- function(maxrec=50000	# maximum no. of records returned
							,type="C"		# type of trade (c=commodities)
							,freq="A"		# frequency (A=annual)
							,px="HS"		# classification (Harmonised System)
							,ps="now"		# time period
							,r				# reporting area
							,p				# partner country
							,rg="all"		# trade flow ((re)import/(re)export/)
							,cc="TOTAL"		# classification code for trade flow type
						) {
	
	# Debug UK-ROW trade: maxrec=50000; type="C"; freq="A"; px="S1"; ps="1962,1963,1964,1965,1966"; r="ALL"; p="826"; rg="1"; cc="TOTAL"
	 
	# Url to query results from
	url_to_query <- paste("http://comtrade.un.org/api/get?max=", maxrec, "&type=", type, "&freq=", freq, "&px=", px, "&ps=", ps, "&r=", r, "&p=", p, "&rg=", rg, "&cc=", cc,"&fmt=json", sep = "")

	# Query the data from API
	response <- httr::GET(url_to_query)
	response_json <- httr::content(response)

	response_data <- response_json$dataset
	
	if(length(response_data) > 0) {

		var.names <- names(response_data[[1]])
		response_data <- as.data.frame(t(sapply(response_data, rbind)))
		names(response_data) <- var.names
		
		setDT(response_data)

		# Flatten lists
		response_data <- response_data[, lapply(.SD, as.character) ]

		# NULLs to NAs
		for (j in seq_len(ncol(response_data))) {
			set(response_data, which(response_data[[j]] == "NULL"), j, NA)
		}
		
		as.data.frame(response_data[,11])

	} else {
		response_data <- data.table(rtCode = r)
	}
	
	return(response_data)
	
}

#############
# Query reporter-partner trade flows yourself
# NB: don't over-use this function as you may be
# hit by API query limits (1 request per second,
# 100 requests per hour)

# As per https://comtrade.un.org/db/mr/daReportersResults.aspx,
# the best classification to use is SITC.1

# Example: query imports from UK in 1962-1966
# to all other countries
imports_from_uk <- get.Comtrade(maxrec=50000, type="C", freq="A", px="S1", ps="1962,1963,1964,1965,1966", r = "ALL", p = "826", rg="1", cc="TOTAL")

imports_from_uk[yr == 1963]

#############
# Entire COMTRADE size: https://comtrade.un.org/db/dqBasicQueryResults.aspx?cc=all&px=HS&so=9999&rpage=dqBasicQuery&qt=n
# "The query will return 531137564 records."
# "* Estimated file size: 64,998.71 MB."
# NB: and these are only non-zero trade-flows!

# We use Correlates of War trade data instead (v 4)
# http://www.correlatesofwar.org/data-sets/bilateral-trade
# NB: for any economic research I advise using COMTRADE
# or CEPII's Gravity (http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=8)

# Download and unzip to data folder
if(!file.exists("data/COW_Trade_4.0.zip")) {
	download.file(url = "http://www.correlatesofwar.org/data-sets/bilateral-trade/cow_trade_4.0/at_download/file", destfile = "data/COW_Trade_4.0.zip" )
}

# We need just one CSV with dyadic trade
if(!file.exists("data/Dyadic_COW_4.0.csv")) {
	unzip(zipfile = "data/COW_Trade_4.0.zip", files = "COW_Trade_4.0/Dyadic_COW_4.0.csv", exdir = "data/")
	file.copy("data/COW_Trade_4.0/Dyadic_COW_4.0.csv", "data/Dyadic_COW_4.0.csv")
	unlink("data/COW_Trade_4.0/", recursive = T)
}

# Read-in the file, missing values are coded as "-9"
dyadic_trade <- fread("data/Dyadic_COW_4.0.csv", logical01 = F, na.strings = c("", "-9"))

# "flow1 is Imports of Country A from Country B, in US millions of current dollars"
# "flow2 is Imports of Country B from Country A, in US millions of current dollars"

# Canada imported from United States in 2014 -- USD276158.28M (flow 2):
dyadic_trade[ccode1 == 2 & ccode2 == 20 & year == 2014]

# To make the data set bilateral we need to mirror it
setorderv(dyadic_trade, c("ccode1", "ccode2"), order = c(1, 1))

first_part <- dyadic_trade[, c("ccode1", "ccode2", "year", "flow1")]
setnames(first_part, c("flow1"), c("imports"))
second_part <- dyadic_trade[, c("ccode1", "ccode2", "year", "flow2")]
setnames(second_part, c("ccode1", "ccode2", "flow2"), c("ccode2", "ccode1", "imports"))

bilateral_trade <- rbind(first_part, second_part, fill = T, use.names = T)
rm(first_part, second_part)
setnames(bilateral_trade, c("ccode1", "ccode2"), c("importer_cow", "exporter_cow"))

# Canada imported from United States in 2014:
bilateral_trade[importer_cow == 20 & exporter_cow == 2 & year == 2014]

# Correlates of War codes into World Bank codes
bilateral_trade[, importer_wb := countrycode(importer_cow, origin = "cown", destination = "wb_api3c")]
bilateral_trade[, exporter_wb := countrycode(exporter_cow, origin = "cown", destination = "wb_api3c")]

# Query GDP in current USD from World Bank
gdp_data <- wb(indicator = "NY.GDP.MKTP.CD", startdate = 1960, enddate = 2014)
setDT(gdp_data)
setnames(gdp_data, c("value"), c("gdp"))
gdp_data[, date := as.integer(date)]

# Add GDP data to trade flows
## For importer
bilateral_trade <- merge(bilateral_trade, gdp_data[,c("iso3c", "date", "gdp")], by.x = c("importer_wb", "year"), by.y = c("iso3c", "date"), all.x = T, all.y = F, sort = F)
setnames(bilateral_trade, c("gdp"), c("importer_gdp"))
## For exporter
bilateral_trade <- merge(bilateral_trade, gdp_data[,c("iso3c", "date", "gdp")], by.x = c("exporter_wb", "year"), by.y = c("iso3c", "date"), all.x = T, all.y = F, sort = F)
setnames(bilateral_trade, c("gdp"), c("exporter_gdp"))

# Add distances between countries from CEPII
if(!file.exists("data/dist_cepii.xls")) {
	download.file(url = "http://www.cepii.fr/distance/dist_cepii.zip", destfile = "data/dist_cepii.zip" )
	unzip(zipfile = "data/dist_cepii.zip", files = "dist_cepii.xls", exdir = "data/")
}

dist_cepii <- readxl::read_excel("data/dist_cepii.xls")
setDT(dist_cepii)

# World Bank codes
dist_cepii[, wb_o := countrycode(iso_o, origin = "iso3c", destination = "wb_api3c")]
dist_cepii[, wb_d := countrycode(iso_d, origin = "iso3c", destination = "wb_api3c")]

## For importer
bilateral_trade <- merge(bilateral_trade, dist_cepii[, c("iso_o", "iso_d", "dist", "comlang_off",  "colony", "contig")], by.x = c("exporter_wb", "importer_wb"), by.y = c("iso_o", "iso_d"), all.x = T, all.y = F, sort = F)

# Final data beautification
bilateral_trade <- bilateral_trade[year >= 1960 & year <= 2014 & !is.na(exporter_wb) & !is.na(importer_wb)]
setnames(bilateral_trade, c("exporter_wb", "importer_wb"), c("exporter", "importer"))
bilateral_trade[, c("importer_cow", "exporter_cow") := NULL]
# GDP in Million USD
bilateral_trade[, importer_gdp := importer_gdp/1e6] 
bilateral_trade[, exporter_gdp := exporter_gdp/1e6] 
bilateral_trade[, importer := as.factor(importer)]
bilateral_trade[, exporter := as.factor(exporter)]

# Distance in thousand kilometres
bilateral_trade[, dist := dist/1e3]

# Shape of the data at the end of the day
bilateral_trade[importer == "CAN" & exporter == "RUS"]

# Note that we do not handle zero trade flows properly yet

#############
# Estimate gravity regressions

# Log of imports plus a small value:  dependent variable
bilateral_trade[, ln_imports := log(imports + .0001)]

# Log of GDP and distance
bilateral_trade[, ln_importer_gdp := log(importer_gdp)]
bilateral_trade[, ln_exporter_gdp := log(exporter_gdp)]
bilateral_trade[, ln_dist := log(dist)]

bilateral_trade[, year := as.factor(year)]

# Very naive model
naive_gravity <- lfe::felm(ln_imports ~ ln_importer_gdp + ln_exporter_gdp + ln_dist + contig + comlang_off + colony, data = bilateral_trade) 
summary(naive_gravity)

100*(1.10^coef(naive_gravity)["ln_importer_gdp"]-1)	# 10% increase in importer GDP is associated with 10.3% increase in imports
100*(1.10^coef(naive_gravity)["ln_exporter_gdp"]-1)	# 10% increase in exporter GDP is associated with 13.3% increase in exports
100*(1.10^coef(naive_gravity)["ln_dist"]-1)			# One thousand-kilometre increase in distance is associated with 24% decrease in imports 
100*(coef(naive_gravity)["colony"])					# This seems improbable

# Very naive model with country FE
naive_gravity_imp_exp_fe <- felm(ln_imports ~ ln_importer_gdp + ln_exporter_gdp + ln_dist + contig + comlang_off + colony | importer + exporter, data = bilateral_trade) 
summary(naive_gravity_imp_exp_fe)

100*(1.10^coef(naive_gravity_imp_exp_fe)["ln_importer_gdp"]-1)
100*(1.10^coef(naive_gravity_imp_exp_fe)["ln_exporter_gdp"]-1)
100*(1.10^coef(naive_gravity_imp_exp_fe)["ln_dist"]-1)
100*(coef(naive_gravity_imp_exp_fe)["colony"])

# Still naive model with year FE
naive_gravity_imp_exp_year_fe <- felm(ln_imports ~ ln_importer_gdp + ln_exporter_gdp + ln_dist + contig + comlang_off + colony | importer + exporter + year, data = bilateral_trade)
summary(naive_gravity_imp_exp_year_fe)

100*(1.10^coef(naive_gravity_imp_exp_year_fe)["ln_importer_gdp"]-1)
100*(1.10^coef(naive_gravity_imp_exp_year_fe)["ln_exporter_gdp"]-1)
100*(1.10^coef(naive_gravity_imp_exp_year_fe)["ln_dist"]-1)
100*(coef(naive_gravity_imp_exp_year_fe)["colony"])

# Accounting for multilateral resistance terms
gravity_imp_exp_fe <- felm(ln_imports ~ ln_importer_gdp + ln_exporter_gdp | importer + exporter + year + importer:exporter, data = bilateral_trade) 
summary(gravity_imp_exp_fe)

100*(1.10^coef(gravity_imp_exp_fe)["ln_importer_gdp"]-1)
100*(1.10^coef(gravity_imp_exp_fe)["ln_exporter_gdp"]-1)

# Accounting for multilateral resistance terms, and importer-year dummies
gravity_imp_exp_year_fe <- felm(ln_imports ~ ln_exporter_gdp | importer + exporter + year + importer:exporter + importer:year, data = bilateral_trade)
summary(gravity_imp_exp_year_fe)

100*(1.10^coef(gravity_imp_exp_year_fe)["ln_exporter_gdp"]-1)

##################
# Homework: does gravity hold for trade in services? Why?
# query the data on services and construct similar gravity panel
# Example: https://comtrade.un.org/api/get?max=50000&type=S&freq=A&px=EB02&ps=2012&r=ALL&p=826&rg=1&cc=ALL&fmt=json
# Adjust get.Comtrade() function accordingly
# NB: API response is buggy when it comes to type = S
# NB: Data availability is here: https://comtrade.un.org/api/refs/da/view?type=S&px=EB02&r=36&p=2
