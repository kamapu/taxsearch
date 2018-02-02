# TODO:   Query to specimens in Tropicos
# 
# Author: Miguel Alvarez
################################################################################

# Modular programming

# 1: Function reading collector and number
read_collector <- function(collector, number, key, encoding) {
	Page <- read_html(paste0(
					"http://services.tropicos.org/Specimen/Search?seniorcollector=",
					collector, "&collectionnumber=", number, "&apikey=", key,
					"&format=xml"), encoding=encoding)
	return(Page)
}

# 2: Function reading specimen number
read_specimen <- function(specimen, key, encoding) {
	Page <- read_html(paste0("http://services.tropicos.org/Specimen/", specimen,
					"?apikey=", key, "&format=xml"), encoding=encoding)
	return(Page)
}

# 3: Conversion to data.frame
xml2df <- function(x) {
	if(!missing(x)) {
		Table <- list(
				specimen_id=xml_text(xml_find_all(x,
								"//specimenid"), trim=TRUE),
				species=xml_text(xml_find_all(x,
								"//determinationfullnamenoauthors"), trim=TRUE),
				fullname=xml_text(xml_find_all(x,
								"//determinationfullname"), trim=TRUE),
				det=xml_text(xml_find_all(x,
								"//determinedby"), trim=TRUE),
				collector=xml_text(xml_find_all(x,
								"//collectorstring"), trim=TRUE),
				collection_nr=xml_text(xml_find_all(x,
								"//collectionnumber"), trim=TRUE),
				day=xml_text(xml_find_all(x,
								"//collectionday"), trim=TRUE),
				month=xml_text(xml_find_all(x,
								"//collectionmonth"), trim=TRUE),
				year=xml_text(xml_find_all(x,
								"//collectionyear"), trim=TRUE),
				country=xml_text(xml_find_all(x,
								"//countryname"), trim=TRUE),
				locality=xml_text(xml_find_all(x,
								"//locality"), trim=TRUE),
				habitat=xml_text(xml_find_all(x,
								"//habitat"), trim=TRUE),
				vegetation=xml_text(xml_find_all(x,
								"//vegetation"), trim=TRUE),
				longitude=xml_text(xml_find_all(x,
								"//longitudedecdeg"), trim=TRUE),
				latitude=xml_text(xml_find_all(x,
								"//latitudedecdeg"), trim=TRUE),
				elevation=xml_text(xml_find_all(x,
								"//elevationdisplay"), trim=TRUE))
		Table[sapply(Table, length) == 0] <- NA
	} else {
		Table <- list(
				specimen_id=NA,
				species=NA,
				fullname=NA,
				det=NA,
				collector=NA,
				collection_nr=NA,
				day=NA,
				month=NA,
				year=NA,
				country=NA,
				locality=NA,
				habitat=NA,
				vegetation=NA,
				longitude=NA,
				latitude=NA,
				elevation=NA)
	}
	return(as.data.frame(Table, stringsAsFactors=FALSE))
}

# 4: Global function
tropicos_specimens <- function(key, collector, number, specimen,
		mode="collector", encoding="UTF-8") {
	if(missing(key))
		stop("You require an API key for this search (request it at http://services.tropicos.org/help?requestkey)")
	mode <- match.arg(tolower(mode), c("collector", "specimen"))
	OUT <- list()
	if(mode == "collector") {
		if(length(collector) == 1) collector <- rep(collector, length(number))
		pb <- tkProgressBar(min=0, max=length(number), width=300)
		for(i in 1:length(number)) {
			Sys.sleep(0.1)
			setTkProgressBar(pb, i, title=paste0("Entry ", i, " of ",
							length(number), " (", round(i/length(number)*100),
							"% done)"))
			Table <- read_collector(collector[i], number[i], key, encoding)
			if(length(xml_find_all(Table, "//specimen")) > 1) {
				Nr <- xml_text(xml_find_all(Table, "//collectionnumber"))
				ID <- xml_text(xml_find_all(Table, "//specimenid"))
				ID <- ID[Nr == paste(number[i])]
				if(length(ID) >= 1) {
					Table <- read_specimen(ID[1], key, encoding)
					if(length(xml_find_all(Table, "//error")) > 0) {
						Table <- xml2df()
					} else {
						Table <- xml2df(Table)
					}
				} else {
					Table <- xml2df()
				}
			} else {
				if(length(xml_find_all(Table, "//error")) > 0) {
					Table <- xml2df()
				} else {
					Table <- read_specimen(xml_text(xml_find_all(Table,
											"//specimenid")), key, encoding)
					if(length(xml_find_all(Table, "//error")) > 0) {
						Table <- xml2df()
					} else {
						Table <- xml2df(Table)
					}
				}
			}
			OUT[[i]] <- Table
		}
		close(pb)
	}
	if(mode == "specimen") {
		pb <- tkProgressBar(min=0, max=length(specimen), width=300)
		for(i in 1:length(specimen)) {
			Sys.sleep(0.1)
			setTkProgressBar(pb, i, title=paste0("Entry ", i, " of ",
							length(specimen), " (",
							round(i/length(specimen)*100), "% done)"))
			Table <- read_specimen(specimen[i], key, encoding)
			if(length(xml_find_all(Table, "//error")) > 0) {
				Table <- xml2df()
			} else {
				Table <- xml2df(Table)
			}
			OUT[[i]] <- Table
		}
		close(pb)
	}
	return(do.call(rbind, OUT))
}
