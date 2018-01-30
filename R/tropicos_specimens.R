# TODO:   Query to specimens in Tropicos
# 
# Author: Miguel Alvarez
################################################################################

# browseURL("http://www.tropicos.org/LinkHelp.aspx")

tropicos_specimens <- function(Home="http://www.tropicos.org/SpecimenSearch.aspx",
		key, collector, number, barcode, accession, specimen, mode="collector",
		encoding="UTF-8") {
	if(missing(key))
		stop("You require an API key for this search (request it at http://services.tropicos.org/help?requestkey)")
	# Mode selection -----------------------------------------------------------
	mode <- match.arg(tolower(mode), c("collector","barcode","accession",
					"specimen"))
	if(mode == "collector") {
		if(length(collector) == 1 & length(number) > 1)
			collector <- rep(collector, length(number))
		Home2 <- paste0(Home, "?sen=", collector, "&num=", number)
	}
	if(mode == "barcode")
		Home2 <- paste0(Home, "?barcode=", barcode)
	if(mode == "accession")
		Home2 <- paste0(Home, "?accession=", accession)
	if(mode == "specimen")
		Home2 <- paste0(sub("Search.aspx", "", Home), "/", specimen)
	# End: Mode selection ------------------------------------------------------
	OUT <- list()
	pb <- tkProgressBar(min=0, max=length(Home2), width=300)
	for(i in 1:length(Home2)) {
		Sys.sleep(0.1)
		setTkProgressBar(pb, i, title=paste0("Entry ", i, " of ", length(Home2),
						" (", round(i/length(Home2)*100), "% done)"))
		Page <- read_html(paste0(Home2[i], "&apikey=", key, "&format=xml"),
				encoding=encoding)
		Name <- html_nodes(Page,
				"tr:nth-child(11) td , tr:nth-child(10) td, tr:nth-child(9) td, tr:nth-child(8) td, tr:nth-child(7) td, tr:nth-child(5) td, tr:nth-child(4) td, #details tr:nth-child(1) td, #ctl00_MainContentPlaceHolder_specimenDetailsTabControl_FullNameLink")
		Name <- html_text(Name, trim=TRUE)
		ID <- NA
		if(length(Name) > 0 & !"Collectors" %in% Name) {
			Table <- html_nodes(Page,
					"#ctl00_MainContentPlaceHolder_specimenSearchControl_gridView")
			Table <- html_table(Table, fill=TRUE)[[1]]
			ID <- which(Table$"Coll No" == number[i])[1] + 1
			if(ID < 10) ID <- paste0("0", ID) else ID <- paste(ID)
			ID <- paste0("#ctl00_MainContentPlaceHolder_specimenSearchControl_gridView_ctl",
					ID, "_SpecimenLink")
			ID <- html_attr(html_node(Page, ID), "href")
			ID <- sub("/Specimen/", "", ID)
			Home2[i] <- paste0(sub("/SpecimenSearch.aspx",
							"/SpecimenPage.aspx?specimenid=", Home), ID)
			Page <- read_html(paste0(Home2[i], "&apikey=", key, "&format=xml"),
					encoding=encoding)
			Name <- html_nodes(Page,
					"tr:nth-child(11) td , tr:nth-child(10) td, tr:nth-child(9) td, tr:nth-child(8) td, tr:nth-child(7) td, tr:nth-child(5) td, tr:nth-child(4) td, #details tr:nth-child(1) td, #ctl00_MainContentPlaceHolder_specimenDetailsTabControl_FullNameLink")
			Name <- html_text(Name, trim=TRUE)
		}
		## Name <- data.frame(
		##         taxon_name=Name[1],
		##         collectors=Name[which(Name == "Collectors") + 1],
		##         specimenid=ID,
		##         date=Name[which(Name == "Collection Date") + 1],
		##         location=paste(Name[which(Name == "Location") + 1],
		##                 Name[which(Name == "Locality") + 1]),
		##         coordinates=Name[which(Name == "Coordinate") + 1],
		##         elevation=Name[which(Name == "Elevation") + 1],
		##         stringsAsFactors=FALSE)
		## 
		Name <- list(
				taxon_name=Name[1],
				collectors=Name[which(Name == "Collectors") + 1],
				specimenid=ID,
				date=Name[which(Name == "Collection Date") + 1],
				location=paste(Name[which(Name == "Location") + 1],
						Name[which(Name == "Locality") + 1]),
				coordinates=Name[which(Name == "Coordinate") + 1],
				elevation=Name[which(Name == "Elevation") + 1])
		Name[sapply(Name, length) == 0] <- NA
		Name <- as.data.frame(Name, stringsAsFactors=FALSE)
		OUT[[i]] <- Name
	}
	close(pb)
	names(OUT) <- NULL
	OUT <- as.data.frame(do.call(rbind, OUT), stringsAsFactors=FALSE)
	OUT$uri <- Home2
	return(OUT)
}
