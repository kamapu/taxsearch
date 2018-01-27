# TODO:   Automatic import of Gentry's tables
# 
# Author: Miguel Alvarez
################################################################################

import_gentry <- function(path, ...) {
	Files <- list.files(path, ".xls")
	Names <- sub(".xls", "", Files, fixed=TRUE)
	Table <- list()
	Entries <- list()
	Vouchers <- list()
	Individuals <- list()
	pb <- tkProgressBar(min=0, max=length(Files), width=300)
	for(i in 1:length(Files)) {
		Sys.sleep(0.1)
		setTkProgressBar(pb, i, title=paste0("Processing '", Names[i],
						"' (", i, " of ", length(Files), ")"))
		Table[[i]] <- read_xls(file.path(path, Files[i]), ...)
		Entries[[i]] <- data.frame(EntryID=paste(Names[i],
						c(1:nrow(Table[[i]])), sep="_"),
				Transect=tolower(Names[i]),
				Table[[i]][,1:4], stringsAsFactors=FALSE)
		colnames(Entries[[i]])[3:6] <- c("Line", "Family","Genus","Species")
		IDvoucher <- grep("voucher", colnames(Table[[i]]), ignore.case=TRUE)
		Vouchers[[i]] <- data.frame(EntryID=rep(Entries[[i]]$EntryID,
						length(IDvoucher)),
				VoucherNr=unlist(Table[[i]][,IDvoucher]),
				stringsAsFactors=FALSE)
		Vouchers[[i]] <- Vouchers[[i]][!is.na(Vouchers[[i]]$VoucherNr),]
		Entries[[i]]$Habit <- Table[[i]][,(max(IDvoucher) + 1), drop=TRUE]
		IDvoucher <- c((max(IDvoucher) + 3):ncol(Table[[i]]))
		Individuals[[i]] <- data.frame(EntryID=rep(Entries[[i]]$EntryID,
						length(IDvoucher)), BHD=unlist(Table[[i]][,IDvoucher]),
				stringsAsFactors=FALSE)
		Individuals[[i]] <- Individuals[[i]][!is.na(Individuals[[i]]$BHD),]
	}
	close(pb)
	Out <- list(entries=do.call(rbind, Entries),
			samples=do.call(rbind, Individuals),
			vouchers=do.call(rbind, Vouchers))
	# Generate a key for entry ids
	Out$vouchers$EntryID <- c(1:nrow(Out$entries))[match(Out$vouchers$EntryID,
					Out$entries$EntryID)]
	Out$samples$EntryID <- c(1:nrow(Out$entries))[match(Out$samples$EntryID,
					Out$entries$EntryID)]
	Out$entries$EntryID <- c(1:nrow(Out$entries))
	# Prepare header table
	Out$header <- data.frame(ReleveID=c(1:length(unique(Out$entries$Transect))),
			ReleveName=unique(Out$entries$Transect))
	Out$samples$ReleveID <- Out$entries$Transect[match(Out$samples$EntryID,
					Out$entries$EntryID)]
	Out$samples$ReleveID <- Out$header$ReleveID[match(Out$samples$ReleveID,
					Out$header$ReleveName)]
	Out$samples <- Out$samples[,c("ReleveID","EntryID","BHD")]
	return(Out)
}
