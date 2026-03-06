


# library(openxlsx2)
library(readODS)

# Read Words from ODS-file
import.words = function(file = "Words.Med.SARS.ods", sheets = NULL, verbose = TRUE) {
	if(is.null(sheets)) {
		sheets = c("Nouns", "Drugs", "Chem", "Other", "Abbr",
			"Genes", "CellLines", "Names"
			);
	}
	x = c();
	for(sh in sheets) {
		ww = read_ods(file, sheet = sh, col_names = TRUE, as_tibble = FALSE);
		ww = ww$Word;
		if(verbose && length(ww) == 0) {
			cat("Missing words in sheet: ", sh, "!\n", sep = "");
		}
		x  = c(x, ww);
	}
	x = x[! is.na(x)];
	x = x[nchar(x) > 0];
	if(verbose) {
		len = length(x);
		cat("Found ", len, " words.\n", sep = "");
	}
	return(x);
}

# Save Words to CSV-file
write.words = function(x, file = "Words.csv") {
	if(! inherits(x, "data.frame")) {
		x = data.frame(Word = x);
	}
	write.csv(x, file = file, row.names = FALSE);
	invisible();
}

### UTF Codes:
which.utf = function(x) {
	stringi::stri_escape_unicode(x);
}

### Upper Case:
toupper.word = function(x) {
	LEN = length(x);
	if(LEN == 0) return(x);
	if(LEN == 1) {
		if(nchar(x) < 1) return(x);
		substr(x, 1, 1) = toupper(substr(x, 1, 1));
		return(x);
	}
	isW = (! is.na(x)) & (nchar(x) > 0);
	idW = which(isW);
	for(id in idW) {
		substr(x[id], 1, 1) = toupper(substr(x[id], 1, 1));
	}
	return(x);
}

### Trim Abstracts
# - remove Extra-Info;
trim.extra = function(x) {
	# \\u00a9: (C)
	reg = paste("\u00a9", "Copyright",
		"This journal is \u00a9", "Communicated by ", sep = "|");
	reg = paste0("[.] ?(?:", reg, ")");
	npos = regexpr(reg, x$Abstract);
	isSubS = npos > 1;
	npos = npos[isSubS];
	x$Abstract[isSubS] = substr(x$Abstract[isSubS], 1, npos);
	invisible(x);
}

