


# library(openxlsx2)
library(readODS)

# Read Words from ODS-file
import.words = function(file = "Words.Med.SARS.ods", verbose = TRUE) {
	sheets = c("Nouns", "Drugs", "Chem", "Other", "Abbr", "Names");
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

