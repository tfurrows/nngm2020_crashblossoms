# NaNoGenMo 2020 entry, tfurrows@sdf.org
# code is in the public domain
#
# Build a book with something like:
#    r crashblossoms.r > crashblossoms.txt
#    pandoc -s -o epub/crashblossoms.epub meta/metadata.txt txt/crashblossoms.txt

data <- read.csv('data/headline.csv', strip.white=TRUE)

# give our columns useful names
colnames(data) <- c("id","headline","url","provider","datetime","lm")

# split up the lastmod column into date and time columns
data$lmd = as.character(lapply(strsplit(as.character(data$lm), split=" "), "[", 1))
data$lmt = as.character(lapply(strsplit(as.character(data$lm), split=" "), "[", 2))

# type, dupes, and sort
data$headline <- as.character(data$headline)
data <- data[!duplicated(data[ , c("headline")]),]
data <- data[order(as.Date(data$lmd, format="%Y-%m-%d")),]

nr <- nrow(data)
ph <- 0
pr <- 0
chap <- 1

cat("PREFACE\n\n")

# seq() to create a for loop step
for(i in seq(1,nr,80)) {
	# use this to include/exclude headlines based on keyword
	#if (grepl("trump",data[i,2],ignore.case=TRUE) == FALSE) {
	
		# clean up a few things	
		data[i,2] <- gsub("&nbsp;"," ", data[i,2])
		data[i,2] <- gsub("&rsquo;","'", data[i,2])
		data[i,2] <- gsub("&lsquo;","'", data[i,2])
		data[i,2] <- gsub("&rdquo;","\"", data[i,2])
		data[i,2] <- gsub("&ldquo;","\"", data[i,2])	
		data[i,2] <- gsub("&#8212;","-", data[i,2])
		data[i,2] <- gsub("&ndash;","-", data[i,2])
		data[i,2] <- gsub("&shy;","-", data[i,2])	

		# Chapter size, in paragraphs
		if(pr > 30 + sample.int(75,1)) {
			# Markdown for chapter break H1
			cat("\n\n# CHAPTER",chap,"\n")
			pr <- 0
			chap <- chap + 1
		}

		# Markdown for chapter sub-title H2
		if(pr == 0) cat("## ")

		# Try to get the punctuation somewhat correct
		if(substr(data[i,2], nchar(data[i,2]), nchar(data[i,2])) != "?" && pr !=0) {
			cat(data[i,2],". ",sep="")
		} else {
			cat(data[i,2]," ",sep="")
		}

		# New chapter, the first headline will be the chapter title.
		if(pr == 0) {
			cat("\n\n")
			pr <- 1
		} else {	
			# Paragraph size, in headline count
			ph <- ph + 1
			if(ph > 3 + sample.int(15,1)) {
				cat("\n\n")
				ph <- 0
				pr <- pr + 1	
			}
		}
	#}	
}

