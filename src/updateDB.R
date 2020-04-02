require(data.table)
require(XML)
require(stringr)
require(RCurl)
Sys.setlocale("LC_TIME", "C")  # This is needed for proper date handling

# Load largest submissionId which is saved in a seperate file
maxDB <- as.numeric(fread(file = "../data/maxDB.txt", colClasses = "numeric"))
if (!is.numeric(maxDB)) {
    stop("maxDB is not a number")
}
print(paste0("Largest submissionId is: ", maxDB))
newMaxDB <- maxDB

numPages <- "unknown"
pageNo <- 1
repeat {
    # Construct page URL
    pageURL <- paste0("http://thegradcafe.com/survey/index.php?q=%28a*%7Cb*%7Cc*%7Cd*%7Ce*%7Cf*%7Cg*%7Ch*%7Ci*%7Cj*%7Ck*%7Cl*%7Cm*%7Cn*%7Co*%7Cp*%7Cq*%7Cr*%7Cs*%7Ct*%7Cu*%7Cv*%7Cw*%7Cx*%7Cy*%7Cz*%7C1*%7C2*%7C3*%7C4*%7C5*%7C6*%7C7*%7C8*%7C9*%7C0*%7C%28*%7C%29*%7C_*%29&t=a&o=&pp=250&p=", 
        pageNo)
    
    # Download the page
    timer <- Sys.time()
    curlHandle <- getCurlHandle()
    curlSetOpt(useragent = "Chrome 39.0.2171.71 (64-bit)", curl = curlHandle)
    sourceCode <- getURL(pageURL, .encoding = "UTF8", curl = curlHandle, maxredirs = as.integer(20), 
        followlocation = TRUE)
    print(paste0("----- Download of page #", pageNo, "(out of ", numPages, ") is complete", 
        " (took ", Sys.time() - timer, " seconds"))
    
    # Fetch total number of result pages
    pattern <- "Showing <strong>\\d* results<\\/strong> over (\\d*) pages"
    numPages <- as.numeric(str_match(sourceCode, pattern)[, -1])
    
    # If total number of result pages is not found, stop execution
    if (is.na(numPages) || !is.numeric(numPages)) {
        stop("numPages NA")
        break
    }
    
    # Extract submissions from source code
    timer <- Sys.time()
    submissions <- readHTMLTable(sourceCode, colClasses = "character", stringsAsFactors = FALSE)[[1]][-1, 
        ]
    submissions <- as.data.table(submissions)
    colnames(submissions) <- c("Institution", "Program (Season)", "Decision & Date", 
        "St1", "Date Added", "Notes")
    
    # Modify 'Date Added' field
    submissions[, `:=`(`Date Added`, as.character(as.Date(`Date Added`, format = "%d %b %Y")))]
    
    # Seperate major, degree, and semester
    regExp <- "^\\s*(.*?)\\s*,\\s*(.*?)\\s*\\((.*)\\)\\s*$"
    prog <- as.data.table(str_match(submissions$`Program (Season)`, regExp)[, -1])
    colnames(prog) <- c("major", "degree", "sem")
    prog[sem == "?", `:=`(sem, NA)]
    
    # Seperate notification info
    regExp <- "^(Accepted|Rejected|Wait listed|Interview|Other) via (Postal Service|Phone|E-mail|Website|Other) on (\\d\\d? .{3} \\d{4})(?: Undergrad GPA: (.*)GRE General \\(V\\/Q\\/W\\): (.*)\\/(.*)\\/(.*)GRE Subject: (n\\/a|\\d*).*)?$"
    notif <- as.data.table(str_match(submissions$`Decision & Date`, regExp)[, -1])
    colnames(notif) <- c("notif_result", "notif_method", "notif_date", "gpa", "gre_v", 
        "gre_q", "gre_w", "gre_subject")
    notif[, `:=`(notif_date, as.character(as.Date(notif_date, format = "%d %b %Y")))]
    notif[gpa == "n/a", `:=`(gpa, NA)]
    notif[gre_v == "n/a", `:=`(gre_v, NA)]
    notif[gre_q == "n/a", `:=`(gre_q, NA)]
    notif[gre_w == "n/a", `:=`(gre_w, NA)]
    notif[gre_subject == "n/a", `:=`(gre_subject, NA)]
    
    # Get submissionIds
    regExp <- "hideControlsBox\\(this\\);\" on(?:m|M)ouse(?:o|O)ver=\"showControlsBox\\(this,(\\d+)\\);"
    subId <- str_match_all(sourceCode, regExp)
    subId <- matrix(subId[[1]][, -1])
    colnames(subId) <- c("submissionId")
    
    # Merge results
    results <- cbind(subId, submissionDate = submissions$`Date Added`, institution = submissions$Institution, 
        prog, notif[, .(notif_result, notif_method, notif_date)], submissions$St1, 
        notif[, .(gpa, gre_v, gre_q, gre_w, gre_subject)], notes = submissions$Notes)
    
    numResults <- nrow(results)
    print(paste0("Parse complete (took ", Sys.time() - timer, " seconds: ", numResults, 
        " results"))
    
    # Filter results based on the largest submissionId in the database
    results[, `:=`(submissionId, as.numeric(submissionId))]
    results <- results[submissionId > maxDB]
    
    # Additional filters
    results <- results[!str_detect(string = institution, regex("chnlove|sexy|love-sites", 
        ignore_case = T)), ]
    
    # If there are no new submissions, stop
    if (nrow(results) == 0) {
        print("There are not any new submissions!")
        break
    }
    print(paste0("There are ", nrow(results), " new submissions in page ", pageNo))
    
    results <- results[, `:=`((colnames(results)), lapply(.SD, function(x) {
        # Trim fields
        x <- str_trim(x, side = "both")
        
        # Delimiter for .csv files is ';', remove all ';' from data
        x <- str_replace_all(x, ";", ",")
        
        # Delete unnecessary line breaks
        x <- str_replace_all(x, "\r\n", " ")
        x <- str_replace_all(x, "\n", " ")
        x <- str_replace_all(x, "\r", " ")
        
        # Correct empty fields
        x[x == ""] <- NA
        
        # Set encoding
        Encoding(x) <- "UTF-8"
        
        return(x)
    })), .SDcols = colnames(results)]
    
    # Insert the results into database
    timer <- Sys.time()
    write.table(x = results, file = "../data/submissions.csv", append = T, sep = ";", 
        row.names = F, col.names = F, quote = F)
    print(paste0("Results were inserted into database (took ", Sys.time() - timer, 
        " seconds) -----"))
    
    newMaxDB <- max(results[, as.numeric(submissionId)], newMaxDB)
    
    pageNo <- pageNo + 1
    # Stop if you are at the last page OR if not all results fetched from the current
    # page were fresh
    if (pageNo > numPages || nrow(results) < numResults) {
        break
    }
}

# Save largest submissionId to a seperate file
write.table(file = "../data/maxDB.txt", x = newMaxDB, append = F, row.names = F, 
    col.names = F, quote = F)
