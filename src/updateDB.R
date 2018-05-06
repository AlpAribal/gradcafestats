require(data.table)
require(XML)
require(stringr)
require(RCurl)
Sys.setlocale("LC_TIME", "C") # This is needed for proper date handling

# Load largest submissionId which is saved in a seperate file
maxDB <- as.numeric(fread(file = '..\\data\\maxDB.txt', colClasses = 'numeric'))
if(!is.numeric(maxDB)){
  stop('maxDB is not a number')
}
print(paste0('Largest submissionId is: ', maxDB))
newMaxDB <- maxDB

numPages <- 'unknown'
pageNo <- 1
repeat {
  # Construct page URL
  pageURL <- paste0('http://thegradcafe.com/survey/index.php?q=%28a*%7Cb*%7Cc*%7Cd*%7Ce*%7Cf*%7Cg*%7Ch*%7Ci*%7Cj*%7Ck*%7Cl*%7Cm*%7Cn*%7Co*%7Cp*%7Cq*%7Cr*%7Cs*%7Ct*%7Cu*%7Cv*%7Cw*%7Cx*%7Cy*%7Cz*%7C1*%7C2*%7C3*%7C4*%7C5*%7C6*%7C7*%7C8*%7C9*%7C0*%7C%28*%7C%29*%7C_*%29&t=a&o=&pp=250&p=',pageNo)

  # Download the page
  timer <- Sys.time()
  curlHandle <- getCurlHandle()
  curlSetOpt(useragent = 'Chrome 39.0.2171.71 (64-bit)', curl = curlHandle)
  sourceCode <- getURL(pageURL, .encoding="UTF8", curl = curlHandle, maxredirs = as.integer(20), followlocation = TRUE)
  print(paste0('----- Download of page #', pageNo, '(out of ', numPages,') is complete', ' (took ', Sys.time()-timer, ' seconds'))
  
  # Fetch total number of result pages
  pattern <- 'Showing <strong>\\d* results<\\/strong> over (\\d*) pages'
  numPages <- as.numeric(str_match(sourceCode, pattern)[,-1])
  
  # If total number of result pages is not found, stop execution
  if(is.na(numPages) || !is.numeric(numPages)){
    stop('numPages NA')
    break
  }
  
  # Extract submissions from source code
  timer <- Sys.time()
  submissions <- readHTMLTable(sourceCode, colClasses = 'character', stringsAsFactors = FALSE)[[1]][-1,]
  
  # Modify 'Date Added' field
  submissions[,"Date Added"] <- as.character(as.Date(submissions[,"Date Added"], format='%d %b %Y'))
  
  # Seperate major, degree, and semester
  regExp <- '^\\s*(.*)\\s*,\\s*(.*)\\s*\\((.\\d\\d|\\?)\\)$'
  prog <- str_match_all(submissions[, "Program (Season)"], regExp)
  prog <- matrix(unlist(prog), ncol = 4, byrow = TRUE)[,-1]
  colnames(prog) <- c("major", "degree", "sem")
  prog[prog[, "sem"] == '?', "sem"] <- NA
  
  # Seperate notification info
  regExp <- '^(Accepted|Rejected|Wait listed|Interview|Other) via (Postal Service|Phone|E-mail|Website|Other) on (\\d\\d? .{3} \\d{4})(?: Undergrad GPA: (.*)GRE General \\(V\\/Q\\/W\\): (.*)\\/(.*)\\/(.*)GRE Subject: (n\\/a|\\d*).*)?$'
  notif <- str_match_all(submissions[, "Decision & Date"], regExp)
  notif <- t(sapply(notif, function(x) {length(x) <- 9
                                    as.character(x)
                                    }))[,-1]
  colnames(notif) <- c("notif_result", "notif_method", "notif_date", "gpa", "gre_v", "gre_q", "gre_w", "gre_subject")
  notif[,"notif_date"] <- as.character(as.Date(notif[,"notif_date"], format='%d %b %Y'))
  notif[notif[, "gpa"] == 'n/a', "gpa"] <- NA
  notif[notif[, "gre_v"] == 'n/a', "gre_v"] <- NA
  notif[notif[, "gre_q"] == 'n/a', "gre_q"] <- NA
  notif[notif[, "gre_w"] == 'n/a', "gre_w"] <- NA
  notif[notif[, "gre_subject"] == 'n/a', "gre_subject"] <- NA
  
  # Get submissionIds
  regExp <- 'hideControlsBox\\(this\\);" on(?:m|M)ouse(?:o|O)ver="showControlsBox\\(this,(\\d+)\\);'
  subId <- str_match_all(sourceCode, regExp)
  subId <- matrix(subId[[1]][,-1])
  colnames(subId) <- c('submissionId')
  
  # Merge results
  results <- cbind(subId, submissions[, 'Date Added'], submissions[, 'Institution'], prog, notif[, 1:3], submissions[, 'St1'], notif[, 4:8], submissions[, 'Notes'])
  results <- apply(results, 2, function(x) {gsub('^\\s+', '',x)})
  results <- apply(results, 2, function(x) {gsub('\\s+$', '',x)})
  colnames(results) <- c('submissionId', 'submissionDate', 'institution', 'major', 'degree', 'sem', 'notif_result', 'notif_method', 'notif_date', 'studentType', 'gpa', 'gre_v', 'gre_q', 'gre_w', 'gre_subject', 'notes')
  
  numResults <- nrow(results)
  print(paste0('Parse complete (took ', Sys.time()-timer, ' seconds: ', numResults, ' results'))
  
  # Filter results based on the largest submissionId in the database
  results <- results[as.numeric(results[,'submissionId']) > maxDB, ]
  
  # If there are no new submissions, stop
  if(!is.matrix(results) && !(is.vector(results) && length(results) > 0)) {
    print('There are not any new submissions!')
    break
  }
  print(paste0('There are ', nrow(results), ' new submissions in page ', pageNo))
  
  # Correct empty fields
  results[results == ''] <- NA
  
  # Delimiter for .csv files is 'é', remove all 'é' from data
  results <- str_replace_all(results, 'é', '?')
  
  # Delete unnecessary line breaks
  results <- str_replace_all(results, '\r\n', ' ')
  results <- str_replace_all(results, '\n', ' ')
  results <- str_replace_all(results, '\r', ' ')
  
  # Re-create the structure
  dim(results) <- c(length(results)/16, 16)
  
  # Some errors occur because of encoding.
  # !!! Following three rows get rid of encoding errors. I do not know why. !!!
  Encoding(results) <- "unknown"
  results <- enc2utf8(results)
  Encoding(results) <- "unknown"

  # Insert the results into database
  timer <- Sys.time()
  write.table(x = results,
              file = '..\\data\\submissions.csv',
              append = T,
              sep = 'é',
              row.names = F,
              col.names = F,
              quote = F)
  print(paste0('Results were inserted into database (took ', Sys.time()-timer, ' seconds) -----'))
  
  newMaxDB <- max(results[, 1], newMaxDB)
  
  pageNo <- pageNo + 1
  # Stop if you are at the last page OR if not all results fetched from the current page were fresh 
  if(pageNo > numPages || nrow(results) < numResults){
    break;
  }
}

# Save largest submissionId to a seperate file
write.table(file = '..\\data\\maxDB.txt', x = newMaxDB, append = F, row.names = F, col.names = F, quote = F)