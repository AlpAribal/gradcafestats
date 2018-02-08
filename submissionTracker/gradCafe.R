setwd("C:\\Users\\User.DESKTOP-KRQQ533\\Desktop\\GradCafe\\")
require(RMySQL)
require(data.table)
require(RCurl)
require(stringr)
source("getSubmissions.R")

# Chars is here just for reference. FOrmerly, the code fetched results from ?q=a+, ?q=b+ and so on.
#chars <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
#           'w','x','y','z','1','2','3','4','5','6','7','8','9','0')

# Connect to the database and retrieve the last submission ID
con <- dbConnect(RMySQL::MySQL(), dbname = "mydb", user="rUpdater", password="rUpdate")
query <- paste('SELECT MAX(submissionId) FROM submissions')
maxDB <- as.integer(dbGetQuery(conn=con, statement=query))
cat(paste0('Last submitId is:', maxDB))

numPages = 'unknown'
pageNo = 1
repeat {
  # Construct the new page URL and log it
  # Following line is here just for reference. Formerly, the code fetched results from ?q=a+, ?q=b+ and so on.
  #pageURL <- paste0("http://thegradcafe.com/survey/index.php?q=", oneChar, "*&p=", pageNo, "&pp=250")
  pageURL <- paste0('http://thegradcafe.com/survey/index.php?q=%28a*%7Cb*%7Cc*%7Cd*%7Ce*%7Cf*%7Cg*%7Ch*%7Ci*%7Cj*%7Ck*%7Cl*%7Cm*%7Cn*%7Co*%7Cp*%7Cq*%7Cr*%7Cs*%7Ct*%7Cu*%7Cv*%7Cw*%7Cx*%7Cy*%7Cz*%7C1*%7C2*%7C3*%7C4*%7C5*%7C6*%7C7*%7C8*%7C9*%7C0*%7C%28*%7C%29*%7C_*%29&t=a&o=&pp=250&p=',pageNo)
  print(paste0('Query: ', 'All results starting with an alphanumeric character.',
             ' Page ', pageNo, '/', numPages,' @', Sys.time()))
  
  # Download the page
  sourceCode <- getURL(pageURL)
  print(paste0('Download of page #', pageNo, ' is complete', ' @', Sys.time()))

  # Fetch the total number of result pages
  pattern <- 'Showing <strong>\\d* results<\\/strong> over (\\d*) pages'
  numPages <- as.numeric(str_match(sourceCode, pattern)[,-1])
  
  # If total number of result pages is not found, stop execution
  if(is.na(numPages)){
    print('numPages NA')
    break
  }
  
  # Trim source code
  # !!! Is this really necessary or efficient? !!!
  startStop <- str_locate(sourceCode, c('<tr class','</table>'))
  sourceCode <- str_sub(sourceCode, startStop[1,1], startStop[2,2])
  
  # Extract the submissions from (trimmed) source code
  submissions <- as.matrix(getSubmissions(sourceCode))
  
  # Take a note of the total number of fetched results
  unfilteredNrow <- nrow(submissions)
  
  # Filter results based on the last submission in the database
  submissions <- submissions[as.numeric(submissions[,'submissionId']) > maxDB, ]
  
  # If there are no new submissions, stop
  if(length(submissions) < 1 || nrow(submissions) < 1) {
    print('There are not any new submissions!')
    break
  }
  print(paste0('There are ', nrow(submissions), ' new submissions in page ', pageNo))
  
  pageNo <- pageNo + 1
  
  # Some errors occur because of encoding.
  # !!! Following three rows get rid of encoding errors. I do not know why. !!!
  Encoding(submissions) <- "unknown"
  submissions <- enc2utf8(submissions)
  Encoding(submissions) <- "unknown"
  
  # Insert the results into database
  dbWriteTable(conn=con, name="submissions", value=as.data.frame(submissions), append=TRUE, row.names=FALSE)
  
  # Stop if you are at the last page OR if not all results fetched from the current page were fresh 
  if(pageNo > numPages || nrow(submissions) < unfilteredNrow){
    break;
  }
}
# Disconnect from database
lapply(dbListConnections(dbDriver(drv="MySQL")), dbDisconnect)