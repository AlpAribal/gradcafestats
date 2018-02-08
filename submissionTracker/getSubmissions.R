getSubmissions <- function(sourceCode) {
  require(stringr)
  # download(pageURL, "file.html", mode = "wb")
  # sourceCode <- readLines("file.html")
  
  # pattern <- '<tr class="row\\d+" onmouseout="hideControlsBox\\(this\\);" onmouseover="showControlsBox\\(this,(\\d+)\\);"><td class="instcol">(?:<b.*Institution<\\/a><\\/b>)?([^<]*)<\\/td><td>(?:<b.*Program<\\/a> \\(Season\\)<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Date<\\/a><\\/b>)?(?:<span[^>]*>([^<]*)<\\/span>)?([^<]*)(?:<a[^>]*><span><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><\\/span>(?:&diams;|♦)<\\/a>)?<\\/td><td>(?:<b.*St<sup>1<\\/sup><\\/b>)?([^<]*)<\\/td><td class="datecol">(?:<b.*Date Added<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Notes<\\/b>)?<ul class="control"[^>]*><li class="controlspam"[^>]*>(?:<a[^>]*>report spam<\\/a>)?<\\/li><li>([^<]*)<\\/li><\\/ul><\\/td><\\/tr>'
  pattern <- '<tr class="row\\d+" on(?:m|M)ouse(?:o|O)ut="hideControlsBox\\(this\\);" on(?:m|M)ouse(?:o|O)ver="showControlsBox\\(this,(\\d+)\\);"><td class="instcol">(?:<b.*Institution<\\/a><\\/b>)?([^<]*)<\\/td><td>(?:<b.*Program<\\/a> \\(Season\\)<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Date<\\/a><\\/b>)?(?:<span[^>]*>([^<]*)<\\/span>)?([^<]*)(?:<a[^>]*><span><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><\\/span>(?:&diams;|♦)<\\/a>)?<\\/td><td>(?:<b.*St<sup>1<\\/sup><\\/b>)?([^<]*)<\\/td><td class="datecol">(?:<b.*Date Added<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Notes<\\/b>)?<ul class="control"[^>]*><li class="controlspam"[^>]*>(?:<a[^>]*>report spam<\\/a>)?<\\/li><li>([^<]*)<\\/li><\\/ul><\\/td><\\/tr>'
  
  # a <- grep(pattern, sourceCode)
  # a <- str_locate_all(sourceCode, pattern)
  
  # Get bulk submission data
  a <- str_match_all(sourceCode, pattern)
  a <- a[[1]][,-1]
  colnames(a) <- c("submissionId", "institution", "program", "notif_result", "notif_method_date", 
                   "gpa", "gre_bulk", "gre_subject", "studentType", "submissionDate", "notes")
  
  # Correct submission dates
  a[, "submissionDate"] <- gsub("Jan", "1", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Feb", "2", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Mar", "3", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Apr", "4", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("May", "5", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Jun", "6", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Jul", "7", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Aug", "8", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Sep", "9", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Oct", "10", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Nov", "11", a[, "submissionDate"])
  a[, "submissionDate"] <- gsub("Dec", "12", a[, "submissionDate"])
  a[, "submissionDate"] <- as.character(as.Date(a[, "submissionDate"], format="%d %m %Y"))
  
  # Divide Program data into Major, Degree and Semester
  sem <- str_match_all(a[, "program"], '^(.*),(.*)\\((.\\d\\d|\\?)\\)$')
  sem <- matrix(unlist(sem), ncol = 4, byrow = TRUE)[,-1]
  colnames(sem) <- c("major", "degree", "sem")
  sem[sem[, "sem"] == '?', "sem"] <- NA
  
  # Divide Notification data into Notif Result, Method and Date
  notif <- str_match_all(a[, "notif_method_date"], '^(Wait listed|Other|Interview|) via (Postal Service|Phone|E-mail|Website|Other) on (.*)$')
  notif <- t(sapply(notif, function(x) {length(x) <- 4
                                      as.character(x)
  }))[,-1]
  colnames(notif) <- c("notif_result", "notif_method", "notif_date")
  notif[, "notif_date"] <- gsub("Jan", "1", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Feb", "2", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Mar", "3", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Apr", "4", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("May", "5", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Jun", "6", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Jul", "7", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Aug", "8", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Sep", "9", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Oct", "10", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Nov", "11", notif[, "notif_date"])
  notif[, "notif_date"] <- gsub("Dec", "12", notif[, "notif_date"])
  notif[, "notif_date"] <- as.character(as.Date(notif[, "notif_date"], format="%d %m %Y"))
  
  # Merge Notif data from a and notif
  a[is.na(a[,"notif_result"]), "notif_result"] <- ""
  notif[is.na(notif[,"notif_result"]), "notif_result"] <- ""
  notif[,"notif_result"] <- paste0(a[, "notif_result"], notif[, "notif_result"])
  
  # Divide GRE info
  # !!! WHAT ABOUT GRE SUBJECT TESTS? !!!
  gre <- str_match_all(a[, "gre_bulk"], '(\\d{1,3})\\/(\\d{1,3})\\/(.*)')
  gre <- matrix(unlist(gre), ncol = 4, byrow = TRUE)[,-1]
  colnames(gre) <- c("gre_v", "gre_q", "gre_w")

  
  # Merge data
  a <- subset(a, select = -c(program, notif_method_date,notif_result,gre_bulk))
  a <- cbind(a, sem, notif, gre)
  a <- apply(a,2,function(x)gsub('^\\s+', '',x))
  a <- apply(a,2,function(x)gsub('\\s+$', '',x))
}