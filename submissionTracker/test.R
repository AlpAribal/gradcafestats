require(RCurl)
pageURL <- paste0('http://thegradcafe.com/survey/index.php?q=%28a*%7Cb*%7Cc*%7Cd*%7Ce*%7Cf*%7Cg*%7Ch*%7Ci*%7Cj*%7Ck*%7Cl*%7Cm*%7Cn*%7Co*%7Cp*%7Cq*%7Cr*%7Cs*%7Ct*%7Cu*%7Cv*%7Cw*%7Cx*%7Cy*%7Cz*%7C1*%7C2*%7C3*%7C4*%7C5*%7C6*%7C7*%7C8*%7C9*%7C0*%7C%28*%7C%29*%7C_*%29&t=a&o=&pp=250&p=1')
sourceCode <- getURL(pageURL)
pattern <- '<tr class="row\\d+" on(?:m|M)ouse(?:o|O)ut="hideControlsBox\\(this\\);" on(?:m|M)ouse(?:o|O)ver="showControlsBox\\(this,(\\d+)\\);"><td class="instcol">(?:<b.*Institution<\\/a><\\/b>)?([^<]*)<\\/td><td>(?:<b.*Program<\\/a> \\(Season\\)<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Date<\\/a><\\/b>)?(?:<span[^>]*>([^<]*)<\\/span>)?([^<]*)(?:<a[^>]*><span><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><strong>[^>]*>:([^<]*)<br\\/?><\\/span>(?:&diams;|â™¦)<\\/a>)?<\\/td><td>(?:<b.*St<sup>1<\\/sup><\\/b>)?([^<]*)<\\/td><td class="datecol">(?:<b.*Date Added<\\/b>)?([^<]*)<\\/td><td>(?:<b.*Notes<\\/b>)?<ul class="control"[^>]*><li class="controlspam"[^>]*>(?:<a[^>]*>report spam<\\/a>)?<\\/li><li>([^<]*)<\\/li><\\/ul><\\/td><\\/tr>'

# a <- grep(pattern, sourceCode)
# a <- str_locate_all(sourceCode, pattern)

# Get bulk submission data
a <- str_match_all(sourceCode, pattern)
a <- a[[1]][,-1]
colnames(a) <- c("submissionId", "institution", "program", "notif_result", "notif_method_date", 
                 "gpa", "gre_bulk", "gre_subject", "studentType", "submissionDate", "notes")
