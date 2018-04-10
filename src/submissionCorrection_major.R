setwd("C:\\Users\\User.DESKTOP-KRQQ533\\Desktop\\GradCafe\\submissionTracker\\")
require(data.table)
require(RMySQL)
require(stringr)
con <- dbConnect(RMySQL::MySQL(), dbname = "mydb", user="rUpdater", password="rUpdate")

test <- T
start <- Sys.time()
query <- 'SELECT submissionId, major FROM mydb.submissions WHERE submissionId NOT IN (SELECT submissionId FROM submission_match WHERE NOT isnull(majorId)) ORDER BY submissionId DESC;'
subms_all <- as.data.table(dbGetQuery(conn=con, statement=query))
subms_major <- str_to_lower(subms_all[,major])

query <- 'SELECT * FROM majors;'
majors_all <- as.data.table(dbGetQuery(conn=con, statement=query))
#majors_all <- majors_all[(nrow(majors_all)-1):nrow(majors_all)]
#majors_all <- majors_all[69]

majors_match <- majors_all[, rgx_match]
majors_no_match <- majors_all[, rgx_no_match]

numMajors <- length(majors_match)
numSubms <- length(subms_major)

res <- as.data.table(matrix(nrow = numSubms, ncol = numMajors+1))
res[, 1] <- subms_all[, submissionId]
colnames(res) <- c('submissionId', majors_all[, majorId])

i <- 1
repeat{
  print(paste0('Major #', i, '/', numMajors))
  res_vec <- str_detect(string = subms_major, pattern = majors_match[i])
  if(!is.na(majors_no_match[i]) && majors_no_match[i] != ''){
    res_vec <- res_vec & !str_detect(string = subms_major, pattern = majors_no_match[i])
  }
  res[,i+1] <- res_vec
  i <- i + 1
  if(i > numMajors){
    break;
  }
}

res <- res[, rSum := rowSums(res[,-1])]
f <- function(lst){
  which(unlist(lst) == TRUE)
}
res <- res[, ID := colnames(res)[f(.SD)], by=seq_len(nrow(res))]

lok <- subms_all[res[rSum==1, .SD, .SDcols=c('submissionId', 'ID')], on='submissionId']
colnames(lok) <- c('submissionId', 'major', 'majorId')
if(!test && nrow(lok) > 0){
  updater <- function(x){
    query <- paste0('INSERT INTO submission_match (submissionId, majorId) VALUES (', x['submissionId'], ',', x['majorId'], ')
                      ON DUPLICATE KEY UPDATE majorId = ', x['majorId'], ';')
    dbClearResult(dbSendStatement(conn=con, statement=query))
  }
  gg <- apply(lok, 1, updater)
}
print(paste0('Finished @', Sys.time(), '. Took: ', Sys.time()-start))
lapply(dbListConnections(dbDriver(drv="MySQL")), dbDisconnect)

if(test){
  unique(lok[,major])
}
#write.csv(unique(lok[,major]), 'C:\\Users\\User.DESKTOP-KRQQ533\\Desktop\\lok2.csv', row.names = FALSE)
