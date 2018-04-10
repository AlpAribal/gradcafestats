setwd("C:\\Users\\User.DESKTOP-KRQQ533\\Desktop\\GradCafe\\submissionTracker\\")
require(data.table)
require(RMySQL)
require(stringr)
con <- dbConnect(RMySQL::MySQL(), dbname = "mydb", user="rUpdater", password="rUpdate")

test <- T
start <- Sys.time()
query <- 'SELECT submissionId, institution FROM mydb.submissions WHERE submissionId NOT IN (SELECT submissionId FROM submission_match WHERE NOT ISNULL(instId)) ORDER BY submissionId DESC'
subms_all <- as.data.table(dbGetQuery(conn=con, statement=query))
subms_inst <- str_to_lower(subms_all[,institution])

query <- 'SELECT * FROM institutions;'
insts_all <- as.data.table(dbGetQuery(conn=con, statement=query))
if(test){
  #insts_all <- insts_all[(nrow(insts_all)-2):nrow(insts_all)]
  insts_all <- insts_all[81]
}
insts_match <- insts_all[, rgx_match]
insts_no_match <- insts_all[, rgx_no_match]

numInsts <- length(insts_match)
numSubms <- length(subms_inst)

res <- as.data.table(matrix(nrow = numSubms, ncol = numInsts+1))
res[, 1] <- subms_all[, submissionId]
colnames(res) <- c('submissionId', insts_all[, instId])

i <- 1
repeat{
  print(paste0('Inst #', i, '/', numInsts))
  res_vec <- str_detect(string = subms_inst, pattern = insts_match[i])
  if(!is.na(insts_no_match[i]) && insts_no_match[i] != ''){
    res_vec <- res_vec & !str_detect(string = subms_inst, pattern = insts_no_match[i])
  }
  res[,i+1] <- res_vec
  i <- i + 1
  if(i > numInsts){
    break;
  }
}

res <- res[, rSum := rowSums(res[,-1])]
f <- function(lst){
  which(unlist(lst) == TRUE)
}
res <- res[, ID := colnames(res)[f(.SD)], by=seq_len(nrow(res))]

lok <- subms_all[res[rSum==1, .SD, .SDcols=c('submissionId', 'ID')], on='submissionId']
colnames(lok) <- c('submissionId', 'instName', 'instId')
if(!test){
  #dbWriteTable(conn=con, name="submission_match", value=as.data.frame(lok[, .SD, .SDcols=c(1,3)]), append=TRUE, row.names=FALSE)
  updater <- function(x){
    query <- paste0('INSERT INTO submission_match (submissionId, instId) VALUES (', x['submissionId'], ',', x['instId'], ')
                    ON DUPLICATE KEY UPDATE instId = ', x['instId'], ';')
    dbClearResult(dbSendStatement(conn=con, statement=query))
  }
  gg <- apply(lok, 1, updater)
}
print(paste0('Finished @', Sys.time(), '. Took: ', Sys.time()-start))
lapply(dbListConnections(dbDriver(drv="MySQL")), dbDisconnect)
