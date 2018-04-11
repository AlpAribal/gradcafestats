require(data.table)
require(stringr)

test <- T
start <- Sys.time()
submissions <- fread(input = '..\\data\\submissions.csv',
                  sep = '�',
                  header = T,
                  select = c('submissionId', 'institution'),
                  quote = '')

matches <- fread(input = '..\\data\\matching.csv',
                 sep = '�',
                 header = T,
                 quote = '')

submissions[, institution := str_to_lower(institution)]

insts_all <- fread(input = '..\\data\\institutions.csv',
                   header = T,
                   quote = '"')
if(test){
  insts_all <- insts_all[(nrow(insts_all)-2):nrow(insts_all)]
  #insts_all <- insts_all[81]
}

res <- as.data.table(matrix(data = 0, nrow = nrow(submissions), ncol = nrow(insts_all)+2))
res[, 1] <- submissions[, submissionId]
res[, 2] <- NA
colnames(res) <- c('submissionId', 'instId', insts_all[, instId])

i <- 1
repeat{
  print(paste0('Inst #', i))
  # Institution name should match rgx_match
  rgx <- insts_all[i , rgx_match]
  res_vec <- str_detect(string = submissions[, institution], pattern = rgx)
  
  # But it shouldn't match rgx_no_match
  rgx <- insts_all[i, rgx_no_match]
  if(!is.na(rgx) && rgx != ''){
    res_vec <- res_vec & !str_detect(string = submissions[, institution], pattern = rgx)
  }
  
  # Save the match result
  res[,i+2] <- res_vec
  
  # If match, write Id
  res[res_vec == T, instId := insts_all[i, instId]]
  i <- i + 1
  if(i > nrow(insts_all)){
    break;
  }
}

res <- res[, rSum := rowSums(res[,-(1:2)])]

if(test){
  res <- res[rSum > 0]
  res <- merge(merge(submissions, res, by = 'submissionId'), insts_all[, .(instId, inst_name)], by = 'instId')
} else{
  res <- res[rSum==1, 1:2]
  write.table(x = res,
              file = '..\\data\\matching.csv',
              append = T,
              sep = '�',
              row.names = F,
              col.names = F,
              quote = F)
}
print(paste0('Finished @', Sys.time(), '. Took: ', Sys.time()-start))