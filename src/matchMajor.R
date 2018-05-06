require(data.table)
require(stringr)

TEST <- T
FULL_DB_MATCH <- F
start <- Sys.time()
submissions <- fread(input = '..\\data\\submissions.csv',
                     sep = 'é',
                     header = T,
                     select = c('submissionId', 'major'),
                     quote = '')
submissions[, major := str_to_lower(major)]
uniqueMajors <- unique(submissions$major)

if(!FULL_DB_MATCH){
  matches <- fread(input = '..\\data\\matchMajors.csv',
                   sep = 'é',
                   header = T,
                   quote = '')
  
  uniqueMajors <- unique(setdiff(uniqueMajors, matches$major))
}

majors_all <- fread(input = '..\\data\\majors.csv',
                   header = T,
                   quote = '"')

if(TEST){
  #majors_all <- majors_all[(nrow(majors_all)-2):nrow(majors_all)]
  #majors_all <- majors_all[32]
}

res <- as.data.table(matrix(data = 0, nrow = length(uniqueMajors), ncol = nrow(majors_all)+2))
res[, 1] <- uniqueMajors
res[, 2] <- NA
colnames(res) <- c('major', 'matched_majorId', majors_all[, majorId])

i <- 1
repeat{
  print(paste0('Major #', i, '/', nrow(majors_all)))
  # Major should match rgx_match
  rgx <- majors_all[i , rgx_match]
  res_vec <- str_detect(string = uniqueMajors, pattern = rgx)
  
  # But it shouldn't match rgx_no_match
  rgx <- majors_all[i, rgx_no_match]
  if(!is.na(rgx) && rgx != ''){
    res_vec <- res_vec & !str_detect(string = uniqueMajors, pattern = rgx)
  }
  
  # Save the match result
  res[,i+2] <- res_vec
  
  # If match, write Id
  res[res_vec == T, matched_majorId := majors_all[i, majorId]]
  i <- i + 1
  if(i > nrow(majors_all)){
    break;
  }
}

res <- res[, rSum := rowSums(res[,-(1:2)])]

if(TEST){
  test_res <- res[rSum > 0, .(major, matched_majorId, rSum)]
  test_res <- merge(majors_all[, .(majorId, major)], test_res, by.y = 'matched_majorId', by.x = 'majorId')
  unmatched <- merge(res[rSum == 0, .(major)], submissions[, .(major)], by = 'major', all = F)
  unmatched <- unmatched[, .N, by = list(major)]
  setorder(unmatched, -N)
} else{
  write_res <- res[rSum==1 & matched_majorId > 0, 1:2]
  write.table(x = write_res,
              file = '..\\data\\matchMajors.csv',
              append = !FULL_DB_MATCH,
              sep = 'é',
              row.names = F,
              col.names = FULL_DB_MATCH,
              quote = F)
}
print(paste0('Finished @', Sys.time(), '. Took: ', Sys.time()-start))