require(data.table)
require(stringr)

TEST <- T
FULL_DB_MATCH <- F
start <- Sys.time()
submissions <- fread(input = '..\\data\\submissions.csv',
                     sep = 'é',
                     header = T,
                     select = c('submissionId', 'institution'),
                     quote = '')
submissions[, institution := str_to_lower(institution)]
uniqueInsts <- unique(submissions$institution)

if(!FULL_DB_MATCH){
  matches <- fread(input = '..\\data\\matchInstitutions.csv',
                   sep = 'é',
                   header = T,
                   quote = '')
  
  uniqueInsts <- unique(setdiff(uniqueInsts, matches$institution))
}

insts_all <- fread(input = '..\\data\\institutions.csv',
                   header = T,
                   quote = '"')
if(TEST){
  #insts_all <- insts_all[(nrow(insts_all)-2):nrow(insts_all)]
  #insts_all <- insts_all[1:50]
}

res <- as.data.table(matrix(data = 0, nrow = length(uniqueInsts), ncol = nrow(insts_all)+2))
res[, 1] <- uniqueInsts
res[, 2] <- NA
colnames(res) <- c('institution', 'matched_instId', insts_all[, instId])

i <- 1
repeat{
  print(paste0('Inst #', i, '/', nrow(insts_all)))
  # Institution name should match rgx_match
  rgx <- insts_all[i , rgx_match]
  res_vec <- str_detect(string = uniqueInsts, pattern = rgx)

  # But it shouldn't match rgx_no_match
  rgx <- insts_all[i, rgx_no_match]
  if(!is.na(rgx) && rgx != ''){
    res_vec <- res_vec & !str_detect(string = uniqueInsts, pattern = rgx)
  }

  # Save the match result
  res[,i+2] <- res_vec

  # If match, write Id
  res[res_vec == T, matched_instId := insts_all[i, instId]]
  i <- i + 1
  if(i > nrow(insts_all)){
    break;
  }
}

res <- res[, rSum := rowSums(res[,-(1:2)])]

if(TEST){
  test_res <- res[rSum > 0 & rSum < 3, .(institution, matched_instId, rSum)]
  test_res <- merge(insts_all[, .(instId, inst_name)], test_res, by.x = 'instId', by.y = 'matched_instId')
  unmatched <- merge(res[rSum == 0, .(institution)], submissions[, .(institution)], by = 'institution', all = F)
  unmatched <- unmatched[, .N, by = list(institution)]
  setorder(unmatched, -N)
} else{
  write_res <- res[rSum==1 & matched_instId > 0, 1:2]
  write.table(x = write_res,
              file = '..\\data\\matchInstitutions.csv',
              append = !FULL_DB_MATCH,
              sep = 'é',
              row.names = F,
              col.names = FULL_DB_MATCH,
              quote = F)
}
print(paste0('Finished @', Sys.time(), '. Took: ', Sys.time()-start))
