require(data.table)
require(stringr)

TEST <- T
FULL_DB_MATCH <- T
start <- Sys.time()
submissions <- fread(input = "../data/submissions.csv", sep = ";", header = T, select = c("submissionId", 
    "institution"), quote = "")
submissions[, `:=`(institution, str_to_lower(institution))]
uniqueInsts <- unique(submissions$institution)

if (!FULL_DB_MATCH) {
    matches <- fread(input = "../data/matchInstitutions.csv", sep = ";", header = T, 
        quote = "")
    
    uniqueInsts <- unique(setdiff(uniqueInsts, matches$institution))
}

insts_all <- fread(input = "../data/institutions.csv", header = T, quote = "\"")
if (TEST) {
    # insts_all <- insts_all[(nrow(insts_all)-2):nrow(insts_all)] insts_all <-
    # insts_all[68:96]
}

res <- data.table(institution = uniqueInsts, matched_instId = NA_integer_, rSum = 0)

i <- 1
repeat {
    print(paste0("Inst #", i, "/", nrow(insts_all)))
    # Institution name should match rgx_match
    rgx <- insts_all[i, rgx_match]
    res_vec <- str_detect(string = uniqueInsts, pattern = rgx)
    
    # But it shouldn't match rgx_no_match
    rgx <- insts_all[i, rgx_no_match]
    if (!is.na(rgx) && rgx != "") {
        res_vec <- res_vec & !str_detect(string = uniqueInsts, pattern = rgx)
    }
    
    # If match, write Id
    res[res_vec == T, `:=`(matched_instId, insts_all[i, instId])]
    res[res_vec == T, `:=`(rSum, rSum + 1)]
    
    i <- i + 1
    if (i > nrow(insts_all)) {
        break
    }
}

if (TEST) {
    test_res <- res[rSum > 0 & rSum < 3]
    test_res <- merge(insts_all[, .(instId, inst_name)], test_res, by.x = "instId", 
        by.y = "matched_instId")
    test_res <- test_res[instId > 0]
    unmatched <- merge(res[rSum == 0, .(institution)], submissions[, .(institution)], 
        by = "institution", all = F)
    unmatched <- unmatched[, .N, by = list(institution)]
    multimatched <- merge(res[rSum > 1, .(institution)], submissions[, .(institution)], 
        by = "institution", all = F)
    multimatched <- multimatched[, .N, by = list(institution)]
    setorder(unmatched, -N)
    setorder(multimatched, -N)
    if (!FULL_DB_MATCH) {
        coverage <- nrow(submissions[institution %in% matches$institution])/nrow(submissions)
    }
} else {
    write_res <- res[rSum == 1 & matched_instId > 0, 1:2]
    write.table(x = write_res, file = "../data/matchInstitutions.csv", append = !FULL_DB_MATCH, 
        sep = ";", row.names = F, col.names = FULL_DB_MATCH, quote = F)
}
print(paste0("Finished @", Sys.time(), ". Took: ", Sys.time() - start))
