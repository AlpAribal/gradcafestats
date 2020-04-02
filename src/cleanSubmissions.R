require(data.table)
require(stringr)

submissions <- fread(input = "../data/submissions.csv", sep = ";", header = T, quote = "", 
    drop = "notes")

# Fill in empty sem values #### 
# If submission month >= September, then sem = F[next_year]
# If submission month < September, then sem = F[same_year]
submissions[, `:=`(submissionDate, as.Date(submissionDate))]
submissions[is.na(sem), `:=`(sem, ifelse(month(submissionDate) < 9, paste0("F", str_sub(year(submissionDate), 
    start = 3)), paste0("F", str_sub(year(submissionDate) + 1, start = 3))))]

# Cleanse notif_result ####
submissions <- submissions[!is.na(notif_result) & notif_result != "Other"]

# Cleanse notif_date ####
submissions[, `:=`(notif_date, as.Date(notif_date))]
submissions[is.na(notif_date), `:=`(notif_date, submissionDate)]

# Cleanse studentType ####
submissions[is.na(studentType) | studentType == "?", `:=`(studentType, "O")]

# Normalize GPA ####
submissions[gpa > 4, `:=`(gpa, gpa/10)]

# Normalize GRE ####
# Verbal
concVerbal <- fread(input = "../data/concordanceVerbal.csv", header = T)
setorder(concVerbal, PriorScale)

submissions[gre_v > 800 | (gre_v > 170 & gre_v < 200) | gre_v < 130, `:=`(gre_v, 
    NA)]
submissions[gre_v >= 200, `:=`(gre_v, concVerbal[findInterval(gre_v, concVerbal$PriorScale), 
    CurrentScale])]

# Quant
concQuant <- fread(input = "../data/concordanceQuant.csv", header = T)
setorder(concQuant, PriorScale)

submissions[gre_q > 800 | (gre_q > 170 & gre_q < 200) | gre_q < 130, `:=`(gre_q, 
    NA)]
submissions[gre_q >= 200, `:=`(gre_q, concQuant[findInterval(gre_q, concQuant$PriorScale), 
    CurrentScale])]

# AW
submissions[gre_w > 6, `:=`(gre_w, NA)]

cleanSubmissions <- submissions[, .(submissionId, submissionDate, institution = str_to_lower(institution), 
    major = str_to_lower(major), sem, notif_result, notif_date, studentType, gpa, 
    gre_v, gre_q, gre_w, notes)]
# Merge institutions ####
matchesInst <- fread(input = "../data/matchInstitutions.csv", sep = ";", header = T, 
    quote = "")
insts_all <- fread(input = "../data/institutions.csv", header = T, select = c("instId", 
    "inst_name"), quote = "\"")
cleanSubmissions <- merge(cleanSubmissions, matchesInst, by = "institution", all = F)
cleanSubmissions <- merge(cleanSubmissions, insts_all[instId > 0], by.x = "matched_instId", 
    by.y = "instId", all = F)
cleanSubmissions[, `:=`(c("institution", "matched_instId"), NULL)]
setnames(cleanSubmissions, "inst_name", "institution")

# Merge majors ####
matchesMajor <- fread(input = "../data/matchMajors.csv", sep = ";", header = T, quote = "")
majors_all <- fread(input = "../data/majors.csv", header = T, select = c("majorId", 
    "major"), quote = "\"")

cleanSubmissions <- merge(cleanSubmissions, matchesMajor, by = "major", all = F)
cleanSubmissions <- merge(cleanSubmissions, majors_all[majorId > 0], by.x = "matched_majorId", 
    by.y = "majorId", all = F)
cleanSubmissions[, `:=`(c("major.x", "matched_majorId"), NULL)]
setnames(cleanSubmissions, "major.y", "major")

# Merge degrees ####
matchesDegree <- fread(input = "../data/matchDegrees.csv", sep = ";", header = T, 
    quote = "")
cleanSubmissions <- merge(cleanSubmissions, matchesDegree, by = "submissionId", all = F)
fwrite(x = cleanSubmissions, file = "../data/cleanSubmissions.csv", append = F, quote = F, 
    sep = ";", na = NA, col.names = T)
save(cleanSubmissions, file = "../data/cleanSubmissions.RData")
