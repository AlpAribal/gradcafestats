require(data.table)
require(stringr)

submissions <- fread(input = "../data/submissions.csv", sep = ";", header = T, select = c("submissionId", 
    "institution", "major", "degree", "notes"), quote = "")

submissions[str_detect(string = degree, pattern = regex("m", ignore_case = T)), `:=`(degree, 
    "Master's")]
submissions[str_detect(string = degree, pattern = regex("d", ignore_case = T)), `:=`(degree, 
    "PhD")]
submissions[degree == "Other", `:=`(degree, NA)]

rgxMasters <- regex("\\bmaster|\\b(?:m[bfph\\.]?a|m\\.?p\\.?p|m\\.?s[cw]?|mcs|m[\\. ]*eng|mmath|m[\\. ]*(?:phil|arch[I1S]*)|MPS|MAUD|M[AS]?PH|M\\.?Div|M[S\\.]Ed|M\\.?St|MEM|MES|MALD|MSFS|MHSc?|MTS|MDesS?|MIA|MLA(?:UD)?|LL\\.?M|MCLS|MFE|MIB|MLS|MRes|MSIM|MSPPM|Th\\.?M)\\b", 
    ignore_case = T)
rgxPhD <- regex("\\bdoctor|\\b(?:ph[\\. ]*d|d[\\. ]*phil|A[\\. ]*u[\\. ]*D|Psy[\\. ]*D|DDes|Dr?PH|dr|Th\\.?D|DBA|DMA|DNP|DPT|ScD|DHSc?|DLA|DMP|EdD)\\b", 
    ignore_case = T)
submissions[str_detect(string = major, pattern = rgxMasters), `:=`(degree, "Master's")]
submissions[str_detect(string = major, pattern = rgxPhD), `:=`(degree, "PhD")]

rgxMasters <- regex('\\bmaster|\\b(?:m[bfph\\.]?a|m\\.?p\\.?p|m\\.?s[cw]?|mcs|m[\\. ]*eng|mmath|m[\\. ]*(?:phil|arch[I1S]*)|MPS|MAUD|M[AS]?PH|M\\.?Div|M[S\\.]Ed|M\\.?St|MEM|MES|MALD|MSFS|MHSc?|MTS|MDesS?|MIA|MLA(?:UD)?|LL\\.?M|MCLS|MFE|MIB|MLS|MRes|MSIM|MSPPM|Th\\.?M)[\\. ]*\\b',
    ignore_case = T)
submissions[is.na(degree) & !str_detect(string = notes, pattern = regex("march", 
    ignore_case = T)) & str_detect(string = notes, pattern = rgxMasters), `:=`(degree, 
    "Master's")]
submissions[is.na(degree) & str_detect(string = notes, pattern = rgxPhD), `:=`(degree, 
    "PhD")]

write_res <- submissions[, .(submissionId, degree)]
write.table(x = write_res, file = "../data/matchDegrees.csv", append = F, sep = ";", 
    row.names = F, col.names = T, quote = F)
