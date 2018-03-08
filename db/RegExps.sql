/* Carnegie Mellon University (CMU) */

SELECT institution, COUNT(submissionId) as CNT, length(institution) as LEN FROM submissions
WHERE institution REGEXP '(carnegie|mellon|melon|carnagie|cmu)'
GROUP BY institution;