/* ARAMA */
SELECT submissions.* from submissions left outer join inst_correction ON institution = inst_old
WHERE (institution like '%austin%' OR inst_new like '%austin%')
	AND major like '%computer%'
	AND degree like '%masters'
    AND notif_date > '2018-01-01'
    AND notif_result IN ('Accepted', 'Rejected')
    AND sem like 'F18';
    
SELECT * FROM mydb.submissions WHERE institution like '%Eastern Virginia Medical Insurance%';

SELECT DISTINCT inst_old FROM inst_correction WHERE inst_old not in (SELECT DISTINCT institution FROM submissions);

/* SUBMISSIONS */
/* kaç submission var */
SELECT COUNT(DISTINCT submissionId) FROM mydb.submissions;
SELECT * FROM mydb.submissions ORDER BY submissionId DESC;

/* INST CORRECTION */
/* ismi düzeltilmiş olan inst'leri göster */
SELECT * FROM submissions INNER JOIN inst_correction ON strcmp(institution, inst_old) = 0;
/* ismi düzeltilmemiş olan inst'leri göster */
SELECT institution, COUNT(institution), max(submissionDate) FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction) GROUP BY institution ORDER BY COUNT(institution) DESC;
SELECT institution FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction) GROUP BY institution ORDER BY COUNT(institution) DESC;

SELECT COUNT(DISTINCT institution) FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction);
SELECT COUNT(DISTINCT institution) FROM submissions;
SELECT COUNT(submissionId) FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction);

INSERT INTO inst_correction
	SELECT DISTINCT institution as inst_old, 'loyolaloyola' as inst_new FROM submissions
    WHERE institution NOT IN (SELECT DISTINCT inst_old FROM inst_correction)
		AND institution like '%loyola%';
        
        
/* doğru ismi inst_old'da olmayanları göster */
SELECT * FROM inst_correction IC WHERE IC.inst_new NOT IN (SELECT inst_old FROM inst_correction IC2);

/* doğru ismi inst_new'da olup inst_old'da olmayanları inst_correction'a ekle*/
INSERT INTO inst_correction 
	SELECT DISTINCT IC.inst_new as inst_old, IC.inst_new as inst_new 
    FROM inst_correction IC 
    WHERE IC.inst_new NOT IN (SELECT inst_old FROM inst_correction IC2);
/* İsmindeki boşluk çıkarsa inst_correction'da bulunan isimleri de inst_correction'a ekle */
INSERT INTO inst_correction
	SELECT DISTINCT institution AS inst_old, 
					(SELECT DISTINCT inst_new FROM inst_correction IC WHERE STRCMP(REPLACE(S.institution, '  ', ' '), IC.inst_new) = 0) AS inst_new
	FROM submissions S
    WHERE REPLACE(institution, '  ', ' ') IN (SELECT inst_new FROM inst_correction)
			AND institution NOT IN (SELECT inst_old FROM inst_correction);

/* submissions'da inst_new -()- hariç olanları da al*/

/* İlk kelime + University inst_correction'da olanları göster */

/* MAJOR CORRECTION */
SELECT major, COUNT(major) FROM submissions WHERE major NOT IN (SELECT major_old FROM major_correction) GROUP BY major HAVING COUNT(major) >= 100 ORDER BY COUNT(major) DESC;
SELECT COUNT(DISTINCT major) FROM submissions WHERE major NOT IN (SELECT major_old FROM major_correction);

SELECT * FROM submissions WHERE major like '%master%' AND degree = '';
SELECT * FROM submissions WHERE major like '%master%' AND degree <> 'Masters';

/* doğru ismi inst_new'da olup inst_old'da olmayanları inst_correction'a ekle*/
INSERT INTO major_correction
	SELECT DISTINCT MC.major_new as major_old, MC.major_new as major_new 
    FROM major_correction MC 
    WHERE MC.major_new NOT IN (SELECT major_old FROM major_correction MC2);





/* DEGREE CORRECTION */
/* !!! DONE !!! */
SELECT DISTINCT degree FROM submissions;
SELECT degree, COUNT(degree) FROM submissions WHERE degree NOT IN (SELECT degree_old FROM degree_correction) GROUP BY degree ORDER BY COUNT(degree) DESC;
SELECT COUNT(submissionId) FROM submissions WHERE degree NOT IN (SELECT degree_old FROM degree_correction);

/* DB SIZE */
SELECT max(length(notes)) FROM submissions;
SELECT max(length(institution)) FROM submissions;
SELECT max(length(major)) FROM submissions;
SELECT max(length(degree)) FROM submissions;
SELECT max(length(studentType)) FROM submissions;
SELECT max(length(gre_subject)) FROM submissions;


/* RESULTS */
SELECT S.submissionId, S.institution, inst_name, instId, S.major, R.major, R.majorId
FROM (SELECT CI.submissionId, CI.inst_name, CI.instId, M.major, M.majorId 
		FROM (SELECT SM.submissionId, I.inst_name, I.instId, SM.majorId
				FROM submission_match SM INNER JOIN institutions I ON SM.instId = I.instId) CI 
			 INNER JOIN majors M ON CI.majorId = M.majorId) R
	 INNER JOIN submissions S ON S.submissionId = R.submissionId;