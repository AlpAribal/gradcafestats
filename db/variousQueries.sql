/* ARAMA */
SELECT submissions.* from submissions left outer join inst_correction ON institution = inst_old
WHERE (institution like '%toronto%' OR inst_new like '%toronto%')
	AND major like '%computer%'
	AND degree like '%masters'
    AND notif_date > '2015-01-01'
    AND notif_result IN ('Accepted', 'Rejected')
    AND sem like 'F%';

/* SUBMISSIONS */
/* kaç submission var */
SELECT COUNT(DISTINCT submissionId) FROM mydb.submissions;
SELECT * FROM mydb.submissions ORDER BY submissionId DESC;

/* INST CORRECTION */
/* ismi düzeltilmiş olan inst'leri göster */
SELECT * FROM submissions INNER JOIN inst_correction ON strcmp(institution, inst_old) = 0;
/* ismi düzeltilmemiş olan inst'leri göster */
SELECT institution, COUNT(institution) FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction) GROUP BY institution HAVING COUNT(institution) >= 100 ORDER BY COUNT(institution) DESC;
SELECT COUNT(institution) FROM submissions WHERE institution NOT IN (SELECT inst_old FROM inst_correction);

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
SELECT COUNT(major) FROM submissions WHERE major NOT IN (SELECT major_old FROM major_correction);
/* doğru ismi inst_new'da olup inst_old'da olmayanları inst_correction'a ekle*/
INSERT INTO major_correction
	SELECT DISTINCT MC.major_new as major_old, MC.major_new as major_new 
    FROM major_correction MC 
    WHERE MC.major_new NOT IN (SELECT major_old FROM major_correction MC2);
    
/* DEGREE CORRECTION */
SELECT * FROM submissions WHERE major like '%master%' AND degree = '';
SELECT * FROM submissions WHERE major like '%master%' AND degree <> 'Masters';
