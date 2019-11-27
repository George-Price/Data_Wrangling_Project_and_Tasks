/* Number Two */

/* Merge Tables */
SELECT * into [gprice].[q2]
FROM gprice.Demographics INNER JOIN dbo.Conditions
ON gprice.Demographics.ID = dbo.Conditions.tri_patientid
INNER JOIN dbo.TextMessages
ON gprice.Demographics.ID = dbo.TextMessages.tri_contactId AND gprice.Demographics.ID = dbo.Conditions.tri_patientid

/* Print 10 rows  */
select top 10 * from gprice.q2
ORDER BY NEWID()

/* Change TextSentDate to date format */
alter table gprice.q2
alter column TextSentDate DATE

/* Create a table that contains the max textsentdate from merged table */
SELECT merged.* INTO gprice.q2max 
FROM gprice.q2 merged 
INNER JOIN (
SELECT tri_contactId, MAX(TextSentDate) AS MaxTextSentDate
FROM gprice.joined
GROUP BY tri_contactId) maxgroup
ON merged.tri_contactID = maxgroup.tri_contactId AND merged.TextSentDate = maxgroup.MaxTextSentDate

/* Check count to ensure not all rows are being saved */ 
SELECT COUNT (TextSentDate)
FROM gprice.q2max

/* Print 10 rows  */
select top 10 * from gprice.q2max
ORDER BY NEWID()
