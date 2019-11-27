EXEC sp_rename 'gprice.Demographics.tri_age','age','COLUMN'
EXEC sp_rename 'gprice.Demographics.gendercode','gender','COLUMN'
EXEC sp_rename 'gprice.Demographics.contactID','ID','COLUMN'
EXEC sp_rename 'gprice.Demographics.address1_stateorprovince','State','COLUMN'
EXEC sp_rename 'gprice.Demographics.tri_imaginecareenrollmentemailsentdate','Emai
lSentDate','COLUMN'
EXEC sp_rename 'gprice.Demographics.Tri_enrollmentcompletedate','Completedate','C
OLUMN'

 ADD TimeCompleteEnrollment as datediff(dd, try_convert(date,Emailsentdate),try_co
 nvert(date,Completedate))
 select top 10 * from gprice.Demographics
 ORDER BY NEWID()
 
 ALTER TABLE gprice.Demographics ADD EnrollmentStatus varchar(255);
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Complete'
WHERE tri_imaginecareenrollmentstatus = '167410011';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Email sent'
WHERE tri_imaginecareenrollmentstatus = '167410001';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Non responder'
WHERE tri_imaginecareenrollmentstatus = '167410004';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Facilitated Enrollment'
WHERE tri_imaginecareenrollmentstatus = '167410005';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Incomplete Enrollments'
WHERE tri_imaginecareenrollmentstatus = '167410002';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Opted Out'
WHERE tri_imaginecareenrollmentstatus = '167410003';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Unprocessed'
WHERE tri_imaginecareenrollmentstatus = '167410000';
UPDATE gprice.Demographics
SET EnrollmentStatus = 'Second email sent'
WHERE tri_imaginecareenrollmentstatus = '167410006';
select top 10 * from gprice.Demographics
 ORDER BY NEWID()
 
 ALTER TABLE gprice.Demographics ADD Sex varchar(255);
UPDATE gprice.Demographics
SET Sex = 'female'
WHERE gender = '2';
UPDATE gprice.Demographics
SET Sex = 'male'
WHERE gender = '1';
UPDATE gprice.Demographics
SET Sex = 'other'
WHERE gender = '167410000';
UPDATE gprice.Demographics
SET Sex = 'Unknown'
WHERE gender = 'NULL';
select top 10 * from gprice.Demographics
 ORDER BY NEWID()
 
