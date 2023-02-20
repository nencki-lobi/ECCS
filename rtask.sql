\f '|'
\pset footer

-- Subjects

\o ./data/rtask-subject.csv

SELECT sid, code, stid
FROM subject 
WHERE stid IN (13,14,15) 
ORDER BY stid;

-- Subjects ranked by number of items rated

\o ./data/rtask-subject-ranking.csv

SELECT s.sid, s.code, s.stid, count(*) / 7 as nitems
FROM subject s
JOIN qcopy q ON q.rid = s.sid 
JOIN bchoice b ON b.qid = q.qid
WHERE s.stid IN (13, 14, 15)
 AND q.name = 'rateme-pl'
GROUP BY s.sid, q.qid
ORDER BY count(*) DESC;

-- Demographic data

\o ./data/rtask-demo.csv

SELECT 
s.sid, s.code, s.stid,
q.name, a.ord, a.val
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN answer a ON a.qid = q.qid
WHERE s.stid IN (13, 14, 15)
 AND q.name = 'demo-1-pl' AND q.is_complete
ORDER BY s.sid, a.ord;

-- Ratings data

\o ./data/rtask-ratings.csv

SELECT 
s.sid, s.code, s.stid,
q.name, b.ord, b.part, b.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN bchoice b ON b.qid = q.qid
WHERE s.stid IN (13, 14, 15)
 AND q.name = 'rateme-pl'
ORDER BY s.sid, b.ord, b.part;

-- Time data

\o ./data/rtask-time.csv

SELECT 
s.sid, s.code, s.stid,
q.name, r.ord, r.presentation_time, r.evaluation_time
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN rtitem r ON r.qid = q.qid
WHERE s.stid IN (13, 14, 15)
 AND q.name = 'rateme-pl'
ORDER BY s.sid;
