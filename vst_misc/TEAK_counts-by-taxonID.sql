-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

### Summarize counts by taxonID for a given eventID ####

WITH m AS (
  SELECT m1.individualid, m1.taxonid
  FROM vstqaqc.mappingandtagging AS m1
  JOIN (
    SELECT individualid, MAX(date) AS date
    FROM vstqaqc.mappingandtagging
    WHERE siteid='TEAK'
    GROUP BY individualid
) AS m2
ON m1.individualid = m2.individualid AND m1.date = m2.date
)
SELECT ai.siteid, m.taxonid, COUNT(*)
FROM vstqaqc.apparentindividual AS ai
JOIN m
ON ai.individualid = m.individualid
WHERE ai.eventid='vst_TEAK_2021'
GROUP BY ai.siteid, m.taxonid
ORDER BY ai.siteid, m.taxonid;


