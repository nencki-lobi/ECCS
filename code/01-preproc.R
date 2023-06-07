# Load data

items = read.table("./data/rtask-items.tsv", header = F, sep = "\t", quote = "", encoding = "UTF-8")
colnames(items) = c("PL", "EN", "NO", "code")

subjects = read.table("./data/rtask-subject.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(subjects) = c("sid", "code", "stid")

external.pl = read.table("./data/rtask-subject-external-pl.csv", header = F, sep = "", strip.white = T, encoding = "UTF-8")
colnames(external.pl) = c("sid")

external.no = read.table("./data/rtask-subject-external-no.csv", header = F, sep = "", strip.white = T, encoding = "UTF-8")
colnames(external.no) = c("sid")

ranking = read.table("./data/rtask-subject-ranking.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ranking) = c("sid", "code", "stid", "rank")

demo = read.table("./data/rtask-demo.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(demo) = c("sid", "code", "stid", "name", "ord", "val")

ratings = read.table("./data/rtask-ratings.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ratings) = c("sid", "code", "stid", "name", "ord", "part", "opt")

times = read.table("./data/rtask-time.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(times) = c("sid", "code", "stid", "name", "ord", "pres_time", "eval_time")

# Studies to be included

studies = eval(parse(text = params$studies))

subjects = filter(subjects, stid %in% studies)
ranking = filter(ranking, stid %in% studies)
demo = filter(demo, stid %in% studies)
ratings = filter(ratings, stid %in% studies)
times = filter(times, stid %in% studies)

# Subjects to keep

## Initial list of subjects

subjects %>% group_by(stid) %>% summarise(n=n())

## For participants recruited by the company, keep those who passed quality check

external = bind_rows(list("15" = external.pl, "17" = external.no), .id = 'stid')

subjects = subjects %>%
  filter(stid == 13 | 
         stid == 14 |
         stid == 15 & sid %in% external$sid |
         stid == 17 & sid %in% external$sid)

## Find participants with required number of stories rated

required = params$required
list1 = ranking %>% 
  filter(rank >= required) %>%
  select(stid, sid)

## Find adult participants

list2 = demo %>% 
  filter((stid == 13 | stid == 14 | stid == 15) & ord == 2 & as.numeric(val) < 2005 |
          stid == 17 & ord == 2 & as.numeric(val) < 2006) %>%
  select(stid, sid)

## Final list of subjects

subjects = subjects %>% filter(sid %in% intersect(list1$sid, list2$sid))
# subjects %>% group_by(stid) %>% summarise(n=n())

# Clean data

demo = filter(demo, sid %in% subjects$sid)
ratings = filter(ratings, sid %in% subjects$sid)
times = filter(times, sid %in% subjects$sid)

ranking = filter(ranking, sid %in% subjects$sid)

# Rename questionnaires for easier processing of data from both countries

demo = demo %>% mutate(name = recode(name, "demo-1-pl" = "demo", "demo-1-no" = "demo"))
ratings = ratings %>% mutate(name = recode(name, "rateme-pl" = "rateme", "rateme-no" = "rateme"))
times = times %>% mutate(name = recode(name, "rateme-pl" = "rateme", "rateme-no" = "rateme"))
