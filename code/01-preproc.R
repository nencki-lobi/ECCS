# Load data

items = read.table("./data/rtask-items.tsv", header = F, sep = "\t", quote = "", encoding = "UTF-8")
colnames(items) = c("PL", "EN", "NO", "code")

subjects = read.table("./data/rtask-subject.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(subjects) = c("sid", "code", "stid")

external = read.table("./data/rtask-subject-external.csv", header = F, sep = "", strip.white = T, encoding = "UTF-8")
colnames(external) = c("sid")

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

## For participants recruited by the company, keep those who passed quality check

if (15 %in% studies) {
  subjects = subjects %>% 
    filter((stid == 13) | 
           (stid == 14) | 
           (stid == 15 & sid %in% external$sid))
}

## Find participants with required number of stories rated

required = params$required
list1 = filter(ranking, rank >= required)$sid

## Find adult participants

list2 = filter(demo, ord == 2 & as.numeric(val) < 2005)$sid

## Final list of subjects

subjects = subjects %>% filter(sid %in% intersect(list1, list2))
# subjects %>% group_by(stid) %>% summarise(n=n())

# Clean data

demo = filter(demo, sid %in% subjects$sid)
ratings = filter(ratings, sid %in% subjects$sid)
times = filter(times, sid %in% subjects$sid)
