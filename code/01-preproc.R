# Load data

ratings = read.table("./data/rtask-ratings.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ratings) = c("sid","code","stid","name","ord", "part", "opt")

demo = read.table("./data/rtask-demo.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(demo) = c("sid","code","stid","name","ord", "val")

items = read.table("./data/rtask-items.tsv", header = F, sep = "\t", encoding = "UTF-8")
colnames(items) = c("PL","EN","NO","code")

ranking = read.table("./data/rtask-subject-ranking.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ranking) = c("sid","code","stid","rank")

# Studies to be included

studies = eval(parse(text = params$studies))

ratings = filter(ratings, stid %in% studies)
demo = filter(demo, stid %in% studies)
ranking = filter(ranking, stid %in% studies)

# Data cleaning

## Remove underage participants

subjects = filter(demo, ord == 2 & as.numeric(val) < 2005)$sid
ratings = filter(ratings, sid %in% subjects)
demo = filter(demo, sid %in% subjects)

## Remove participants who rated insufficient number of stories (items)

required = params$required

subjects = filter(ranking, rank >= required)$sid
ratings = filter(ratings, sid %in% subjects)
demo = filter(demo, sid %in% subjects)