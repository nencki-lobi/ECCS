library('dplyr')
library('tidyr')
library('ggplot2')

# Load data

ratings = read.table("./data/rtask-ratings.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ratings) = c("sid","code","stid","name","ord", "part", "opt")

demo = read.table("./data/rtask-demo.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(demo) = c("sid","code","stid","name","ord", "val")

items = read.table("./data/rtask-items.tsv", header = F, sep = "\t", encoding = "UTF-8")
colnames(items) = c("PL","EN","NO","code")

# Define helpers

code_to_ord = c(0:179)
names(code_to_ord) = items$code

ord_to_code = items$code
names(ord_to_code) = c(0:179)

ord_to_category = substr(items$code, 1, 3)
names(ord_to_category) = c(0:179)

# How to use: 
# code_to_ord["GUI2"]
# ord_to_code["0"]
# ord_to_category["0"]

# Change data format from long to wide

transposed_ratings = ratings %>%
  pivot_wider(id_cols = c("sid", "code", "stid"),
              names_from = c("name", "ord", "part"),
              names_sep = ".",
              names_sort = T,
              values_from = "opt")

transposed_demo = demo %>%
  pivot_wider(id_cols = c("sid", "code", "stid"),
              names_from = c("name", "ord"),
              names_sep = ".",
              values_from = "val")

transposed_demo = filter(transposed_demo, sid %in% transposed_ratings$sid)
colnames(transposed_demo) = c("sid","code","stid","sex","year","res","edu","child","work","org","belief","concern","sex_other","edu_other")

# For each story calculate mean ratings on each of the scales

story_mean_ratings = ratings %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n())

# For each story obtain demographic profile

subjects.ords = select(ratings, sid, code, stid, ord) %>% distinct()

story_sex_count = full_join(subjects.ords, transposed_demo, by = "sid") %>%
  group_by(ord, sex) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = "ord",
              names_from = sex,
              names_sep = ".",
              values_from = "n")

story_concern_count = full_join(subjects.ords, transposed_demo, by = "sid") %>%
  group_by(ord, concern) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = "ord",
              names_from = concern,
              names_sep = ".",
              values_from = "n")

# For each participant calculate mean ratings for each stimulus category and each rating scale

participant_mean_ratings =
  mutate(ratings, category = ord_to_category[as.character(ratings$ord)]) %>%
  group_by(sid, code, category, part) %>%
  summarise(mean = mean(opt), n = n())

# Plots

pdir = "./plots"
if (!dir.exists(pdir)) {dir.create(pdir)}

labels = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")

ords = 0:179
categories = ord_to_category[as.character(ords)]

for(i in 0:6) {
  scale = filter(story_mean_ratings, part == i)
  
  p1 = ggplot(scale, aes(x=ords, y=mean)) + 
    geom_point() +
    labs(title = paste(labels[i+1], "- mean ratings per story"))
  
  ggsave(paste(labels[i+1], "- scatter.png"), p1, path = pdir)
  
  p2 = ggplot(scale, aes(x=categories, y=mean)) + 
    geom_boxplot()  +
    labs(title = paste(labels[i+1], "- mean ratings per category"))
  
  ggsave(paste(labels[i+1], "- box.png"), p2, path = pdir)
}