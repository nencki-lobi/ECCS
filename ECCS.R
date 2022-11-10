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

stories_rated_ranking = read.table("./data/rtask-subject-ranking.csv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(stories_rated_ranking) = c("sid","code","stid","rank")

## Removing underage respondents

subjects = filter(demo, ord == 2 & as.numeric(val) < 2005)$sid
ratings = filter(ratings, sid %in% subjects)
demo = filter(demo, sid %in% subjects)

## Removing ratings from participants who rated less than k stories

items_required = 10

subjects = filter(stories_rated_ranking, rank >= items_required)$sid
ratings = filter(ratings, sid %in% subjects)
demo = filter(demo, sid %in% subjects)

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

# Adding age column

current_year = 2022

transposed_demo = mutate(transposed_demo, age = current_year - as.numeric(year)) %>%
  relocate(age, .after = year) %>%
  filter(age>17)

# For each story calculate mean ratings on each of the scales

story_mean_ratings = ratings %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n())

# For each story calculate mean ratings for men

men_demo = filter(transposed_demo, transposed_demo$sex == "1")
men_ratings = filter(ratings, ratings$sid %in% men_demo$sid)

men_mean_ratings = men_ratings %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n())

# For each story calculate mean ratings for women

women_demo = filter(transposed_demo, transposed_demo$sex == "0")
women_ratings = filter(ratings, ratings$sid %in% women_demo$sid)

women_mean_ratings = women_ratings %>%
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

labels_scales = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")
labels_categories = c("ANG", "ANX", "COM", "GUI", "HOP", "NEU")

# Plots for stories

ords = 0:179
categories = ord_to_category[as.character(ords)]

for(i in 0:6) {
  scale_data = filter(story_mean_ratings, part == i)
  
  # Plots showing mean ratings for each story on each scale
  p1 = ggplot(scale_data, aes(x=ords, y=mean)) + 
    geom_point() +
    labs(title = paste(labels_scales[i+1], "- mean ratings per story"))
  
  ggsave(paste(labels_scales[i+1], "- scatter.png"), p1, path = pdir)
  
  # Plots showing mean ratings for each emotion category on each scale
  p2 = ggplot(scale_data, aes(x=categories, y=mean)) + 
    geom_boxplot()  +
    labs(title = paste(labels_scales[i+1], "- mean ratings per category"))
  
  ggsave(paste(labels_scales[i+1], "- box.png"), p2, path = pdir)
}

# Plot for valence and arousal for each story 

df = story_mean_ratings %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = "ord",
            names_from = part,
            names_sep = ".",
            values_from = "mean") %>%
  mutate(category = ord_to_category[as.character(ord)])

colnames(df) = c("ord","valence","arousal","category")

p4 = ggplot(df,aes(x=valence, y=arousal)) +
  geom_point()

# Plots for valence and arousal for each emotion category

p5 = ggplot(df,aes(x=valence, y=arousal, colour=factor(category))) +
    geom_point()


# Plots for participants

df = full_join(transposed_demo,participant_mean_ratings) %>%
  mutate(concern_group = recode(concern,"1"="1", "2"="1", "3"="1","4"="2","5"="3")) %>%
  relocate(concern_group, .after = concern)

for(i in 1:6) {
  category_data = select(df, sid, concern_group, part, mean, category) %>%
    filter(category == labels_categories[i])
  
  # Plots showing how CC concern impacts ratings for each stimulus category
  p3 = ggplot(category_data, aes(x=as.character(part), y=mean, fill=as.character(concern_group))) + 
    geom_boxplot() +
    labs(title = paste("Impact of concern level on mean ratings for", labels_categories[i], "stories on each of the scales")) +
    scale_x_discrete(name = "Scales", labels = labels_scales) +
    scale_fill_manual(values = c("#1E555C", "#257e8a", "#9BC53D", "#475052"), name = "Concern level", labels = c("Low","Medium","High","In denial"))
  
  ggsave(paste(labels_categories[i], "- box.png"), p3, path = pdir)
}