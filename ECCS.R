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

psubdir = file.path(pdir, paste0("items-required-",as.character(items_required)))
if (!dir.exists(psubdir)) {dir.create(psubdir)}

ords = 0:179
categories = ord_to_category[as.character(ords)]
labels_scales = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")
labels_categories = c("ANG", "ANX", "COM", "GUI", "HOP", "NEU")

beauty = theme_linedraw() + theme(panel.grid = element_blank(), aspect.ratio = 1)

# Plots for stories

for(i in 0:6) {
  scale_data = filter(story_mean_ratings, part == i)
  
  # Plots showing mean ratings for each story on each scale
  p1 = ggplot(scale_data, aes(x=ords, y=mean)) + 
    geom_point() +
    xlim(c(-1,180)) + ylim(c(-1,100)) +
    labs(title = paste(labels_scales[i+1], "- mean ratings per story on each of the scales")) + beauty
  
  ggsave(paste(labels_scales[i+1], "- scatter.png"), p1, path = psubdir)
  
  # Plots showing mean ratings for each emotion category on each scale
  p2 = ggplot(scale_data, aes(x=categories, y=mean)) + 
    geom_boxplot()  +
    ylim(c(-1,100)) +
    labs(title = paste(labels_scales[i+1], "- mean ratings per category")) + beauty
  
  ggsave(paste(labels_scales[i+1], "- box.png"), p2, path = psubdir)
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

p3 = ggplot(df,aes(x=valence, y=arousal)) +
  geom_point()+
  xlim(c(-1,100)) + ylim(c(-1,100)) + beauty

ggsave("valece_x_arousal.png", p3, path = psubdir)

# Plots for valence and arousal for each emotion category on one plot
p4 = ggplot(df,aes(x=valence,y=arousal, colour=factor(category)))+
  geom_point() + 
  xlim(c(-1,100)) + ylim(c(-1,100)) + beauty

ggsave(paste(labels_categories[i],"vs all - scatter.png"),p4, path = psubdir)

# Plots for valence and arousal for each emotion category
df %>% mutate(is_category = case_when(
  grepl(labels_categories[i],category) ~ "Group1",
  TRUE ~ "Group2"))

for (i in 1:6) {
  tmp = df %>% mutate(is_category = case_when(
    grepl(labels_categories[i], category) ~ "Group1",
    TRUE ~"Group2"))
  colnames(tmp) = c("ord","valence","arousal","category","group")
  p5 = ggplot(tmp,aes(x=valence, y=arousal, colour=factor(group))) +
    geom_point() +
    xlim(c(-1,100)) + ylim(c(-1,100)) + beauty
  
  ggsave(paste(labels_categories[i],"vs all - scatter.png"),p5, path = psubdir)
}

# Plots of mean ratings on each scale for men and women
df = full_join(men_mean_ratings, women_mean_ratings, by = c("ord","part"), suffix = c(".m", ".w")) %>%
  select("ord","part","mean.m","mean.w") %>%
  mutate(code = ord_to_code[as.character(ord)])

for (i in 1:7) {
  tmp = filter(df, part == i)
  p6 = ggplot(tmp, aes(x = mean.m, y = mean.w)) +
    geom_point() +
    geom_abline(aes(intercept = 0, slope = 1)) +
    geom_label(data = subset(tmp, abs(mean.w - mean.m) > 25),
               aes(x = mean.w, y = mean.m, label = code)) +
    xlim(c(-1,100)) + ylim(c(-1,100)) + beauty
  
  ggsave(paste(labels_scales[i],"- men_vs_women.png"),p6, path = psubdir)
}

# Plots for participants

df = full_join(transposed_demo,participant_mean_ratings) %>%
  mutate(concern_group = recode(concern,"1"="1", "2"="1", "3"="1","4"="2","5"="3")) %>%
  relocate(concern_group, .after = concern)

for(i in 1:6) {
  category_data = select(df, sid, concern_group, part, mean, category) %>%
    filter(category == labels_categories[i])
  
  # Plots showing how CC concern impacts ratings for each stimulus category
  p7 = ggplot(category_data, aes(x=as.character(part), y=mean, fill=as.character(concern_group))) + 
    geom_boxplot() +
    labs(title = paste("Impact of concern level on mean ratings for", labels_categories[i], "stories on each of the scales")) +
    scale_x_discrete(name = "Scales", labels = labels_scales) +
    scale_fill_manual(values = c("#1E555C", "#257e8a", "#9BC53D", "#475052"), name = "Concern level", labels = c("Low","Medium","High","In denial")) + beauty
  
  ggsave(paste(labels_categories[i], "- box.png"), p7, path = psubdir)
}