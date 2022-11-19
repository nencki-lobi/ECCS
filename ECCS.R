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

items_required = 0

subjects = filter(stories_rated_ranking, rank >= items_required)$sid
ratings = filter(ratings, sid %in% subjects)
demo = filter(demo, sid %in% subjects)

# Define helpers

ords = 0:179
labels_scales = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")
labels_categories = c("ANG", "ANX", "COM", "GUI", "HOP", "NEU")
colors_categories = c("#E05263", "#659157", "#FFCAB1", "#303633", "#69A2B0", "#96949B")

code_to_ord = ords
names(code_to_ord) = items$code

ord_to_code = items$code
names(ord_to_code) = ords

ord_to_category = substr(items$code, 1, 3)
names(ord_to_category) = ords

part_to_scale = labels_scales
names(part_to_scale) = c(0:6)

categories = ord_to_category[as.character(ords)]

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

beauty = theme_linedraw() + theme(panel.grid = element_blank(), aspect.ratio = 1)

# Plots for stories

# Plots showing mean ratings for each story on each scale

df = story_mean_ratings %>%
  mutate(category = ord_to_category[as.character(ord)])
  
plot.fig1 <- function(data) {
  ggplot(data, aes(x=ord, y=mean, colour=factor(category))) + 
    geom_point() +
    xlim(c(-1,180)) + ylim(c(-1,100)) +
    xlab("Stories") +
    scale_color_manual(values = colors_categories, name = "Story category") + beauty
}

# Separate plots
  
for(i in 0:5) {
  subdf = filter(df, part == i)
  p = plot.fig1(subdf) + labs(title = paste("Mean story ratings on", labels_scales[i+1], "scale"))  + ylab(paste(labels_scales[i+1],"mean ratings"))
  ggsave(paste("Fig 1 -", part_to_scale[i+1], "- scatter.png"), p, path = psubdir)
}

# Wrapped plots
Fig1 = plot.fig1(df) + facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + theme(aspect.ratio = 1.1) + 
  labs(title ="Mean story ratings on each of the scales") + ylab("Mean ratings")
ggsave("Fig 1 - Wrap Scatter.png", Fig1, width = 15, height = 10, path = psubdir)

# Plots showing mean ratings for each emotion category on each scale

plot.fig2 <-function(data) {
  ggplot(data, aes(x=ord, y=mean,colour=factor(category))) + 
    geom_boxplot()  +
    ylim(c(-1,100)) +
    xlab("")+
    ylab(paste(labels_scales[i+1],"mean ratings")) +
    scale_x_discrete() +
    scale_color_manual(values = colors_categories, name = "Story category") +
    labs(title = "Mean scale ratings in each story category") + beauty
}

# Separate plots

for(i in 0:5) {
  subdf = filter(df, part == i)
  p = plot.fig2(subdf) + labs(title = paste( "Mean scale ratings in", labels_scales[i+1] ,"category"))
  ggsave(paste("Fig 2 -", part_to_scale[i+1], "- box.png"), p, path = psubdir)
}

# Wrapped plot
  Fig2 = plot.fig2(df) + facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + ylab("Mean ratings")
  ggsave("Fig 2 - Wrap Box.png", Fig2, width = 15, height = 10, path = psubdir)
  

# Plot for valence and arousal for each story on one plot

df = story_mean_ratings %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = "ord",
                names_from = part,
                names_sep = ".",
                values_from = "mean") %>%
  mutate(category = ord_to_category[as.character(ord)])

colnames(df) = c("ord","valence","arousal","category")

Fig3 = ggplot(df,aes(x=valence,y=arousal, colour=factor(category)))+
  geom_point() + 
  labs(title = "Mean valence and arousal for each story") +
  scale_color_manual(values = colors_categories, name = "Story category") +
  xlim(c(-1,100)) + ylim(c(-1,100)) + beauty

ggsave("Fig 3 - Valence_x_Arousal.png",Fig3 , path = psubdir)


#Separate plots
df %>% mutate(is_category = case_when(
  !grepl(labels_categories[i],category) ~ "Group1",
  TRUE ~ "Group2"))

plot.fig3 = function(data){
  ggplot(data,aes(x=valence, y=arousal, colour=factor(group))) +
    geom_point() +
    labs(title = paste("Mean valence and arousal of the stories from", labels_categories[i], "category"), colour = "Emotion category") +
    scale_color_manual(values = c(colors_categories[i], "#D2CFDC")) +
    xlim(c(-1,100)) + ylim(c(-1,100)) + beauty
}

for(i in 1:6) {
  subdf = df %>% mutate(is_category = case_when(
    grepl(labels_categories[i], category) ~ labels_categories[i],
    TRUE ~"Other"))
  colnames(subdf) = c("ord","valence","arousal","category","group")
  p = plot.fig3(subdf)
  ggsave(paste("Fig 3 -", labels_categories[i], "- vs all other.png"), p, path = psubdir)
}
  
# Plots showing how CC concern impacts ratings for each stimulus category

df = full_join(transposed_demo,participant_mean_ratings) %>%
  mutate(concern_group = recode(concern,"1"="1", "2"="1", "3"="1","4"="2","5"="3")) %>%
  relocate(concern_group, .after = concern)

plot.fig4 = function(data) {
  ggplot(data, aes(x=as.character(part), y=mean, fill=as.character(concern_group))) + 
    geom_boxplot() +
    scale_x_discrete(name = "Scales", labels = labels_scales) +
    scale_fill_manual(values = c("#E8EC67", "#659157", "#0A3200", "#475052"), name = "Concern level", labels = c("Low","Medium","High","In denial")) + beauty
}

# Wrapped plots 

Fig4 = plot.fig4(df) + facet_wrap(~category, ncol = 3, labeller = as_labeller(part_to_scale)) +
  labs(title = "Impact of concern level on mean ratings on all scales for all stories")
ggsave("Fig 4 - Wrap CC_concern.png",Fig4 , width = 15, height = 12, path = psubdir)

#Separate plots

for (i in 1:6) {
  subdf = filter(df, category == labels_categories[i])
  p = plot.fig4(subdf) +
    labs(title = paste("Impact of concern level on mean ratings for stories from", labels_categories[i],"category"))
  ggsave(paste("Fig 4 -",labels_categories[i], "- cc_concern.png"), p, path = psubdir)
}

# Plots of mean ratings on each scale for men and women

df = full_join(men_mean_ratings, women_mean_ratings, by = c("ord","part"), suffix = c(".m", ".w")) %>%
  select("ord","part","mean.m","mean.w") %>%
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = ord_to_category[as.character(ord)])

plot.fig5 = function(data) {
  ggplot(data, aes(x = mean.m, y = mean.w, label = code, colour=factor(category))) +
    scale_color_manual(values = colors_categories, name = "Story category") +
    geom_point() +
    geom_abline(aes(intercept = 0, slope = 1)) +
    geom_label(data = subset(data, abs(mean.w - mean.m) > 25), show.legend = FALSE) +
    xlab("Men mean ratings") + ylab("Women mean ratings") +
    xlim(c(-1,100)) + ylim(c(-1,100)) + beauty
}

# Wrapped plots
Fig5 = plot.fig5(df) + facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  theme(aspect.ratio = 1.2) + labs(title = "Gender differences in mean ratings of stories on each scale")
ggsave("Fig 5 - Wrap Gender.png", Fig5, width = 15, height = 10, path = psubdir)

#Separate plots
for (i in 0:6) {
  subdf = filter(df, part == i)
  p = plot.fig5(subdf) + labs(title = paste("Gender differences in mean ratings of stories on", labels_scales[i+1] ,"scale"))
  ggsave(paste("Fig 5 -", part_to_scale[i+1], "- gender.png"), p, path = psubdir)
}

#Descriptives
  
pdir = "./descriptives"
if (!dir.exists(pdir)) {dir.create(pdir)}

descriptives = transposed_demo %>%
  select(sex,age,res,edu,child,work,org,belief,concern) %>%
  mutate(across(where(is.character), as.factor))
  
sink(file = "./descriptives/summary_descriptives.txt")
summary(descriptives)
sink(file = NULL)

fig_gender = ggplot(descriptives, aes(x = sex, fill=sex)) +
  geom_bar() +
  labs(title = "Gender distribution") +
  scale_x_discrete(name = "", labels = c("Women", "Men", "Other")) + ylab("Count") +
  scale_fill_manual(values = c("#659157","#E05263", "#69A2B0"),
                    name = "Gender",
                    labels = c("Women", "Men", "Other")) + beauty
ggsave("Fig 6 gender.png",fig_gender , path = "./descriptives")

fig_age = ggplot(descriptives, aes(age)) +geom_bar() +
  labs(title = "Age distribution", xlab("Age")) +
  xlab("Age") + ylab("Count") + beauty
ggsave("Fig 6 age.png",fig_age, path = "./descriptives")

fig_res = ggplot(descriptives, aes(res, fill=res)) + geom_bar() +
  labs(title = "Place of residence") +
  scale_x_discrete(name = "", labels = c("Rural", "", "", "","Urban")) + ylab("Count") +
  scale_fill_manual(values = c("#E05263", "#659157", "#FFCAB1", "#303633", "#69A2B0"),
                    name = "Place",
                    labels = c("Rural", "Urban <50k", "Urban 50k-100k", "Urban 100k-500k","Urban >500k")) + beauty
ggsave("Fig 6 res.png",fig_res , path = "./descriptives")

fig_edu = ggplot(descriptives, aes(edu, fill=edu)) +geom_bar() + 
  labs(title = "Education level") +
  scale_x_discrete(name = "", labels = c("Primary", "", "","", "", "","Doctoral")) + ylab("Count") +
  scale_fill_manual(values = c("#E05263", "#659157", "#FFCAB1", "#303633", "#69A2B0", "#96949B"),
                    name = "Education",
                    labels = c("Primary", "Vocational","Secondary",
                               "Undergraduate", "Graduate","Doctoral")) + beauty
ggsave("Fig 6 edu.png",fig_edu ,width = 20, height = 10, path = "./descriptives")


fig_child = ggplot(descriptives, aes(child, fill=child)) +geom_bar() +
  labs(title = "Parenthood status") +
  scale_x_discrete(name = "", labels = c("Yes", "No")) + ylab("Count") +
  scale_fill_manual(values = c("#659157","#E05263"),
                    name = "Respondend has child(ren)",
                    labels = c("Yes", "No")) + beauty
ggsave("Fig 6 child.png",fig_child , path = "./descriptives")

fig_work = ggplot(descriptives, aes(work, fill=work)) +geom_bar() +
  labs(title = "Occupational status") +
  scale_x_discrete(name = "", labels = c("Yes", "No")) + ylab("Count") +
  scale_fill_manual(values = c("#659157","#E05263"),
                    name = "Work related to climate",
                    labels = c("Yes", "No")) + beauty
ggsave("Fig 6 work.png",fig_work , path = "./descriptives")

fig_org = ggplot(descriptives, aes(org, fill=org)) +geom_bar() +
  labs(title = "Activism and volunteering") +
  scale_x_discrete(name = "", labels = c("Yes", "No")) + ylab("Count") +
  scale_fill_manual(values = c("#659157","#E05263"),
                    name = "Involvement in climate action",
                    labels = c("Yes", "No")) + beauty
ggsave("Fig 6 org.png",fig_org , path = "./descriptives")

fig_belief = ggplot(descriptives, aes(belief, fill=belief)) + geom_bar() +
  labs(title = "Belief in climate change") +
  scale_x_discrete(name = "", labels = c("Believes", "", "", "Disbelieves")) + ylab("Count") +
  scale_fill_manual(values = c("#E05263", "#659157", "#FFCAB1", "#303633", "#69A2B0"),
                    name = "Respondent",
                    labels = c("Believes in climate change", "Rather believes", 
                               "Rather disbelieves", "Disbelieves in climate change")) + beauty
ggsave("Fig 6 belief.png",fig_belief , path = "./descriptives")

fig_concern = ggplot(descriptives, aes(concern, fill=concern)) + geom_bar() +
  labs(title = "Concern about climate change") +
  scale_x_discrete(name = "", labels = c("Not concerned", "", "", "", "Not concerned", "")) + ylab("Count") +
  scale_fill_manual(values = c("#E05263", "#659157", "#FFCAB1", "#303633", "#69A2B0"),
                    name = "Respondent is:",
                    labels = c("Not concerned", "Barely concerned", "Somewhat concerned", "Very concerned",
                               "Extremely concerned","Doesn't believe in climate change")) + beauty
ggsave("Fig 6 concern.png",fig_concern , path = "./descriptives")

