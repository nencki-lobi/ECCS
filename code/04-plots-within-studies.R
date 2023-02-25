# Plots

odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

osubdir = file.path(odir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(osubdir)) {dir.create(osubdir)}

fdir.create = function(name) {
  fdir = file.path(osubdir, name)
  if (!dir.exists(fdir)) {dir.create(fdir)}
  fdir
}

## Plot demographic data

fdir = fdir.create("Fig 1 - Demographics")

df = transposed_demo %>%
  select(sex,age,res,edu,child,work,org,belief,concern) %>%
  mutate(across(where(is.character), as.factor))

plot.fig1 = function(data, variable, name, lnames) {
  ggplot(data, aes(variable)) +
    geom_bar() +
    xlab(name) + ylab("Number of participants") + 
    scale_x_discrete(labels = lnames) +
    beauty + theme(axis.text = element_text(size = 6))
}

### Gender
name = "Gender"
lnames = c("Female", "Male", "Other")
p = plot.fig1(df, df$sex, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Age
name = "Age"
lnames = NULL
p = plot.fig1(df, df$age, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Place of residence
name = "Place of residence"
lnames = c("Rural", "Urban <50k", "Urban 50-100k", "Urban 100-500k", "Urban >500k")
p = plot.fig1(df, df$res, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10))
ggsave(paste0(name, ".png"), p, path = fdir)

### Education
name = "Education"
lnames = c("Primary", "Vocational", "Secondary", "Undergraduate", "Graduate", "Doctoral")
p = plot.fig1(df, df$edu, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Parenthood status
name = "Parenthood status"
lnames = c("Yes", "No")
p = plot.fig1(df, df$child, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Occupation related to climate change
name = "Occupation related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$work, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Activism related to climate change
name = "Activism related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$org, name, lnames)
ggsave(paste0(name, ".png"), p, path = fdir)

### Belief in climate change
name = "Belief in climate change"
lnames = c("Strongly believes",
           "Rather believes",
           "Rather does not believe",
           "Strongly does not believe")
p = plot.fig1(df, df$belief, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10))
ggsave(paste0(name, ".png"), p, path = fdir)

### Concern about climate change
name = "Concern about climate change"
lnames = c("Not concerned",
           "Barely concerned",
           "Somewhat concerned",
           "Very concerned",
           "Extremely concerned",
           "Denies climate change")
p = plot.fig1(df, df$concern, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10))
ggsave(paste0(name, ".png"), p, path = fdir)

## Plot mean ratings for each story with stories ordered by number (ord)

fdir = fdir.create("Fig 2 - Story ratings")

df = story_mean_ratings
  
plot.fig2 = function(data) {
  ggplot(data, aes(x=ord, y=mean, colour=factor(category))) + 
    geom_point() +
    xlim(c(-1,180)) + ylim(c(-1,100)) +
    xlab("Story number") + ylab("Mean ratings") +
    scale_color_manual(values = colors_categories, name = "Story type") + beauty
}

### Separate plots
for(i in 0:6) {
  subdf = filter(df, part == i)
  p = plot.fig2(subdf) +
    labs(title = paste("Mean ratings on", tolower(labels_scales[i+1]), "scale"))
  ggsave(paste0(part_to_scale[i+1], ".png"), p, path = fdir)
}

### Wrapped plots
p = plot.fig2(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) +
  labs(title = "Mean ratings on each of the scales")
ggsave("Fig 2 - Story ratings - Facet by scale.png", p, width = 15, height = 10, path = osubdir)

## Plot mean ratings for each story type

fdir = fdir.create("Fig 3 - Category ratings")

df = story_mean_ratings

plot.fig3 = function(data) {
  ggplot(data, aes(x=category, y=mean, fill=factor(category))) + 
    geom_boxplot() +
    ylim(c(-1,100)) +
    xlab("Story type") + ylab("Mean ratings") +
    scale_fill_manual(name = "Story type", values = colors_categories) + beauty
}

### Separate plots
for(i in 0:6) {
  subdf = filter(df, part == i)
  p = plot.fig3(subdf) +
    labs(title = paste("Mean ratings on", tolower(labels_scales[i+1]),"scale")) +
    theme(legend.position = "none")
  ggsave(paste0(part_to_scale[i+1], ".png"), p, path = fdir)
}

### Wrapped plot
p = df %>% plot.fig3 +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3 - Category ratings - Facet by scale.png", p, width = 15, height = 10, path = osubdir)

p = filter(df, part <= 1) %>% plot.fig3 +
  facet_wrap(~part, ncol = 1, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3a - Category ratings - Facet by scale.png", p, width = 5, height = 6, path = osubdir)

p = filter(df, part > 1) %>% plot.fig3 +
  facet_wrap(~part, ncol = 3, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3b - Category ratings - Facet by scale.png", p, width = 10, height = 8, path = osubdir)

### Grid plot

df = story_mean_ratings_study %>% 
  mutate(part  = factor(part,  labels = labels_scales),
         study = factor(study, labels = c("Study 1", "Study 2")))

p = df %>% plot.fig3 +
  facet_grid(part ~ study) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3 - Category ratings - Facet by scale & study.png", p, width = 10, height = 15, path = osubdir)

p = filter(df, part %in% c("Valence", "Arousal")) %>% plot.fig3 +
  facet_grid(part ~ study) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3a - Category ratings - Facet by scale & study.png", p, width = 6, height = 5, path = osubdir)

p = filter(df, !(part %in% c("Valence", "Arousal"))) %>% plot.fig3 +
  facet_grid(part ~ study) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3b - Category ratings - Facet by scale & study.png", p, width = 8, height = 10, path = osubdir)

## Plot mean ratings for each story with stories ordered by ratings

fdir = fdir.create("Fig 4 - Story ratings ordered")

df = story_mean_ratings %>%
  group_by(part) %>% arrange(mean, .by_group = TRUE) %>% ungroup() %>%
  mutate(index = rep(ords, 7))

plot.fig4 = function(data) {
  ggplot(data, aes(x=index, y=mean, fill = category)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    scale_x_discrete(labels = NULL) +
    xlab("Stories (ordered)") + ylab("Mean ratings") +
    scale_fill_manual(name = "Story type", values = colors_categories) +
    beauty + theme(aspect.ratio = 0.3, axis.ticks.x=element_blank())
}

### Separate plots
for(i in 0:6) {
  subdf = filter(df, part == i)
  
  p = plot.fig4(subdf) +
    labs(title = paste("Mean ratings on", tolower(labels_scales[i+1]), "scale")) +
    theme(legend.position = "none")
  ggsave(paste0(labels_scales[i+1], ".png"), p, path = fdir)
}

### Wrapped plot
p = plot.fig4(df) +
  facet_wrap(~part, ncol = 2, labeller = as_labeller(part_to_scale)) +
  labs(title ="Mean ratings on each of the scales") +
  theme(panel.spacing = unit(2, "lines")) 
ggsave("Fig 4 - Story ratings ordered - Facet by scale.png", p, width = 16, height = 10, path = osubdir)
  
## Plot mean valence and arousal ratings for each story

fdir = fdir.create("Fig 5 - Correlation between valence & arousal")

df = story_mean_ratings %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = c("ord", "category"),
                names_from = part,
                names_sep = ".",
                values_from = "mean")

colnames(df) = c("ord","category","valence","arousal")

plot.fig5 = function(data){
  ggplot(data, aes(x=valence, y=arousal, colour=factor(group))) +
    geom_point() +
    xlim(c(-1,100)) + ylim(c(-1,100)) +
    xlab("Valence") + ylab("Arousal") +
    scale_color_manual(values = c(colors_categories[i], "#D2CFDC"), name = "Story type") + beauty
}

### Separate plots
for(i in 1:6) {
  subdf = df %>% mutate(group = case_when(
    grepl(labels_categories[i], category) ~ labels_categories[i],
    TRUE ~ "Other"))

  p = plot.fig5(subdf) +
    labs(title = paste("Mean valence and arousal for", labels_categories[i], "stories"))
  ggsave(paste0(labels_categories[i], ".png"), p, path = fdir)
}

### Single plot
p = ggplot(df, aes(x=valence, y=arousal, colour=factor(category)))+
  geom_point() +
  xlim(c(-1,100)) + ylim(c(-1,100)) +
  xlab("Valence") + ylab("Arousal") +
  scale_color_manual(values = colors_categories, name = "Story type") +
  labs(title = "Mean valence and arousal for each story type") + beauty
ggsave("Fig 5 - Correlation between valence & arousal.png", p, path = osubdir)

### Grid plot

df = story_mean_ratings_study %>% 
  mutate(study = factor(study, labels = c("Study 1", "Study 2"))) %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = c("ord", "category", "study"),
              names_from = part,
              names_sep = ".",
              values_from = "mean")

colnames(df) = c("ord","category", "study", "valence","arousal")

p = ggplot(df, aes(x=valence, y=arousal, colour=factor(category)))+
  geom_point() +
  xlim(c(-1,100)) + ylim(c(-1,100)) +
  xlab("Valence") + ylab("Arousal") +
  scale_color_manual(values = colors_categories, name = "Story type") +
  facet_wrap(~study, ncol = 3) +
  labs(title = "Mean valence and arousal for each story type") + beauty
ggsave("Fig 5 - Correlation between valence & arousal - Facet by study.png", p, path = osubdir)

## Create a martix of scatterplots to inspect relationships between rating scales

df = transposed_story_mean_ratings %>%
  select("ord", "category", starts_with("mean."))

colnames(df) = c("ord", "category", labels_scales)

png(file.path(osubdir, "Fig 6 - Correlations among all the scales.png"),
    width=12, height=12, units="in", res=300)

par(mar=c(4,4,1,1))

pairs(df[,labels_scales], pch = 19, cex = 1,
      col = colors_categories[df$category],
      cex.labels = 2, 
      cex.axis = 2,
      xlim = c(0,100),
      ylim = c(0,100),
      upper.panel=NULL)

dev.off()

## Plot how climate change concern impacts story ratings

df = full_join(participant_score, select(transposed_demo, "sid", "concern")) %>%
  filter(!is.na(concern)) %>%
  mutate(concern_group = factor(recode(concern, "1"="Low", "2"="Low", "3"="Low", "4"="Medium", "5"="High"),
                                levels = c("Low", "Medium", "High"))) %>%
  relocate(concern_group, .after = concern)

p = ggplot(df, aes(x=study, y=score, fill=concern)) + 
  geom_boxplot() +
  xlab("Study") + ylab("Summary score") + 
  scale_x_discrete(labels = c("Study 1", "Study 2")) +
  scale_fill_brewer(name = "Concern level",
                    palette = "Greys") +
  labs(title = "Impact of climate change concern on story ratings") + beauty
ggsave("Fig 7a - Ratings by CC concern.png", p, path = osubdir)

p = ggplot(df, aes(x=study, y=score, fill=concern_group)) + 
  geom_boxplot() +
  xlab("Study") + ylab("Summary score") + 
  scale_x_discrete(labels = c("Study 1", "Study 2")) +
  scale_fill_brewer(name = "Concern level",
                    palette = "Greys")  +
  labs(title = "Impact of climate change concern on story ratings") + beauty
ggsave("Fig 7b - Ratings by CC concern.png", p, path = osubdir)

## Plot comparison of mean ratings in male and female samples

fdir = fdir.create("Fig 8 - Ratings by gender")

df = full_join(story_mean_ratings_M, story_mean_ratings_F, 
               by = c("ord","code","category","part"), 
               suffix = c(".m", ".w")) %>%
  select("ord","code","category","part","mean.m","mean.w")

plot.fig8 = function(data) {
  ggplot(data, aes(x = mean.m, y = mean.w, label = code, colour=factor(category))) +
    geom_point() +
    xlim(c(-1,100)) + ylim(c(-1,100)) +
    xlab("Mean ratings in male sample") + ylab("Mean ratings in female sample") +
    scale_color_manual(values = colors_categories, name = "Story type") +
    geom_abline(aes(intercept = 0, slope = 1)) +
    geom_label(data = subset(data, abs(mean.w - mean.m) > 25), show.legend = FALSE) +
    beauty
}

### Separate plots
for (i in 0:6) {
  subdf = filter(df, part == i)
  p = plot.fig8(subdf) +
    labs(title = paste("Gender differences in mean ratings of stories on", tolower(labels_scales[i+1]) , "scale"))
  ggsave(paste0(part_to_scale[i+1], ".png"), p, path = fdir)
}

### Wrapped plots
p = plot.fig8(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Gender differences in mean ratings of stories on each scale")
ggsave("Fig 8 - Ratings by gender - Facet by scale.png", p, width = 15, height = 10, path = osubdir)
