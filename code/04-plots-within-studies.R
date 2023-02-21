# Plots

pdir = "./output"
if (!dir.exists(pdir)) {dir.create(pdir)}

psubdir = file.path(pdir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(psubdir)) {dir.create(psubdir)}

## Plot demographic data

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
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Age
name = "Age"
lnames = NULL
p = plot.fig1(df, df$age, name, lnames)
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Place of residence
name = "Place of residence"
lnames = c("Rural", "Urban <50k", "Urban 50-100k", "Urban 100-500k", "Urban >500k")
p = plot.fig1(df, df$res, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10))
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Education
name = "Education"
lnames = c("Primary", "Vocational", "Secondary", "Undergraduate", "Graduate", "Doctoral")
p = plot.fig1(df, df$edu, name, lnames)
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Parenthood status
name = "Parenthood status"
lnames = c("Yes", "No")
p = plot.fig1(df, df$child, name, lnames)
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Occupation related to climate change
name = "Occupation related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$work, name, lnames)
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Activism related to climate change
name = "Activism related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$org, name, lnames)
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

### Belief in climate change
name = "Belief in climate change"
lnames = c("Strongly believes",
           "Rather believes",
           "Rather does not believe",
           "Strongly does not believe")
p = plot.fig1(df, df$belief, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10))
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

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
ggsave(paste0("Fig 1 - Demographics - ", name, ".png"), p, path = psubdir)

## Plot mean ratings for each story with stories ordered by number (ord)

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
  ggsave(paste("Fig 2 -", part_to_scale[i+1], "- ratings by story number.png"), p, path = psubdir)
}

### Wrapped plots
p = plot.fig2(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) +
  labs(title = "Mean ratings on each of the scales")
ggsave("Fig 2 - Wrap - ratings by story number.png", p, width = 15, height = 10, path = psubdir)

## Plot mean ratings for each story type

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
  ggsave(paste("Fig 3 -", part_to_scale[i+1], "- ratings by story type.png"), p, path = psubdir)
}

### Wrapped plot
p = df %>% plot.fig3 +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3 - Wrap - ratings by story type.png", p, width = 15, height = 10, path = psubdir)

p = filter(df, part <= 1) %>% plot.fig3 +
  facet_wrap(~part, ncol = 1, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3a - Wrap - ratings by story type.png", p, width = 5, height = 6, path = psubdir)

p = filter(df, part > 1) %>% plot.fig3 +
  facet_wrap(~part, ncol = 3, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Mean ratings on each of the scales") +
  scale_x_discrete(labels = NULL)
ggsave("Fig 3b - Wrap - ratings by story type.png", p, width = 10, height = 8, path = psubdir)

## Plot mean ratings for each story with stories ordered by ratings

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
  ggsave(paste("Fig 4 - Stories ordered by", tolower(labels_scales[i+1]), "ratings.png"), p, path = psubdir)
}

### Wrapped plot
p = plot.fig4(df) +
  facet_wrap(~part, ncol = 2, labeller = as_labeller(part_to_scale)) +
  labs(title ="Mean ratings on each of the scales") +
  theme(panel.spacing = unit(2, "lines")) 
ggsave("Fig 4 - Wrap - stories ordered by ratings.png", p, width = 16, height = 10, path = psubdir)
  
## Plot mean valence and arousal ratings for each story

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
  ggsave(paste("Fig 5 -", labels_categories[i], "- valence & arousal ratings.png"), p, path = psubdir)
}

### Single plot
p = ggplot(df, aes(x=valence, y=arousal, colour=factor(category)))+
  geom_point() +
  xlim(c(-1,100)) + ylim(c(-1,100)) +
  xlab("Valence") + ylab("Arousal") +
  scale_color_manual(values = colors_categories, name = "Story type") +
  labs(title = "Mean valence and arousal for each story type") + beauty
ggsave("Fig 5 - All - valence & arousal ratings.png", p, path = psubdir)
  
## Create a martix of scatterplots to inspect relationships between rating scales

df = transposed_story_mean_ratings %>%
  select("ord", "category", starts_with("mean."))

colnames(df) = c("ord", "category", labels_scales)

png(file.path(psubdir, "Fig 6 - Scatterplot matrix.png"),
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

## Plot how climate change concern impacts mean ratings

df = full_join(transposed_demo,participant_mean_ratings) %>%
  mutate(concern_group = recode(concern, "1"="1", "2"="1", "3"="1", "4"="2", "5"="3")) %>%
  mutate(across(c("part", "concern_group"), as.character)) %>%
  relocate(concern_group, .after = concern)

plot.fig7 = function(data) {
  ggplot(data, aes(x=part, y=mean, fill=concern_group)) + 
    geom_boxplot() +
    ylim(c(-1,100)) +
    xlab("Scales") + ylab("Mean ratings") +
    scale_x_discrete(labels = labels_scales) +
    scale_fill_manual(name = "Climate change concern",
                      labels = c("Low","Medium","High","In denial"),
                      values = c("#E8EC67", "#659157", "#0A3200", "#475052")) + beauty
}

### Separate plots
for (i in 1:6) {
  subdf = filter(df, category == labels_categories[i])
  p = plot.fig7(subdf) +
    labs(title = paste("Impact of climate change concern on ratings for", labels_categories[i], "stories"))
  ggsave(paste("Fig 7 -",labels_categories[i], "- ratings by CC concern.png"), p, path = psubdir)
}

### Wrapped plots 
p = plot.fig7(df) +
  facet_wrap(~category, ncol = 3) +
  labs(title = "Impact of climate change concern on ratings for each story type")
ggsave("Fig 7 - Wrap - ratings by CC concern.png",p , width = 15, height = 10, path = psubdir)

## Plot comparison of mean ratings in male and female samples

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
  ggsave(paste("Fig 8 -", part_to_scale[i+1], "- ratings by gender.png"), p, path = psubdir)
}

### Wrapped plots
p = plot.fig8(df) +
  facet_wrap(~part, ncol = 4, labeller = as_labeller(part_to_scale)) + 
  labs(title = "Gender differences in mean ratings of stories on each scale")
ggsave("Fig 8 - Wrap - ratings by gender.png", p, width = 15, height = 10, path = psubdir)
