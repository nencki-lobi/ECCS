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

df = transposed_demo

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
p = plot.fig1(df, df$sex, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Age
name = "Age"
lnames = NULL
p = plot.fig1(df, df$age, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Place of residence
name = "Place of residence"
lnames = c("Rural", "Urban <50K", "Urban 50-100K", "Urban >100K")
p = plot.fig1(df, df$res_group, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10)) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Education
name = "Education"
lnames = c("Primary", "Secondary", "Higher", "Other")
p = plot.fig1(df, df$edu_group, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Parenthood status
name = "Parenthood status"
lnames = c("Yes", "No")
p = plot.fig1(df, df$child, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Occupation related to climate change
name = "Occupation related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$work, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Activism related to climate change
name = "Activism related to climate change"
lnames = c("Yes", "No")
p = plot.fig1(df, df$org, name, lnames) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Belief in climate change
name = "Belief in climate change"
lnames = c("Strongly believes",
           "Rather believes",
           "Rather does not believe",
           "Strongly does not believe")
p = plot.fig1(df, df$belief, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10)) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

### Concern about climate change
name = "Concern about climate change"
lnames = c("Low",
           "Middle",
           "High",
           "Denies climate change")
p = plot.fig1(df, df$concern_group, name, lnames) +
  scale_x_discrete(labels = str_wrap(lnames, width = 10)) + 
  facet_wrap(~study, ncol = 3)
ggsave(paste0(name, ".png"), p, width = 8, height = 3, path = fdir)

## Plot mean ratings for each story with stories ordered by number (ord)

fdir = fdir.create("Fig 2 - Story ratings")

df = story_mean_ratings
  
plot.fig2 = function(data) {
  ggplot(data, aes(x=ord, y=mean, colour=category)) + 
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
  ggplot(data, aes(x=category, y=mean, fill=category)) + 
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
  mutate(part  = factor(part,  labels = labels_scales))

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
  
## Plot mean valence and arousal ratings for each story

fdir = fdir.create("Fig 5 - Correlation between valence & arousal")

df = story_mean_ratings %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = c("ord", "category"),
                names_from = part,
                names_sep = ".",
                values_from = "mean")

colnames(df) = c("ord", "category", "valence", "arousal")

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
p = ggplot(df, aes(x=valence, y=arousal, colour=category))+
  geom_point() +
  xlim(c(-1,100)) + ylim(c(-1,100)) +
  xlab("Valence") + ylab("Arousal") +
  scale_color_manual(values = colors_categories, name = "Story type") +
  labs(title = "Mean valence and arousal for each story type") + beauty
ggsave("Fig 5 - Correlation between valence & arousal.png", p, path = osubdir)

### Grid plot

df = story_mean_ratings_study %>%
  filter(part =="0" | part =="1") %>%
  pivot_wider(id_cols = c("ord", "category", "study"),
              names_from = part,
              names_sep = ".",
              values_from = "mean")

colnames(df) = c("ord", "category", "study", "valence", "arousal")

p = ggplot(df, aes(x=valence, y=arousal, colour=category))+
  geom_point() +
  xlim(c(-1,100)) + ylim(c(-1,100)) +
  xlab("Valence") + ylab("Arousal") +
  scale_color_manual(values = colors_categories, name = "Story type") +
  facet_wrap(~study, ncol = 3) +
  labs(title = "Mean valence and arousal for each story type") + beauty
ggsave("Fig 5 - Correlation between valence & arousal - Facet by study.png", p, width = 8, height = 3, path = osubdir)

## Plot how climate change concern impacts story ratings

df = full_join(participant_score, select(transposed_demo, "sid", "concern", "concern_group")) %>%
  filter(!is.na(concern))

p = ggplot(df, aes(x=concern, y=score, fill=category, alpha=concern)) +
  geom_boxplot(outlier.alpha = 1, notch = TRUE, width = 0.5) +
  xlab("Concern level") + ylab("Summary score") +
  scale_fill_manual(name = "Story type", values = colors_categories) +
  scale_alpha_manual(name = "Concern level", values = c(0.1, 0.3, 0.5, 0.7, 0.9),
                     guide = "none") +
  labs(title = "Impact of climate change concern on story ratings") + beauty

p = p + facet_wrap(~category) +
  theme(aspect.ratio = 1.5,
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
ggsave("Fig 7a - Ratings by CC concern.png", p, path = osubdir)

p = ggplot(df, aes(x=concern_group, y=score, fill=category, alpha=concern_group)) +
  geom_boxplot(outlier.alpha = 1, notch = TRUE, width = 0.5) +
  xlab("Concern level") + ylab("Summary score") +
  scale_fill_manual(name = "Story type", values = colors_categories) +
  scale_alpha_manual(name = "Concern level", values = c(0.15, 0.5, 0.85),
                     guide = "none") +
  labs(title = "Impact of climate change concern on story ratings") + beauty

p = p + facet_wrap(~category) +
  theme(aspect.ratio = 1.5,
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
ggsave("Fig 7b - Ratings by CC concern.png", p, path = osubdir)
