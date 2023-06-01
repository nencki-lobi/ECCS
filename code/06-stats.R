pdir = "./output"
if (!dir.exists(pdir)) {dir.create(pdir)}

psubdir = file.path(pdir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(psubdir)) {dir.create(psubdir)}

# Define functions

print.me = function(title, result) {
  cat("\n", title, "\n", "\n")
  print(result)
}

get.tally = function(df, gvar, val) {
  df %>%
    select(gvar = {{gvar}}, val = {{val}}) %>%
    group_by(gvar, val) %>%
    tally() %>%
    mutate(prop = prop.table(n)) %>%
    pivot_wider(id_cols = "val",
                names_from = "gvar",
                names_sep = ".",
                names_sort = T,
                values_from = c("n","prop")) %>%
    print.data.frame(row.names = F)
}

get.means = function(df, gvar, val1, val2, val3) {
  df %>%
    select(gvar = {{gvar}}, {{val1}}, {{val2}}, {{val3}}) %>%
    group_by(gvar) %>%
    summarise_all(mean) %>%
    print.data.frame(row.names = F)
}

get.correlations = function(df, gvar, val1, val2) {
  df %>% 
    select(gvar = {{gvar}}, val1 = {{val1}}, val2 = {{val2}}) %>% 
    group_by(gvar) %>%
    nest() %>% 
    mutate(
      pearson = map(data, ~cor.test(.x$val1, .x$val2)),
      glanced = map(pearson, glance)
    ) %>% 
    unnest(glanced) %>% 
    select(gvar, r = estimate, conf.low, conf.high, p.value) %>% 
    arrange(gvar) %>%
    print.data.frame(row.names = F)
}

get.anova.with.posthocs = function(df, dv, iv) {
  
  formula = reformulate(response = dv, 
                        termlabels = iv)
  
  df %>% anova_test(formula) %>%
    print.data.frame(row.names = F)
  
  df %>% tukey_hsd(formula) %>%
    print.data.frame(row.names = F)
}

check.normality = function(df, gvar, val) {
  df %>% 
    select(gvar = {{gvar}}, val = {{val}}) %>% 
    group_by(gvar) %>% 
    nest() %>% 
    mutate(
      shapiro = map(data, ~shapiro.test(.x$val)),
      glanced = map(shapiro, glance)
    ) %>% 
    unnest(glanced) %>% 
    select(gvar, W = statistic, p.value) %>% 
    arrange(gvar) %>%
    print.data.frame(row.names = F)
}

# Difference in story length across categories

df = stories %>%
  mutate(len_PL = nchar(PL), len_EN = nchar(EN), len_NO = nchar(NO))

sink(file = file.path(psubdir,"Differences - story length across categories.txt"), type ="output")

print.me("Polish stories - summary statistics", summary(df$len_PL))
print.me("Polish stories - difference in story length across categories (ANOVA & Tukey post-hocs)", 
         get.anova.with.posthocs(df, "len_PL", "category"))

print.me("English stories - summary statistics", summary(df$len_EN))
print.me("English stories - difference in story length across categories (ANOVA & Tukey post-hocs)", 
         get.anova.with.posthocs(df, "len_EN", "category"))

print.me("Norwegian stories - summary statistics", summary(df$len_NO))
print.me("Norwegian stories - difference in story length across categories (ANOVA & Tukey post-hocs)", 
         get.anova.with.posthocs(df, "len_NO", "category"))

sink(file = NULL)

# Summary statistics of demographic data

df = transposed_demo %>%
  select(-c("sid", "code", "stid", "birth"))

sink(file = file.path(psubdir, "Demographics - summary statistics.txt"), type ="output")

cat("\n", "Descriptive statistics by study:", "\n", "\n")

ldf = describe(df ~ study)
do.call("rbind", ldf) %>% 
  print.data.frame()

print.me("Sex", get.tally(df, "study", "sex"))
print.me("Residence", get.tally(df, "study", "res_group"))
print.me("Education", get.tally(df, "study", "edu_group"))
print.me("Child", get.tally(df, "study", "child"))
print.me("Work", get.tally(df, "study", "work"))
print.me("Organization", get.tally(df, "study", "org"))
print.me("Belief", get.tally(df, "study", "belief"))
print.me("Concern group", get.tally(df, "study", "concern_group"))
sink(file = NULL)

# Correlation of mean ratings between studies

df = story_mean_ratings_study %>%
  pivot_wider(id_cols = c("ord","category","part"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean","n")) %>%
  mutate(scale = factor(part_to_scale[as.character(part)], levels = labels_scales))

sink(file = file.path(psubdir,"Correlations - mean ratings between studies.txt"), type = "output")
print.me("Shapiro-Wilk normality of ratings distribution - Study 1", check.normality(df,"scale","mean.Study 1"))
print.me("Shapiro-Wilk normality of ratings distribution - Study 2", check.normality(df,"scale","mean.Study 2"))
print.me("Shapiro-Wilk normality of ratings distribution - Study 3", check.normality(df,"scale","mean.Study 3"))
print.me("Correlations of mean ratings on each of the scales between Study 1 and Study 2", get.correlations(df,"scale","mean.Study 1","mean.Study 2"))
print.me("Correlations of mean ratings on each of the scales between Study 2 and Study 3", get.correlations(df,"scale","mean.Study 2","mean.Study 3"))
sink(file = NULL)

# Correlations of times between studies

df = mean_times %>%
  pivot_wider(id_cols = c("ord", "code", "category"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean_pres","mean_eval", "n"))

sink(file = file.path(psubdir,"Correlations - mean times between studies.txt"), type ="output")
print.me("Summary statistics (means)", get.means(mean_times, "study", "mean_pres", "mean_eval", "n"))
print.me("Shapiro-Wilk normality of presentation time distribution", check.normality(mean_times,"study","mean_pres"))
print.me("Shapiro-Wilk normality of evaluation time distribution", check.normality(mean_times,"study","mean_eval"))
print.me("Correlations of presentation times between Study 1 and Study 2", get.correlations(df,"category","mean_pres.Study 1","mean_pres.Study 2"))
print.me("Correlations of evaluation times between Study 1 and Study 2", get.correlations(df,"category","mean_eval.Study 1","mean_eval.Study 2"))
print.me("Correlations of presentation times between Study 2 and Study 3", get.correlations(df,"category","mean_pres.Study 2","mean_pres.Study 3"))
print.me("Correlations of evaluation times between Study 2 and Study 3", get.correlations(df,"category","mean_eval.Study 2","mean_eval.Study 3"))
sink(file = NULL)

# Comparison of mean ratings for each story type across studies

sink(file = file.path(psubdir,"Differences - mean ratings for each story type across studies.txt"), type = "output")

cat("\n \n MANOVA \n \n")

df = participant_mean_ratings %>%
  pivot_wider(id_cols = c("sid", "study", "category"),
              names_from = "scale",
              values_from = "mean") %>%
  select(!c("Valence", "Arousal"))

manova_result = manova(cbind(Anger, Anxiety, Compassion, Guilt, Hope) ~ category * study, data = df)
print(summary(manova_result))

cat("\n \n ANOVA \n \n")

df = participant_mean_ratings %>%
  filter(!(scale %in% c("Valence","Arousal"))) %>%
  group_by(scale)

cat("\n \n ANOVA: Study 1 vs Study 2 \n \n")

anova_results = df %>%
  filter(study %in% c("Study 1", "Study 2")) %>%
  anova_test(mean ~ category*study) %>%
  adjust_pvalue(method = 'bonferroni') %>% 
  add_significance(p.col = "p",
                   output.col = "sig.",
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  add_significance(p.col = "p.adj", 
                   output.col = "sig. adjusted", 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  p_format(digits = 2, leading.zero = FALSE)

anova_results %>% print.data.frame(row.names = F)

posthoc_results = df %>%
  filter(study %in% c("Study 1", "Study 2")) %>%
  tukey_hsd(mean ~ category*study) %>%
  p_format(digits = 2, leading.zero = FALSE)

cat("\n \n Posthocs: Study 1 vs Study 2 \n \n")

print.me("ANG", posthoc_results %>% filter(group1 == "ANG:Study 1" & group2 == "ANG:Study 2") %>% print.data.frame(row.names = F))
print.me("ANX", posthoc_results %>% filter(group1 == "ANX:Study 1" & group2 == "ANX:Study 2") %>% print.data.frame(row.names = F))
print.me("COM", posthoc_results %>% filter(group1 == "COM:Study 1" & group2 == "COM:Study 2") %>% print.data.frame(row.names = F))
print.me("GUI", posthoc_results %>% filter(group1 == "GUI:Study 1" & group2 == "GUI:Study 2") %>% print.data.frame(row.names = F))
print.me("HOP", posthoc_results %>% filter(group1 == "HOP:Study 1" & group2 == "HOP:Study 2") %>% print.data.frame(row.names = F))

cat("\n \n ANOVA: Study 2 vs Study 3 \n \n")

anova_results = df %>%
  filter(study %in% c("Study 2", "Study 3")) %>%  
  anova_test(mean ~ category*study) %>%
  adjust_pvalue(method = 'bonferroni') %>% 
  add_significance(p.col = "p",
                   output.col = "sig.",
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  add_significance(p.col = "p.adj", 
                   output.col = "sig. adjusted", 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>%
  p_format(digits = 2, leading.zero = FALSE)

anova_results %>% print.data.frame(row.names = F)

cat("\n \n Posthocs: Study 2 vs Study 3 \n \n")

posthoc_results = df %>%
  filter(study %in% c("Study 2", "Study 3")) %>%  
  tukey_hsd(mean ~ category*study) %>%
  p_format(digits = 2, leading.zero = FALSE)

print.me("ANG", posthoc_results %>% filter(group1 == "ANG:Study 2" & group2 == "ANG:Study 3") %>% print.data.frame(row.names = F))
print.me("ANX", posthoc_results %>% filter(group1 == "ANX:Study 2" & group2 == "ANX:Study 3") %>% print.data.frame(row.names = F))
print.me("COM", posthoc_results %>% filter(group1 == "COM:Study 2" & group2 == "COM:Study 3") %>% print.data.frame(row.names = F))
print.me("GUI", posthoc_results %>% filter(group1 == "GUI:Study 2" & group2 == "GUI:Study 3") %>% print.data.frame(row.names = F))
print.me("HOP", posthoc_results %>% filter(group1 == "HOP:Study 2" & group2 == "HOP:Study 3") %>% print.data.frame(row.names = F))

sink(file = NULL)

sink(file = file.path(psubdir,"Differences - summary emotion score across concern groups.txt"), type = "output")

# Difference in summary emotion score across concern groups

cat("\n \n Linear regression analysis \n \n")

df = full_join(participant_score, select(transposed_demo, "sid", "concern", "concern_group")) %>%
  filter(!is.na(concern))

norm_qqplot = function(x, title="Normal Q-Q Plot") {
  qqnorm(x, main=title)
  qqline(x)}

norm_qqplot(df$score, title="Normal Q-Q Plot of Score")

model = lm(score ~ concern_group, data = df)
print(summary(model))
sink(file = NULL)
