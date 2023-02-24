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
print.me("Polish stories - difference in story length across categories (ANOVA)", anova_test(df, len_PL ~ category))
print.me("Polish stories - post-hoc comparisons", tukey_hsd(df, len_PL ~ category))
print.me("English stories - summary statistics", summary(df$len_EN))
print.me("English stories - difference in story length across categories", anova_test(df, len_EN ~ category))
print.me("English stories - post-hoc comparisons", tukey_hsd(df, len_EN ~ category))
print.me("Norwegian stories - summary statistics", summary(df$len_NO))
print.me("Norwegian stories - difference in story length across categories (ANOVA)", anova_test(df, len_NO ~ category))
print.me("Norwegian stories - post-hoc comparisons", tukey_hsd(df, len_NO ~ category))
sink(file = NULL)


# Summary statistics of demographic data

df = transposed_demo %>%
  mutate(study = recode(stid, "13"="1", "14"="1", "15"="2")) %>%
  select(study, sex, age, res, edu, child, work, org, belief, concern) %>%
  mutate(across(where(is.character), as.factor))

sink(file = file.path(psubdir, "Demographics - summary statistics.txt"), type ="output")

cat("\n", "Descriptive statistics by study:", "\n", "\n")

ldf = describe(df ~ study)
do.call("rbind", ldf) %>% 
  print.data.frame()

print.me("Sex", get.tally(df, "study", "sex"))
print.me("Residence", get.tally(df, "study", "res"))
print.me("Education", get.tally(df, "study", "edu"))
print.me("Child", get.tally(df, "study", "child"))
print.me("Work", get.tally(df, "study", "work"))
print.me("Organization", get.tally(df, "study", "org"))
print.me("Belief", get.tally(df, "study", "belief"))
print.me("Concern", get.tally(df, "study", "concern"))
sink(file = NULL)

# Difference in mean ratings across concern groups

df = full_join(transposed_demo,participant_mean_ratings) %>%
  mutate(concern_group = recode(concern, "1"="1", "2"="1", "3"="1", "4"="2", "5"="3")) %>%
  mutate(across(c("part", "concern_group"), as.character)) %>%
  relocate(concern_group, .after = concern)

sink(file = file.path(psubdir,"Differences - mean ratings across concern groups.txt"), type ="output")
print.me("Difference in mean ratings across concern groups",
         anova_test(data = df, dv = mean, wid = sid,
                    between = concern_group,
                    within = c(category, part)))
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
print.me("Shapiro-Wilk normality of ratings distribution - Study 1", check.normality(df,"scale","mean.1"))
print.me("Shapiro-Wilk normality of ratings distribution - Study 2", check.normality(df,"scale","mean.2"))
print.me("Correlations of mean ratings on each of the scales between studies", get.correlations(df,"scale","mean.1","mean.2"))
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
print.me("Correlations of presentation times between studies", get.correlations(df,"category","mean_pres.1","mean_pres.2"))
print.me("Correlations of evaluation times between studies", get.correlations(df,"category","mean_eval.1","mean_eval.2"))
sink(file = NULL)
