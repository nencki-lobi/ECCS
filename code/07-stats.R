pdir = "./output"
if (!dir.exists(pdir)) {dir.create(pdir)}

psubdir = file.path(pdir, paste0(
  "studies-", paste(studies, collapse = "-"),
  "-required-", as.character(required)))
if (!dir.exists(psubdir)) {dir.create(psubdir)}

# Correlations - means between studies

# Define functions

print.me = function(title, result) {
  cat("\n", title, "\n", "\n")
  print(result)
}

check.normality = function(df, x, y) {
  df %>% select(gvar = {{x}}, val = {{y}}) %>% 
    group_by(gvar) %>% 
    nest(data = -gvar) %>% 
    mutate(
      shapiro = map(data, ~shapiro.test(.x$val)),
      glanced = map(shapiro, glance)
    ) %>% 
    unnest(glanced) %>% 
    select(variable = gvar, W = statistic, p.value) %>% 
    arrange(variable)
}

get.correlations = function(df, x, v1, v2) {
  df %>% select(gvar = {{x}}, val1 = {{v1}}, val2 = {{v2}}) %>% 
    group_by(gvar) %>%
    nest(data = -gvar) %>% 
    mutate(
      pearson = map(data, ~cor.test(.x$val1, .x$val2)),
      glanced = map(pearson, glance)
    ) %>% 
    unnest(glanced) %>% 
    select(variable = gvar, r = estimate, conf.low, conf.high, p.value) %>% 
    arrange(variable)
}


get.means = function(df, gval, v1, v2, v3) {
  df %>%
    select(gval = {{gval}}, val1 = {{v1}}, val2 = {{v2}}, val3 = {{v3}}) %>%
    group_by(gval) %>%
  summarise(mean_presentation = mean(val1), mean_evaluation = mean(val2), mean_n = mean(val3))
}

# Correlation of mean ratings between studies

df = story_mean_ratings_study %>%
  pivot_wider(id_cols = c("ord","category","part"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean","n"))

sink(file = file.path(psubdir,"Correlations - mean ratings.txt"), type ="output")
print.me("Shapiro-Wilk normality of ratings distribution - Study 1", check.normality(df,"part","mean.1"))
print.me("Shapiro-Wilk normality of ratings distribution - Study 2", check.normality(df,"part","mean.2"))
print.me("Correlations of mean ratings on each scale between studies", get.correlations(df,"part","mean.1","mean.2"))
sink(file = NULL)

##########

# Correlations - times

df = mean_times %>%
  pivot_wider(id_cols = c("ord", "code", "category"),
              names_from = "study",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean_pres","mean_eval", "n"))


sink(file = file.path(psubdir,"Correlations - mean times.txt"), type ="output")
print.me("Mean presentation and evaluation time + mean number of ratings per story", get.means(mean_times, "study", "mean_pres", "mean_eval", "n"))
print.me("Shapiro-Wilk normality of presentation time distribution", check.normality(mean_times,"study","mean_pres"))
print.me("Shapiro-Wilk normality of evaluation time distribution", check.normality(mean_times,"study","mean_eval"))
print.me("Correlations of presentation times between studies", get.correlations(df,"category","mean_pres.1","mean_pres.2"))
print.me("Correlations of evaluation times between studies", get.correlations(df,"category","mean_eval.1","mean_eval.2"))
sink(file = NULL)

