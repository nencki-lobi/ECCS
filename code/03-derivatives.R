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

# Create extra variables for later use

current_year = 2022

transposed_demo = mutate(transposed_demo, age = current_year - as.numeric(year)) %>%
  relocate(age, .after = year) %>%
  filter(age>17)

# For each story calculate mean ratings on each of the scales

## Whole sample

story_mean_ratings = ratings %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  ungroup()

transposed_story_mean_ratings = story_mean_ratings %>%
  pivot_wider(id_cols = "ord",
              names_from = "part",
              names_sep = ".",
              names_sort = T,
              values_from = c("mean", "n"))

## Male sample

demo_M = filter(transposed_demo, transposed_demo$sex == "1")
ratings_M = filter(ratings, ratings$sid %in% demo_M$sid)

story_mean_ratings_M = ratings_M %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  ungroup()

## Female sample

demo_F = filter(transposed_demo, transposed_demo$sex == "0")
ratings_F = filter(ratings, ratings$sid %in% demo_F$sid)

story_mean_ratings_F = ratings_F %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  ungroup()

# For each story obtain demographic profile

subjects.ords = select(ratings, sid, code, stid, ord) %>% distinct()

story_sex_count = full_join(subjects.ords, transposed_demo, by = "sid") %>%
  group_by(ord, sex) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = "ord",
              names_from = sex,
              names_sep = ".",
              values_from = "n") %>%
  ungroup()

story_concern_count = full_join(subjects.ords, transposed_demo, by = "sid") %>%
  group_by(ord, concern) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = "ord",
              names_from = concern,
              names_sep = ".",
              values_from = "n") %>%
  ungroup()

# For each participant calculate mean ratings for each stimulus category and each rating scale

participant_mean_ratings =
  mutate(ratings, category = ord_to_category[as.character(ratings$ord)]) %>%
  group_by(sid, code, category, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  ungroup()