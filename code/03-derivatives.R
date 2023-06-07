# Keep stories information in one place

stories = items %>%
  mutate(ord = ords) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category")

# Change data format from long to wide

transposed_ratings = ratings %>%
  pivot_wider(id_cols = c("sid", "code", "stid"),
              names_from = c("name", "ord", "part"),
              names_sep = ".",
              names_sort = T,
              values_from = "opt")

lookup = c(sex = "demo.0", 
           sex_other = "demo.1",
           birth = "demo.2",
           res = "demo.3",
           edu = "demo.4",
           edu_other = "demo.5",
           child = "demo.6",
           work = "demo.7",
           org = "demo.8",
           belief = "demo.9",
           concern = "demo.10")

transposed_demo = demo %>%
  pivot_wider(id_cols = c("sid", "code", "stid"),
              names_from = c("name", "ord"),
              names_sep = ".",
              values_from = "val") %>%
  rename(any_of(lookup)) %>%
  select(-contains("_other")) %>%
  mutate(study = factor(recode(stid, "13"="Study 1", "14"="Study 1", "15"="Study 2", "17"="Study 3"),
                        levels = c("Study 1", "Study 2", "Study 3")), .after = stid) %>%
  mutate(across(c(birth), as.numeric)) %>%
  mutate(across(c(where(is.character), -sid, -code, -stid), as.factor)) %>%
  filter(sid %in% transposed_ratings$sid)

# Create extra variables for later use

transposed_demo = transposed_demo %>%
  mutate(age = case_when(stid %in% c("13","14","15") ~ 2022 - birth,
                         stid %in% c("17") ~ 2023 - birth), .after = birth) %>%
  mutate(res_group = case_when(
    stid %in% c("13","14","15") ~ recode(res, 
        "0"="Rural",
        "1"="Urban <50K",
        "2"="Urban 50-100K",
        "3"="Urban >100K",
        "4"="Urban >100K"),
    stid %in% c("17") ~ recode(res, 
        "0"="Rural",
        "1"="Urban <50K",
        "2"="Urban <50K",
        "3"="Urban 50-100K",
        "4"="Urban >100K")), .after = res) %>%
  mutate(edu_group = case_when(
    stid %in% c("13","14","15") ~ recode(edu, 
        "0"="Primary",
        "1"="Primary",
        "2"="Primary",
        "3"="Secondary",
        "4"="Secondary",
        "5"="Higher",
        "6"="Higher",
        "7"="Higher",
        "8"="Other"),
    stid %in% c("17") ~ recode(edu, 
        "0"="Primary",
        "1"="Secondary",
        "2"="Secondary",
        "3"="Higher",
        "4"="Other")), .after = edu) %>%
  mutate(concern_group = factor(recode(concern, "1"="Low", "2"="Low", "3"="Low", "4"="Medium", "5"="High"),
                                levels = c("Low", "Medium", "High")), .after = concern)

# For each story calculate mean ratings on each of the scales

## Whole sample

story_mean_ratings = ratings %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category", "part") %>%
  ungroup()

transposed_story_mean_ratings = story_mean_ratings %>%
  pivot_wider(id_cols = c("ord", "code", "category"),
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
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category", "part") %>%
  ungroup()

## Female sample

demo_F = filter(transposed_demo, transposed_demo$sex == "0")
ratings_F = filter(ratings, ratings$sid %in% demo_F$sid)

story_mean_ratings_F = ratings_F %>%
  group_by(ord, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category", "part") %>%
  ungroup()

## Different studies

story_mean_ratings_study = ratings %>%
  mutate(study = factor(recode(stid, "13"="Study 1", "14"="Study 1", "15"="Study 2", "17"="Study 3"),
                        levels = c("Study 1", "Study 2", "Study 3")), .after = stid) %>%
  group_by(ord, part, study) %>%
  summarise(mean = mean(opt), sd = sd(opt), n = n()) %>%
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category", "part") %>%
  ungroup()

transposed_mean_ratings_study = story_mean_ratings_study %>%
  mutate(scale = factor(part_to_scale[as.character(part)], levels = labels_scales)) %>%
  pivot_wider(id_cols = c("ord", "code", "category"),
              names_from = c("scale", "study"),
              names_sep = ".",
              names_sort = T,
              values_from = c("mean", "sd", "n"))

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

participant_mean_ratings = ratings %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  group_by(code, sid, stid, category, part) %>%
  summarise(mean = mean(opt), n = n()) %>%
  mutate(scale = factor(part_to_scale[as.character(part)], levels = labels_scales)) %>%
  mutate(study = factor(recode(stid, "13"="Study 1", "14"="Study 1", "15"="Study 2", "17"="Study 3"),
                        levels = c("Study 1", "Study 2", "Study 3")), .after = stid) %>%
  relocate("code", "sid", "stid", "study", "category", "part") %>%
  ungroup()

# For each participant calculate summary score (based on all rating scales taken together)

labels_emotion_scales = labels_scales[!(labels_scales %in% c('Valence','Arousal'))]

participant_score = participant_mean_ratings %>%
  select("sid", "code", "study", "category", "part", "mean") %>%
  mutate(scale = factor(part_to_scale[as.character(part)], levels = labels_scales)) %>%
  pivot_wider(id_cols = c("sid", "code", "study", "category"),
              names_from = c("scale"), values_from = "mean") %>%
  group_by(sid, code, study, category) %>%
  summarise_at(labels_scales, mean, na.rm = TRUE) %>% 
  mutate(Valence = abs(Valence-50)) %>%
  mutate(score = Valence * Arousal)
#mutate(score = rowSums(across(labels_scales)))
#mutate(score = rowSums(across(labels_emotion_scales) ** 2))

participant_score[,"score"] = rescale(participant_score$score, to=c(0,1))

# Mean presentation and evaluation times

## Remove outliers

remove_outliers = function(df, variable) {
  Q1 = quantile(df[[variable]], .25, na.rm = TRUE)
  Q3 = quantile(df[[variable]], .75, na.rm = TRUE)
  IQR = IQR(df[[variable]], na.rm = TRUE)
  subset(df, df[[variable]] > (Q1 - 1.5*IQR) & df[[variable]] < (Q3 + 1.5*IQR))
}

times_cleaned = intersect(remove_outliers(times, "pres_time"), 
                          remove_outliers(times, "eval_time"))
times_to_say_goodbye = setdiff(remove_outliers(times, "pres_time"), 
                               remove_outliers(times, "eval_time"))

mean_times = times_cleaned %>%
  mutate(study = factor(recode(stid, "13"="Study 1", "14"="Study 1", "15"="Study 2", "17"="Study 3"),
                        levels = c("Study 1", "Study 2", "Study 3")), .after = stid) %>%
  group_by(study, ord) %>%
  summarise(mean_pres = mean(pres_time), mean_eval = mean(eval_time), n = n()) %>%
  mutate(code = ord_to_code[as.character(ord)]) %>%
  mutate(category = factor(ord_to_category[as.character(ord)], levels = labels_categories)) %>%
  relocate("ord", "code", "category", "study", "mean_pres", "mean_eval", "n") %>%
  ungroup()
