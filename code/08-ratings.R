# Generate output files with mean story ratings

## Study 1

ECCS.S1 = transposed_mean_ratings_study %>% 
  select("ord", "code", "category", contains("Study 1")) %>%
  rename(n = starts_with("n.Valence")) %>%
  select(-starts_with("n.")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  rename_all(~stringr::str_replace(.,"mean.","M.")) %>%
  rename_all(~stringr::str_replace(.,"sd.","SD.")) %>%
  rename_all(~stringr::str_replace(.,".Study 1",""))

write.table(ECCS.S1, file="./ECCS-ratings-S1.tsv", row.names = F, col.names = T, sep='\t', fileEncoding = "UTF-8")

## Study 2

ECCS.S2 = transposed_mean_ratings_study %>% 
  select("ord", "code", "category", contains("Study 2")) %>%
  rename(n = starts_with("n.Valence")) %>%
  select(-starts_with("n.")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  rename_all(~stringr::str_replace(.,"mean.","M.")) %>%
  rename_all(~stringr::str_replace(.,"sd.","SD.")) %>%
  rename_all(~stringr::str_replace(.,".Study 2",""))

write.table(ECCS.S2, file="./ECCS-ratings-S2.tsv", row.names = F, col.names = T, sep='\t', fileEncoding = "UTF-8")

## Study 3

ECCS.S3 = transposed_mean_ratings_study %>% 
  select("ord", "code", "category", contains("Study 3")) %>%
  rename(n = starts_with("n.Valence")) %>%
  select(-starts_with("n.")) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  rename_all(~stringr::str_replace(.,"mean.","M.")) %>%
  rename_all(~stringr::str_replace(.,"sd.","SD.")) %>%
  rename_all(~stringr::str_replace(.,".Study 3",""))

write.table(ECCS.S3, file="./ECCS-ratings-S3.tsv", row.names = F, col.names = T, sep='\t', fileEncoding = "UTF-8")
