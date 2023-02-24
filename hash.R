

# library(readr)
# df <- read_csv("data/CIDP_NCS_0115_DurCorrected.csv")

library(dplyr)
temp = df %>%
  select(Name, ID) %>%
  distinct()

# cidp_26_210 
library(digest)

temp$hash = NA

for (i in 1:dim(temp)[1]){
  temp$hash[i] = digest(paste0(temp$Name[i], temp$ID[i]), algo = "murmur32")
  }

write_csv(temp, "data/hash.csv")

df %>%
  inner_join(temp, by = "ID") %>% 
  select(-c(Name.y)) %>%
  rename(Name = Name.x) %>%
  select(-ID) %>%
  rename(ID = hash) %>%
  relocate(ID, .after = Name) -> temp2

write_csv(temp2, "data/CIDP_demo_hashed_26_210.csv")
saveRDS(temp2, "data/cidp_demo.rds")

