library(sf); library(dplyr); library(magrittr); library(here)
bg <- read_sf("./bgs.shp") %>%
  select(GEOID10, ALAND10) %>%
  rename(bgID = GEOID10, bgAREA = ALAND10)
bl <- read_sf("./blocks.shp") %>%
  select(GEOID10, ALAND10) %>%
  rename(blID = GEOID10, blAREA = ALAND10)
taz <- read_sf("./taz.shp") %>%
  select(GEOID10, ALAND10) %>%
  rename(tazID = GEOID10, tazAREA = ALAND10)

bt <- st_intersection(bl, taz)
bb <- st_intersection(bl, bg)

splitByTAZ <- bt %>%
  mutate(bgID = substr(blID, 1, 12)) %>%
  group_by(tazID) %>%
  do(data=(.)) %>%
  pull(data)

res <- data.frame()
for(i in 1:length(splitByTAZ)){
  data <- splitByTAZ[[i]]
  data %<>%
    st_set_geometry(NULL) %>%
    mutate(weight = blAREA / sum(blAREA)) %>%
    group_by(bgID) %>%
    summarize(bgWEIGHT = sum(weight),
              tazID = splitByTAZ[[i]]$tazID[1])
  res <- rbind(res, data)
}

# # as a test, take a subset of just one TAZ
# bt %<>% filter(tazID == "3401500024020") %>%
#   mutate(weight = blAREA / sum(blAREA))
# # block group contributions to this single TAZ
# weight <- bt %>%
#   mutate(bgID = substr(blID, 1, 12)) %>%
#   group_by(bgID) %>%
#   summarize(bgWEIGHT = sum(weight))