library(tidyverse)
library(sf)
library(leaflet)

library(htmltools)
library(htmlwidgets)
results2025 <- read_delim("data/EventResults.txt", delim = "\t", 
                          escape_double = FALSE, trim_ws = TRUE)

colnames(results2025) = c("ID", "Electoral District", "Riding_FR", "Type",
                          "Type_FR",
                          "Surname", "Middle Name", "Given Name", "Party",
                          "Party_FR", "Votes", "Vote Share", "rj", "ttlb")
results2025 = results2025 %>%
  filter(Type == "preliminary") %>%
  select(ID, `Electoral District`, Party, Votes, `Vote Share`)
lpc2025 = results2025 %>% 
  filter(Party == "Liberal") %>%
  mutate(lpc_candidate = TRUE)
cpc2025 = results2025 %>% 
  filter(Party == "Conservative") %>%
  mutate(cpc_candidate = TRUE)
ndp2025 = results2025 %>% 
  filter(Party == "NDP-New Democratic Party") %>%
  mutate(ndp_candidate = TRUE)
bq2025 = results2025 %>% 
  filter(Party == "Bloc Québécois") %>%
  mutate(bq_candidate = TRUE)
gpc2025 = results2025 %>% 
  filter(Party == "Green Party") %>%
  mutate(gpc_candidate = TRUE)

missing_lpc = data.frame(ID  = setdiff(results2025$ID, lpc2025$ID),
                         "Electoral District" = setdiff(
                           results2025$`Electoral District`, 
                           lpc2025$`Electoral District`),
                         Party = rep("Liberal",
                                     length(setdiff(results2025$ID,
                                                    lpc2025$ID))),
                         Votes = rep(0, length(setdiff(results2025$ID,
                                                       lpc2025$ID))),
                         "Vote Share" = rep(0.0, length(setdiff(results2025$ID,
                                                                lpc2025$ID))),
                         lpc_candidate = rep(FALSE, length(setdiff(results2025$ID,
                                                                   lpc2025$ID))))
colnames(missing_lpc) = c("ID", "Electoral District", "Party", "Votes", 
                          "Vote Share", "lpc_candidate")
lpc2025 = rbind(lpc2025, missing_lpc)

missing_cpc = data.frame(ID  = setdiff(results2025$ID, cpc2025$ID),
                         `Electoral District` = setdiff(
                           results2025$`Electoral District`, 
                           cpc2025$`Electoral District`),
                         Party = rep("Liberal",
                                     length(setdiff(results2025$ID,
                                                    cpc2025$ID))),
                         Votes = rep(0, length(setdiff(results2025$ID,
                                                       cpc2025$ID))),
                         `Vote Share` = rep(0.0, length(setdiff(results2025$ID,
                                                                cpc2025$ID))),
                         cpc_candidate = rep(FALSE, length(setdiff(results2025$ID,
                                                                   cpc2025$ID))))
colnames(missing_cpc) = c("ID", "Electoral District", "Party", "Votes", 
                          "Vote Share", "cpc_candidate")
cpc2025 = rbind(cpc2025, missing_cpc)

missing_ndp = data.frame(ID  = setdiff(results2025$ID, ndp2025$ID),
                         `Electoral District` = setdiff(
                           results2025$`Electoral District`, 
                           ndp2025$`Electoral District`),
                         Party = rep("Liberal",
                                     length(setdiff(results2025$ID,
                                                    ndp2025$ID))),
                         Votes = rep(0, length(setdiff(results2025$ID,
                                                       ndp2025$ID))),
                         `Vote Share` = rep(0.0, length(setdiff(results2025$ID,
                                                                ndp2025$ID))),
                         ndp_candidate = rep(FALSE, length(setdiff(results2025$ID,
                                                                   ndp2025$ID))))
colnames(missing_ndp) = c("ID", "Electoral District", "Party", "Votes", 
                          "Vote Share", "ndp_candidate")
ndp2025 = rbind(ndp2025, missing_ndp)
missing_bq = data.frame(ID  = setdiff(results2025$ID, bq2025$ID),
                        `Electoral District` = setdiff(
                          results2025$`Electoral District`, 
                          bq2025$`Electoral District`),
                        Party = rep("Liberal",
                                    length(setdiff(results2025$ID,
                                                   bq2025$ID))),
                        Votes = rep(0, length(setdiff(results2025$ID,
                                                      bq2025$ID))),
                        `Vote Share` = rep(0.0, length(setdiff(results2025$ID,
                                                               bq2025$ID))),
                        bq_candidate = rep(FALSE, length(setdiff(results2025$ID,
                                                                 bq2025$ID))))
colnames(missing_bq) = c("ID", "Electoral District", "Party", "Votes", 
                         "Vote Share", "bq_candidate")
bq2025 = rbind(bq2025, missing_bq)

missing_gpc = data.frame(ID  = setdiff(results2025$ID, gpc2025$ID),
                         `Electoral District` = setdiff(
                           results2025$`Electoral District`, 
                           gpc2025$`Electoral District`),
                         Party = rep("Liberal",
                                     length(setdiff(results2025$ID,
                                                    gpc2025$ID))),
                         Votes = rep(0, length(setdiff(results2025$ID,
                                                       gpc2025$ID))),
                         `Vote Share` = rep(0.0, length(setdiff(results2025$ID,
                                                                gpc2025$ID))),
                         gpc_candidate = rep(FALSE, length(setdiff(results2025$ID,
                                                                   gpc2025$ID))))
colnames(missing_gpc) = c("ID", "Electoral District", "Party", "Votes", 
                          "Vote Share", "gpc_candidate")
gpc2025 = rbind(gpc2025, missing_gpc)
lpc2025 = lpc2025 %>%
  select(ID, `Electoral District`, Votes, `Vote Share`, lpc_candidate)
colnames(lpc2025) = c("ID", "Electoral District", "LPC Votes", "LPC Vote Share",
                      "lpc_candidate")

cpc2025 = cpc2025 %>%
  select(`Electoral District`, Votes, `Vote Share`, cpc_candidate)
colnames(cpc2025) = c("Electoral District", "CPC Votes", "CPC Vote Share",
                      "cpc_candidate")
ndp2025 = ndp2025 %>%
  select(`Electoral District`, Votes, `Vote Share`, ndp_candidate)
colnames(ndp2025) = c("Electoral District", "NDP Votes", "NDP Vote Share",
                      "ndp_candidate")
bq2025 = bq2025 %>%
  select(`Electoral District`, Votes, `Vote Share`, bq_candidate)
colnames(bq2025) = c("Electoral District", "BQ Votes", "BQ Vote Share",
                     "bq_candidate")
gpc2025 = gpc2025 %>%
  select(`Electoral District`, Votes, `Vote Share`, gpc_candidate)
colnames(gpc2025) = c("Electoral District", "GPC Votes", "GPC Vote Share",
                      "gpc_candidate")

election2025 = left_join(lpc2025, cpc2025, by = "Electoral District")
election2025 = left_join(election2025, ndp2025, by = "Electoral District")
election2025 = left_join(election2025, bq2025, by = "Electoral District")
election2025 = left_join(election2025, gpc2025, by = "Electoral District")

winning_party = c()
winning_vote_share = c()
for (i in 1:343){
  w = max(unlist(c(election2025[i,4], election2025[i,7], election2025[i,10],
                   election2025[i,13], election2025[i,16])))
  v = max(unlist(c(election2025[i,3], election2025[i,6], election2025[i,9],
                   election2025[i,12], election2025[i,15])))
  winning_vote_share = c(winning_vote_share, w)
  if (v == unlist(election2025[i,3])) {
    winning_party = c(winning_party, "LPC")
  }
  if (v == unlist(election2025[i,6])) {
    winning_party = c(winning_party, "CPC")
  }
  if (v == unlist(election2025[i,9])) {
    winning_party = c(winning_party, "NDP")
  }
  if (v == unlist(election2025[i,12])) {
    winning_party = c(winning_party, "BQ")
  }
  if (v == unlist(election2025[i,15])) {
    winning_party = c(winning_party, "GPC")
  }
}
election2025$winning_party = winning_party
election2025$winning_vote_share = winning_vote_share

boundaries2023 = st_read("data/FED_CA_2023_EN-SHP/FED_CA_2023_EN.shp")
boundaries2023 = boundaries2023 %>%
  st_transform(crs = 4326,method = "NAD_1983_CSRS_To_WGS_1984_2")
colnames(boundaries2023) = c("ID", "Electoral District", "ED_F", "reporder",
                             "SHAPE_AREA", "SHAPE_LEN", "geometry")

election2025 = election2025[order(election2025$ID),]
parties = c( "#00F7FF", "blue", "#29D462", "red", "orange")
cd1 <- read_csv("data/98-401-X2021029_eng_CSV/98-401-X2021029_English_CSV_data.csv")
cd1 = cd1 %>%
  filter(GEO_LEVEL == "Federal electoral district (2023 Representation Order)")


total = cd1 %>% filter(CHARACTERISTIC_NAME == "Total - Age groups of the population - 100% data")
total = total %>%
  select(c(3,5, 12, 14, 16, 18, 20, 22))
twenty_twenty4 = cd1 %>% filter(CHARACTERISTIC_NAME == "20 to 24 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
twenty5_twenty9 = cd1 %>% filter(CHARACTERISTIC_NAME == "25 to 29 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
thirty_thirty4 = cd1 %>% filter(CHARACTERISTIC_NAME == "30 to 34 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
forty_forty4 = cd1 %>% filter(CHARACTERISTIC_NAME == "40 to 44 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
fifty_fifty4 = cd1 %>% filter(CHARACTERISTIC_NAME == "50 to 54 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
sixty_sixty4 = cd1 %>% filter(CHARACTERISTIC_NAME == "60 to 64 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
thirty5_thirty9 = cd1 %>% filter(CHARACTERISTIC_NAME == "35 to 39 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
forty5_forty9 = cd1 %>% filter(CHARACTERISTIC_NAME == "45 to 49 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
fifty5_fifty9 = cd1 %>% filter(CHARACTERISTIC_NAME == "55 to 59 years") %>%
  select(c(5, 12, 14, 16, 18, 20, 22))
sixty5_over = cd1 %>% filter(CHARACTERISTIC_ID == 24) %>%
  select(c(5, 12, 18))
colnames(sixty5_over) = c("GEO_NAME", "Total_65_above", "Rate_65_above")

men = data.frame(
  GEO_NAME = total$GEO_NAME,
  total_men = total$`C2_COUNT_MEN+`,
  rate_men = round(total$`C2_COUNT_MEN+` * 100 / total$C1_COUNT_TOTAL, 3)
)
women = data.frame(
  GEO_NAME = total$GEO_NAME,
  total_women = total$`C3_COUNT_WOMEN+`,
  rate_women = round(total$`C3_COUNT_WOMEN+` * 100 / total$C1_COUNT_TOTAL, 3)
)
t20_29 = data.frame(
  GEO_NAME = twenty_twenty4$GEO_NAME,
  Total_20_29 = twenty_twenty4$C1_COUNT_TOTAL + twenty5_twenty9$C1_COUNT_TOTAL,
  Rate_20_29 = round(((twenty_twenty4$C1_COUNT_TOTAL + 
                         twenty5_twenty9$C1_COUNT_TOTAL) / total$C1_COUNT_TOTAL)*100, 3)
)
t30_49 = data.frame(
  GEO_NAME = twenty_twenty4$GEO_NAME,
  Total_30_49 = thirty_thirty4$C1_COUNT_TOTAL + thirty5_thirty9$C1_COUNT_TOTAL +
    forty_forty4$C1_COUNT_TOTAL + forty5_forty9$C1_COUNT_TOTAL,
  Rate_30_49 = round((thirty_thirty4$C1_COUNT_TOTAL + thirty5_thirty9$C1_COUNT_TOTAL +
                        forty_forty4$C1_COUNT_TOTAL + forty5_forty9$C1_COUNT_TOTAL) /( total$C1_COUNT_TOTAL)*100, 3)
)
t50_65 = data.frame(
  GEO_NAME = twenty_twenty4$GEO_NAME,
  Total_50_65 = fifty5_fifty9$C1_COUNT_TOTAL + fifty_fifty4$C1_COUNT_TOTAL +
    sixty_sixty4$C1_COUNT_TOTAL,
  Rate_50_65 = round((fifty5_fifty9$C1_COUNT_TOTAL + fifty_fifty4$C1_COUNT_TOTAL +
                        sixty_sixty4$C1_COUNT_TOTAL) /( total$C1_COUNT_TOTAL)*100, 3)
)

age_groups = left_join(t20_29, t30_49, by = "GEO_NAME")
age_groups = left_join(age_groups, t50_65, by = "GEO_NAME")
age_groups = left_join(age_groups, sixty5_over, by = "GEO_NAME")

sex = left_join(men, women, by = "GEO_NAME")

household_size = cd1 %>% filter(CHARACTERISTIC_NAME == "Average household size") %>%
  select(5,12)
colnames(household_size) = c("GEO_NAME", "Average Household Size")

income = cd1 %>% filter(CHARACTERISTIC_NAME == "Median total income in 2020 among recipients ($)") %>%
  select(5,12)
colnames(income) = c("GEO_NAME", "Average Income")
first_generation = cd1 %>% 
  filter(CHARACTERISTIC_NAME == "First generation") %>%
  select(5,12,18)
colnames(first_generation) = c("GEO_NAME", "First Generation", "First Generation Rate")
second_generation = cd1 %>% 
  filter(CHARACTERISTIC_NAME == "Second generation") %>%
  select(5,12,18)
colnames(second_generation) = c("GEO_NAME", "Second Generation", "Second Generation Rate")
third_generation = cd1 %>% 
  filter(CHARACTERISTIC_NAME == "Third generation or more") %>%
  select(5,12,18)
colnames(third_generation) = c("GEO_NAME", "Third Generation", "Third Generation Rate")

generations = left_join(first_generation, second_generation, by = "GEO_NAME")
generations = left_join(generations, third_generation, by = "GEO_NAME")


visible_minority = cd1 %>%
  filter(CHARACTERISTIC_NAME == "Total visible minority population") %>%
  select(5,12,18)
colnames(visible_minority) = c("GEO_NAME", "Visible Minority Population", "Visible Minority Population Proportion")



post_secondary_education = cd1 %>%
  filter(CHARACTERISTIC_ID == 2001) %>%
  select(5,12,18)
colnames(post_secondary_education) = c("GEO_NAME", "Post Secondary Education", "Postsecondary certificate, diploma or degree Proportion")
features = left_join(age_groups, generations, by = "GEO_NAME")
features = left_join(features, household_size, by = "GEO_NAME")
features = left_join(features, sex, by = "GEO_NAME")
features = left_join(features, post_secondary_education, by = "GEO_NAME")
features = left_join(features, household_size, by = "GEO_NAME")
features = left_join(features, visible_minority, by = "GEO_NAME")
write.csv(features, "data/census_features_2025.csv")
features = read.csv("data/census_features_2025.csv")
features$X = NULL

index <- features$GEO_NAME %in% setdiff(features$GEO_NAME, election2025$`Electoral District`)
features$GEO_NAME[index] <- setdiff(election2025$`Electoral District`, features$GEO_NAME)[match(features$GEO_NAME[index], setdiff(features$GEO_NAME, election2025$`Electoral District`))]
features$`Electoral District` = features$GEO_NAME
features$GEO_NAME = NULL
df = left_join(features, election2025, by = "Electoral District")
df = left_join(df, boundaries2023, by = "Electoral District")
df = st_as_sf(df)
df = st_transform(df, crs = 3347)

colnames(df) = c("Total_20_29", "Proportion of population aged between 20 and 29", "Total_30_39", "Proportion of population aged between 30 and 49", 
                 "Total_50_65", "Proportion of population aged between 50 and 65", "Total_65_above", "Proportion of population aged 65 and above", "First Generation", "First Generation Proportion", "Second Generation", "Second Generation Proportion", "Third Generation and above", "Third Generation and above Proportion", "Average Household Size", "Total Men", "Total Men Proportion", "Total Women", "Total Women Proportion", "Post Secondary Education", "Post Secondary Education Proportion", "A", "Visible Minority", "Visible Minority Proportion", "Electoral District", "ID", "LPC Votes", "LPC Vote Share", "lpc_candidate","CPC Votes", "CPC Vote Share", "cpc_candidate", "NDP Votes", "NDP Vote Share", "ndp_candidate", "BQ Votes", "BQ Vote Share", "bq_candidate", "GPC Votes", "GPC Vote Share", "gpc_candidate", "winning_party", "winning_vote_share", "idy", "ED_F", "reporder", "SHAPE_AREA", "SHAPE_LEN", "geometry")