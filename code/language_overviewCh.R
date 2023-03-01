## ----setup, include=FALSE-----
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, dpi = 300)


if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load(
  dotwhisker, interplot, maptools, gridExtra, kableExtra, # Visualization
  survey, ordinal, DAMisc, # Applied
  knitr, # dependency
  readxl, haven, descr, stringr, arm, broom, tidyverse
) # data wrangling # data wrangling

# Functions preload
set.seed(114)


## ----dataMan, cache = TRUE----
# 2010 ####
cgss2010 <- readRDS("/Users/zhumeng/Desktop/Fall semester_2021/language_english/data/cgss2010.rds")
cgss2010$id <- as.factor(cgss2010$id)
cgss2010[cgss2010 < 0] <- NA
cgss2010$year <- 2010
cgss2010$wt <- cgss2010$WEIGHT


# area
cgss2010$prov <- as.character(cgss2010$s41) %>%
  recode("110000" = "北京市", "120000" = "天津市", "130000" = "河北省", "140000" = "山西省", "150000" = "内蒙古自治区", "210000" = "辽宁省", "220000" = "吉林省", "230000" = "黑龙江省", "310000" = "上海市", "320000" = "江苏省", "330000" = "浙江省", "340000" = "安徽省", "350000" = "福建省", "360000" = "江西省", "370000" = "山东省", "410000" = "河南省", "420000" = "湖北省", "430000" = "湖南省", "440000" = "广东省", "450000" = "广西壮族自治区", "460000" = "海南省", "5e+05" = "重庆市", "510000" = "四川省", "520000" = "贵州省", "530000" = "云南省", "540000" = "西藏自治区", "610000" = "陕西省", "620000" = "甘肃省", "630000" = "青海省", "640000" = "宁夏回族自治区", "650000" = "新疆维吾尔自治区")


cgss2010$county <- cgss2010$s42
cgss2010$town <- cgss2010$s43
cgss2010$block <- cgss2010$s44


cgss2010$regionLan <- case_when(
  cgss2010$prov %in% c("北京市", "天津市", "河北省", "辽宁省", "吉林省", "黑龙江省", "山东省", "陕西省", "甘肃省", "宁夏回族自治区", "河南省", "湖北省", "重庆市", "四川省", "江苏省", "安徽省", "贵州省", "云南省") ~ "mandarin",
  cgss2010$prov %in% c("山西省") ~ "jin",
  cgss2010$prov %in% c("上海市", "浙江省") ~ "wu",
  cgss2010$prov %in% c("福建省", "海南省") ~ "min",
  cgss2010$prov %in% c("湖南省") ~ "xiang",
  cgss2010$prov %in% c("江西省") ~ "gan",
  cgss2010$prov %in% c("广东省") ~ "canHakka",
  cgss2010$prov %in% c("广西壮族自治区") ~ "ping",
  cgss2010$prov %in% c("内蒙古自治区", "新疆维吾尔自治区", "西藏自治区", "青海省") ~ "minority",
  TRUE ~ as.character(cgss2010$prov)
) %>%
  as.factor() %>%
  relevel(ref = "mandarin")

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2010", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp = ...10) %>%
  mutate(gdpp = as.numeric(gdpp)) %>%
  select(prov, gdpp)
cgss2010 <- left_join(cgss2010, prov)

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2005", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp5y = ...10) %>%
  mutate(gdpp5y = as.numeric(gdpp5y)) %>%
  select(prov, gdpp5y)
cgss2010 <- left_join(cgss2010, prov)

prov <- read_excel("D:/Data/China/economy/provPopulation.xlsx", sheet = "2010", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, popu = ...2) %>%
  mutate(popu = as.numeric(popu)) %>%
  select(prov, popu) %>%
  left_join(read_excel("D:/Data/China/economy/provSize.xlsx")) %>%
  mutate(density = popu * 10000 / size, densLog = log(density)) %>%
  select(prov, popu, density, densLog)
cgss2010 <- left_join(cgss2010, prov)

prov <- read_excel("D:/Data/China/economy/provTravel.xlsx", sheet = "2010", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, tourist = ...2) %>%
  mutate(tourist = as.numeric(tourist)) %>%
  select(prov, tourist)
cgss2010 <- left_join(cgss2010, prov)

prov <- read_excel("D:/Data/China/economy/provEdu.xlsx", sheet = "2010", skip = 4, col_names = FALSE) %>%
  mutate(
    prov = ...1, teacher = ...11, teachLog = log(teacher), pupil = ...5,
    teacherR = teacher / pupil, teacherRLog = log(teacherR)
  ) %>%
  select(prov, teacher, teachLog, teacherR, teacherRLog)
cgss2010 <- left_join(cgss2010, prov)

rm(prov)

cgss2010$prov <- as.factor(cgss2010$prov) %>%
  relevel(ref = "北京市")

# personal characteristics
cgss2010$female <- cgss2010$a2 - 1
cgss2010$age <- 2010 - cgss2010$a3a
cgss2010$generation <- case_when(
  cgss2010$a3a < 1950 ~ "Founding Time",
  cgss2010$a3a >= 1950 & cgss2010$a3a < 1966 ~ "Before Cultural Revolution",
  cgss2010$a3a >= 1966 & cgss2010$a3a < 1978 ~ "Cultural Revolution",
  cgss2010$a3a >= 1978 ~ "Reform and Opening-Up",
  TRUE ~ as.character(cgss2010$a3a)
) %>%
  factor(levels = c(
    "Founding Time",
    "Before Cultural Revolution",
    "Cultural Revolution",
    "Reform and Opening-Up"
  ))

cgss2010$age3 <- case_when(
  cgss2010$a3a < 1956 ~ "Before Standardization",
  cgss2010$a3a >= 1956 & cgss2010$a3a < 1980 ~ "Before 9yr Education",
  cgss2010$a3a >= 1980 ~ "After 9yr Education",
  TRUE ~ as.character(cgss2010$a3a)
) %>%
  factor(levels = c(
    "Before Standardization",
    "Before 9yr Education",
    "After 9yr Education"
  ))


cgss2010$race <- factor(cgss2010$a4, labels = c("han", "mongolian", "manchu", "hui", "tibetan", "zhuang", "uygur", "others"))
cgss2010$minority <- ifelse(as.numeric(cgss2010$race) != 1, 1, 0)
cgss2010$minor_hhm <- ifelse(as.numeric(cgss2010$race) %in% c(1, 3, 4), 0, 1)

cgss2010$edu <- Recode(cgss2010$a7a, "6:8 = 5; 9:10 = 6; 11:12 = 7; 13 = 8; 14 = NA") - 1
cgss2010$edu9yr <- case_when(
  cgss2010$edu <= 2 ~ 0, # 0 for not taking elementary
  cgss2010$edu > 3 & 1958 - cgss2010$a3a < 16 ~ 1, # middle school and smaller than 15 in 1959
  cgss2010$edu > 2 & 1958 - cgss2010$a3a < 13 ~ 1, # elementary school and smaller than 13 in 1959
  is.na(cgss2010$edu) ~ 0
)


cgss2010$incomePer <- cgss2010$a8a
cgss2010$incomePer[cgss2010$incomePer >= 9999990] <- NA # some suspicious data having 9999992, which should not be that "accurate." Thus all coding to missing
cgss2010$incomeFam <- cgss2010$a62
cgss2010$incomeFam[cgss2010$incomeFam >= 9999990] <- NA

cgss2010$party <- if_else(cgss2010$a10 %in% c(1, 3), 1, 0) # including party and cyl
cgss2010$health <- cgss2010$a15

cgss2010$rural <- cgss2010$s5 - 1
cgss2010$hukouRural <- cgss2010$a18
cgss2010$hukouRural[cgss2010$hukouRural %in% c(2:5)] <- 0
cgss2010$hukouRural[cgss2010$hukouRural %in% c(6, 7)] <- NA # no hukou or others


cgss2010$migrant <- if_else(cgss2010$a21 < 3, 0, 1)

cgss2010$marriage <- if_else(cgss2010$a69 == 3, 1, 0)

# life style
media_ratio <- function() {
  total <- rowSums(select(cgss2010, a281:a286), na.rm = TRUE) # calculate the maximum value of using the traditional media
  tvRadio <- rowSums(select(cgss2010, a283:a284), na.rm = TRUE) ## calculate the maximum value of using the new media
  ratio <- tvRadio / total # to what extent one use new media more than traditional
  return(ratio)
}
cgss2010$mediaR <- media_ratio()

cgss2010$tvRadio <- cgss2010$a283 + cgss2010$a284
cgss2010$internet <- cgss2010$a285 + cgss2010$a286
cgss2010$paperMedia <- cgss2010$a281 + cgss2010$a282


# Language
cgss2010$mandarinListen <- cgss2010$a49
cgss2010$mandarinSpeak <- cgss2010$a50
cgss2010$mandarin <- select(cgss2010, mandarinSpeak, mandarinListen) %>%
  rowMeans(na.rm = TRUE)

cgss2010$englishListen <- cgss2010$a51
cgss2010$englishSpeak <- cgss2010$a52
cgss2010$english <- select(cgss2010, englishSpeak, englishListen) %>%
  rowMeans(na.rm = TRUE)


temp <- group_by(cgss2010, block) %>%
  summarise(
    lanSphere = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphere_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphere_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVar = var(mandarin, na.rm = TRUE),
    lanSphereVar_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVar_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2010 <- left_join(cgss2010, temp)

temp <- group_by(cgss2010, county) %>%
  summarise(
    lanSphereC = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphereC_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphereC_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVarC = var(mandarin, na.rm = TRUE),
    lanSphereVarC_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVarC_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2010 <- left_join(cgss2010, temp) %>%
  mutate(
    lanDiff = mandarin - lanSphere, lanDiffC = mandarin - lanSphereC,
    lanRatio = mandarin / lanSphere, lanRatioC = mandarin / lanSphereC,
    lanDiff_s = mandarinSpeak - lanSphere_s, lanDiffC_s = mandarinSpeak - lanSphereC_s,
    lanRatio_s = mandarinSpeak / lanSphere_s, lanRatioC_s = mandarinSpeak / lanSphereC_s
  )

# Occupation
cgss2010$job <- cgss2010$a59j
cgss2010$job[cgss2010$a58 > 3] <- 8 # 8 = unemployed
cgss2010$job[cgss2010$a58 %in% c(2, 3)] <- 9 # agriculture
cgss2010$job[cgss2010$job == 2 & cgss2010$a59k == 1] <- 10 # SOE
cgss2010$job[cgss2010$job == 2 & cgss2010$a59k %in% c(2, 3)] <- 11 # group or private
cgss2010$job[cgss2010$job == 2 & cgss2010$a59k %in% c(4, 5)] <- 12 # foreign capital
cgss2010$job <- recode(as.numeric(cgss2010$job), `1` = 1, `6` = 2, `10` = 3, `11` = 4, `12` = 5, `2` = 6, `3` = 7, `4` = 8, `5` = 9, `7` = 10, `8` = 12, `9` = 11)
# reorder the categories of occupation
cgss2010$job <- factor(cgss2010$job, labels = c("Government", "Military", "Company_SOE", "Company_Private", "Company_Foreign", "Company_Others", "Public", "Organization", "Self_Employed", "Others", "Agriculture", "Unemployed"), exclude = c(NA, NaN)) %>%
  relevel(ref = "Unemployed") # Setting the modal group as the reference group

cgss2010$job6 <- Recode(as.numeric(cgss2010$job), "1 = 'Unemployed'; 2:3 = 'Governmental'; 8 = 'Governmental'; 4 = 'SOE'; 5:7 = 'Private'; 10 = 'Private'; 9 = 'Others'; 11 = 'Others'; 12 = 'Agriculture'") %>%
  as.factor() %>%
  relevel(ref = "Agriculture") # reference is governmental

cgss2010$job3 <- Recode(as.numeric(cgss2010$job6), "c(1,3,4,5) = 2; 2 = 1; 6 = 0") %>%
  factor(labels = c("Unemployed", "Agriculture", "Nonarg"))
cgss2010$job2 <- Recode(as.numeric(cgss2010$job6), "c(1,3,4,5) = 1; c(2,6) = 0") %>%
  factor(labels = c("Agriculture", "Nonarg"))

# Social attitudes
cgss2010$socialFair <- cgss2010$a35
cgss2010$socialFair_f <- as.factor(cgss2010$socialFair)
cgss2010$socialHappy <- cgss2010$a36
cgss2010$socialHappy_f <- as.factor(cgss2010$socialHappy)
cgss2010$socialTolerance <- select(cgss2010, a38:a40) %>%
  rowMeans(na.rm = TRUE) # average of sex prior to or outside marriage and homosexuality
cgss2010$socialLevel <- cgss2010$a43a
cgss2010$trustInter <- cgss2010$a33
cgss2010$trustInter_f <- as.factor(cgss2010$trustInter)

## Efficacy
cgss2010$complicated <- as.factor(cgss2010$d1001)
cgss2010$noConfidence <- as.factor(cgss2010$d1009)
cgss2010$ableJoinPolitics <- as.factor(cgss2010$d1002)
cgss2010$beOfficial <- as.factor(cgss2010$d1003)

cgss2010$noInfluence <- as.factor(cgss2010$d1004)
cgss2010$govCare <- as.factor(cgss2010$d1005)
cgss2010$officialAdopt <- as.factor(cgss2010$d1006)
cgss2010$officialAttention <- as.factor(cgss2010$d1008)
cgss2010$officialAware <- as.factor(cgss2010$d1011)

cgss2010$efficacyIn <-
  select(cgss2010, d1001, d1002, d1003, d1009) %>%
  mutate(d1001 = 6 - d1001, d1009 = 6 - d1009) %>%
  rowMeans(na.rm = TRUE)
cgss2010$efficacyEx <-
  select(cgss2010, d1004, d1005, d1006, d1008, d1011) %>%
  mutate(d1004 = 6 - d1004, d1005 = 6 - d1005) %>%
  rowMeans(na.rm = TRUE)

# ## estimate working satisfaction with IRT
# library(mirt)
#
# cgss2010$id_irt <- seq(nrow(cgss2010))
#
# df_irt <- select(cgss2010, id_irt, d1001, d1002, d1003, d1009, d1004, d1005, d1006, d1008, d1011) %>%
#   mutate(d1001 = 6 - d1001, d1009 = 6 - d1009, d1004 = 6 - d1004, d1005 = 6 - d1005) %>%
#   filter_at(vars(d1001, d1002, d1003, d1009, d1004, d1005, d1006, d1008, d1011), any_vars(!is.na(.))) # removed the rows with completely missing data of the indictors
#
# score_irt <- select(df_irt, -id_irt) %>%
#   mirt(2, itemtype = "gpcm", verbose = FALSE)
#
# irt_goodness <- M2(score_irt, type = "C2", na.rm = TRUE) # goodness of fit, C2 for few dof
# # summary(score_irt) # factor loading
#
# df_irt$efficacyIn2 <- -fscores(score_irt)[, "F2"] # using negative b/c the items negatively correlate to the scores; verified with the mean measurements (effficacyIn & efficacyEx)
# df_irt$efficacyEx2 <- -fscores(score_irt)[, "F1"]
#
# cgss2010 <- select(df_irt, id_irt, efficacyIn2, efficacyEx2) %>%
#   right_join(cgss2010)

# 2012 ####
cgss2012 <- read_dta("D:/Data/China/surveys/CGSS/Data/cgss2012_14.dta")
cgss2012$id <- as.factor(cgss2012$id)
cgss2012[cgss2012 < 0] <- NA
cgss2012$year <- 2012
cgss2012$wt <- cgss2012$weight
cgss2012$ab <- is.na(cgss2012$m18a) # use a question only in A version to identify A/B. Crosschecked with q1 (a question only in B).

# area
cgss2012$prov <- as.character(cgss2012$s41)
cgss2012$prov <- recode(cgss2012$prov, "110000" = "北京市", "120000" = "天津市", "130000" = "河北省", "140000" = "山西省", "150000" = "内蒙古自治区", "210000" = "辽宁省", "220000" = "吉林省", "230000" = "黑龙江省", "310000" = "上海市", "320000" = "江苏省", "330000" = "浙江省", "340000" = "安徽省", "350000" = "福建省", "360000" = "江西省", "370000" = "山东省", "410000" = "河南省", "420000" = "湖北省", "430000" = "湖南省", "440000" = "广东省", "450000" = "广西壮族自治区", "5e+05" = "重庆市", "510000" = "四川省", "520000" = "贵州省", "530000" = "云南省", "610000" = "陕西省", "620000" = "甘肃省", "630000" = "青海省", "640000" = "宁夏回族自治区", "650000" = "新疆维吾尔自治区")

cgss2012$regionLan <- case_when(
  cgss2012$prov %in% c("北京市", "天津市", "河北省", "辽宁省", "吉林省", "黑龙江省", "山东省", "陕西省", "甘肃省", "宁夏回族自治区", "河南省", "湖北省", "重庆市", "四川省", "江苏省", "安徽省", "贵州省", "云南省") ~ "mandarin",
  cgss2012$prov %in% c("山西省") ~ "jin",
  cgss2012$prov %in% c("上海市", "浙江省") ~ "wu",
  cgss2012$prov %in% c("福建省", "海南省") ~ "min",
  cgss2012$prov %in% c("湖南省") ~ "xiang",
  cgss2012$prov %in% c("江西省") ~ "gan",
  cgss2012$prov %in% c("广东省") ~ "canHakka",
  cgss2012$prov %in% c("广西壮族自治区") ~ "ping",
  cgss2012$prov %in% c("内蒙古自治区", "新疆维吾尔自治区", "西藏自治区", "青海省") ~ "minority",
  TRUE ~ as.character(cgss2012$prov)
) %>%
  as.factor() %>%
  relevel(ref = "mandarin")

cgss2012$county <- cgss2012$s42
cgss2012$town <- cgss2012$s43
cgss2012$block <- cgss2012$s44

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2012", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp = ...10) %>%
  mutate(gdpp = as.numeric(gdpp)) %>%
  select(prov, gdpp)
cgss2012 <- left_join(cgss2012, prov)

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2007", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp5y = ...10) %>%
  mutate(gdpp5y = as.numeric(gdpp5y)) %>%
  select(prov, gdpp5y)
cgss2012 <- left_join(cgss2012, prov)


prov <- read_excel("D:/Data/China/economy/provPopulation.xlsx", sheet = "2012", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, popu = ...2) %>%
  mutate(popu = as.numeric(popu)) %>%
  select(prov, popu) %>%
  left_join(read_excel("D:/Data/China/economy/provSize.xlsx")) %>%
  mutate(density = popu * 10000 / size, densLog = log(density)) %>%
  select(prov, popu, density, densLog)
cgss2012 <- left_join(cgss2012, prov)

prov <- read_excel("D:/Data/China/economy/provTravel.xlsx", sheet = "2012", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, tourist = ...2) %>%
  mutate(tourist = as.numeric(tourist)) %>%
  select(prov, tourist)
cgss2012 <- left_join(cgss2012, prov)

prov <- read_excel("D:/Data/China/economy/provEdu.xlsx", sheet = "2012", skip = 4, col_names = FALSE) %>%
  mutate(
    prov = ...1, teacher = ...11, teachLog = log(teacher), pupil = ...5,
    teacherR = teacher / pupil, teacherRLog = log(teacherR)
  ) %>%
  select(prov, teacher, teachLog, teacherR, teacherRLog)
cgss2012 <- left_join(cgss2012, prov)

rm(prov)

cgss2012$prov <- as.factor(cgss2012$prov) %>%
  relevel(ref = "北京市")

# personal characteristics
cgss2012$female <- cgss2012$a2 - 1
cgss2012$age <- 2012 - cgss2012$a3a
cgss2012$generation <- case_when(
  cgss2012$a3a < 1950 ~ "Founding Time",
  cgss2012$a3a >= 1950 & cgss2012$a3a < 1966 ~ "Before Cultural Revolution",
  cgss2012$a3a >= 1966 & cgss2012$a3a < 1978 ~ "Cultural Revolution",
  cgss2012$a3a >= 1978 ~ "Reform and Opening-Up",
  TRUE ~ as.character(cgss2012$a3a)
) %>%
  factor(levels = c(
    "Founding Time",
    "Before Cultural Revolution",
    "Cultural Revolution",
    "Reform and Opening-Up"
  ))
cgss2012$age3 <- case_when(
  cgss2012$a3a < 1956 ~ "Before Standardization",
  cgss2012$a3a >= 1956 & cgss2012$a3a < 1980 ~ "Before 9yr Education",
  cgss2012$a3a >= 1980 ~ "After 9yr Education",
  TRUE ~ as.character(cgss2012$a3a)
) %>%
  factor(levels = c(
    "Before Standardization",
    "Before 9yr Education",
    "After 9yr Education"
  ))

cgss2012$race <- factor(cgss2012$a4, labels = c("han", "mongolian", "manchu", "hui", "tibetan", "zhuang", "uygur", "others"))
cgss2012$minority <- ifelse(as.numeric(cgss2012$race) != 1, 1, 0)
cgss2012$minor_hhm <- ifelse(as.numeric(cgss2012$race) %in% c(1, 3, 4), 0, 1)

cgss2012$edu <- Recode(cgss2012$a7a, "6:8 = 5; 9:10 = 6; 11:12 = 7; 13 = 8; 14 = NA") - 1
cgss2012$edu9yr <- case_when(
  cgss2012$edu <= 2 ~ 0, # 0 for not taking elementary
  cgss2012$edu > 3 & 1958 - cgss2012$a3a < 16 ~ 1, # middle school and smaller than 15 in 1959
  cgss2012$edu > 2 & 1958 - cgss2012$a3a < 13 ~ 1, # elementary school and smaller than 13 in 1959
  is.na(cgss2012$edu) ~ 0
)

cgss2012$incomePer <- cgss2012$a8a
cgss2012$incomePer[cgss2012$incomePer >= 9999990] <- NA
cgss2012$incomeFam <- cgss2012$a62
cgss2012$incomeFam[cgss2012$incomeFam >= 9999990] <- NA


cgss2012$party <- if_else(cgss2012$a10 %in% c(1, 3), 1, 0) # including party and cyl
cgss2012$health <- cgss2012$a15

cgss2012$rural <- ifelse(cgss2012$s5a == 6, NA, cgss2012$s5a)
cgss2012$rural[cgss2012$rural != 1] <- 0 # only central city zones are coded as urban
cgss2012$hukouRural <- cgss2012$a18
cgss2012$hukouRural[cgss2012$hukouRural %in% c(2:6)] <- 0
cgss2012$hukouRural[cgss2012$hukouRural %in% c(7, 8)] <- NA # no hukou or others

cgss2012$migrant <- if_else(cgss2012$a21 < 3, 0, 1)

cgss2012$marriage <- if_else(cgss2012$a69 == 3, 1, 0)

# life style
media_ratio <- function() {
  total <- rowSums(select(cgss2012, a281:a286), na.rm = TRUE) # calculate the maximum value of using the traditional media
  tvRadio <- rowSums(select(cgss2012, a283:a284), na.rm = TRUE) ## calculate the maximum value of using the new media
  ratio <- tvRadio / total # to what extent one use new media more than traditional
  return(ratio)
}
cgss2012$mediaR <- media_ratio()

cgss2012$tvRadio <- cgss2012$a283 + cgss2012$a284
cgss2012$internet <- cgss2012$a285 + cgss2012$a286
cgss2012$paperMedia <- cgss2012$a281 + cgss2012$a282

# Language
cgss2012$mandarinListen <- cgss2012$a49
cgss2012$mandarinSpeak <- cgss2012$a50
cgss2012$mandarin <- select(cgss2012, mandarinSpeak, mandarinListen) %>%
  rowMeans(na.rm = TRUE) # using mean in case some respondents only answer the speaking or only answer the listening

cgss2012$englishListen <- cgss2012$a51
cgss2012$englishSpeak <- cgss2012$a52
cgss2012$english <- select(cgss2012, englishSpeak, englishListen) %>%
  rowMeans(na.rm = TRUE)

temp <- group_by(cgss2012, block) %>%
  summarise(
    lanSphere = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphere_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphere_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVar = var(mandarin, na.rm = TRUE),
    lanSphereVar_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVar_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2012 <- left_join(cgss2012, temp)

temp <- group_by(cgss2012, county) %>%
  summarise(
    lanSphereC = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphereC_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphereC_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVarC = var(mandarin, na.rm = TRUE),
    lanSphereVarC_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVarC_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2012 <- left_join(cgss2012, temp) %>%
  mutate(
    lanDiff = mandarin - lanSphere, lanDiffC = mandarin - lanSphereC,
    lanRatio = mandarin / lanSphere, lanRatioC = mandarin / lanSphereC,
    lanDiff_s = mandarinSpeak - lanSphere_s, lanDiffC_s = mandarinSpeak - lanSphereC_s,
    lanRatio_s = mandarinSpeak / lanSphere_s, lanRatioC_s = mandarinSpeak / lanSphereC_s
  )


# Occupation
cgss2012$job <- cgss2012$a59j
cgss2012$job[cgss2012$a58 > 3] <- 8 # 8 = unemployed
cgss2012$job[cgss2012$a58 %in% c(2, 3)] <- 9 # agriculture
cgss2012$job[cgss2012$job == 2 & cgss2012$a59k == 1] <- 10 # SOE
cgss2012$job[cgss2012$job == 2 & cgss2012$a59k %in% c(2, 3, 4)] <- 11 # group or private
cgss2012$job[cgss2012$job == 2 & cgss2012$a59k == 5] <- 12 # foreign capital
cgss2012$job <- recode(as.numeric(cgss2012$job), `1` = 1, `6` = 2, `10` = 3, `11` = 4, `12` = 5, `2` = 6, `3` = 7, `4` = 8, `5` = 9, `7` = 10, `8` = 12, `9` = 11)
# reorder the categories of occupation

cgss2012$job <- factor(cgss2012$job, labels = c("Government", "Military", "Company_SOE", "Company_Private", "Company_Foreign", "Company_Others", "Public", "Organization", "Self_Employed", "Others", "Agriculture", "Unemployed"), exclude = c(NA, NaN)) %>%
  relevel(ref = "Agriculture") # Setting the modal group as the reference group

cgss2012$job6 <- Recode(as.numeric(cgss2012$job), "1 = 'Unemployed'; 2:3 = 'Governmental'; 8 = 'Governmental'; 4 = 'SOE'; 5:7 = 'Private'; 10 = 'Private'; 9 = 'Others'; 11 = 'Others'; 12 = 'Agriculture'") %>%
  as.factor() %>%
  relevel(ref = "Agriculture") # reference is governmental

cgss2012$job3 <- Recode(as.numeric(cgss2012$job6), "c(1,3,4,5) = 2; 2 = 1; 6 = 0") %>%
  factor(labels = c("Unemployed", "Agriculture", "Nonarg"))
cgss2012$job2 <- Recode(as.numeric(cgss2012$job6), "c(1,3,4,5) = 1; c(2,6) = 0") %>%
  factor(labels = c("Agriculture", "Nonarg"))

# Social attitudes
cgss2012$trustInter <- cgss2012$a33
cgss2012$trustInter_f <- as.factor(cgss2012$trustInter)
cgss2012$socialFair <- cgss2012$a35
cgss2012$socialFair_f <- as.factor(cgss2012$socialFair)
cgss2012$socialHappy <- cgss2012$a36
cgss2012$socialHappy_f <- as.factor(cgss2012$socialHappy)
cgss2012$socialTolerance <- select(cgss2012, a38:a40) %>%
  rowMeans(na.rm = TRUE) # average of sex prior to or outside marriage and homosexuality
cgss2012$socialLevel <- cgss2012$a43a


# 2013 ####
cgss2013 <- read_dta("D:/Data/China/surveys/CGSS/Data/cgss2013_14.dta")
cgss2013$id <- as.factor(cgss2013$id)
cgss2013[cgss2013 < 0] <- NA
cgss2013$year <- 2013
cgss2013$wt <- cgss2013$weight
cgss2013$ab <- is.na(cgss2013$c1b) # use a question only in A version to identify A/B. Crosschecked with d13 (a question only in B).

# area
cgss2013$prov <- as.character(cgss2013$s41)
cgss2013$prov <- recode(cgss2013$prov, "11" = "北京市", "12" = "天津市", "13" = "河北省", "14" = "山西省", "15" = "内蒙古自治区", "21" = "辽宁省", "22" = "吉林省", "23" = "黑龙江省", "31" = "上海市", "32" = "江苏省", "33" = "浙江省", "34" = "安徽省", "35" = "福建省", "36" = "江西省", "37" = "山东省", "41" = "河南省", "42" = "湖北省", "43" = "湖南省", "44" = "广东省", "45" = "广西壮族自治区", "50" = "重庆市", "51" = "四川省", "52" = "贵州省", "53" = "云南省", "61" = "陕西省", "62" = "甘肃省", "63" = "青海省", "64" = "宁夏回族自治区")

cgss2013$county <- cgss2013$s42


cgss2013$regionLan <- case_when(
  cgss2013$prov %in% c("北京市", "天津市", "河北省", "辽宁省", "吉林省", "黑龙江省", "山东省", "陕西省", "甘肃省", "宁夏回族自治区", "河南省", "湖北省", "重庆市", "四川省", "江苏省", "安徽省", "贵州省", "云南省") ~ "mandarin",
  cgss2013$prov %in% c("山西省") ~ "jin",
  cgss2013$prov %in% c("上海市", "浙江省") ~ "wu",
  cgss2013$prov %in% c("福建省", "海南省") ~ "min",
  cgss2013$prov %in% c("湖南省") ~ "xiang",
  cgss2013$prov %in% c("江西省") ~ "gan",
  cgss2013$prov %in% c("广东省") ~ "canHakka",
  cgss2013$prov %in% c("广西壮族自治区") ~ "ping",
  cgss2013$prov %in% c("内蒙古自治区", "新疆维吾尔自治区", "西藏自治区", "青海省") ~ "minority",
  TRUE ~ as.character(cgss2013$prov)
) %>%
  as.factor() %>%
  relevel(ref = "mandarin")

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2013", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp = ...10) %>%
  mutate(gdpp = as.numeric(gdpp)) %>%
  select(prov, gdpp)
cgss2013 <- left_join(cgss2013, prov)


prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2008", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp5y = ...10) %>%
  mutate(gdpp5y = as.numeric(gdpp5y)) %>%
  select(prov, gdpp5y)
cgss2013 <- left_join(cgss2013, prov)

prov <- read_excel("D:/Data/China/economy/provPopulation.xlsx", sheet = "2013", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, popu = ...2) %>%
  mutate(popu = as.numeric(popu)) %>%
  select(prov, popu) %>%
  left_join(read_excel("D:/Data/China/economy/provSize.xlsx")) %>%
  mutate(density = popu * 10000 / size, densLog = log(density)) %>%
  select(prov, popu, density, densLog)
cgss2013 <- left_join(cgss2013, prov)

prov <- read_excel("D:/Data/China/economy/provTravel.xlsx", sheet = "2013", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, tourist = ...2) %>%
  mutate(tourist = as.numeric(tourist)) %>%
  select(prov, tourist)
cgss2013 <- left_join(cgss2013, prov)

prov <- read_excel("D:/Data/China/economy/provEdu.xlsx", sheet = "2013", skip = 4, col_names = FALSE) %>%
  mutate(
    prov = ...1, teacher = ...11, teachLog = log(teacher), pupil = ...5,
    teacherR = teacher / pupil, teacherRLog = log(teacherR)
  ) %>%
  select(prov, teacher, teachLog, teacherR, teacherRLog)
cgss2013 <- left_join(cgss2013, prov)

rm(prov)

cgss2013$prov <- as.factor(cgss2013$prov) %>%
  relevel(ref = "北京市")

cgss2013$female <- cgss2013$a2 - 1
cgss2013$age <- 2013 - cgss2013$a3a
cgss2013$generation <- case_when(
  cgss2013$a3a < 1950 ~ "Founding Time",
  cgss2013$a3a >= 1950 & cgss2013$a3a < 1966 ~ "Before Cultural Revolution",
  cgss2013$a3a >= 1966 & cgss2013$a3a < 1978 ~ "Cultural Revolution",
  cgss2013$a3a >= 1978 ~ "Reform and Opening-Up",
  TRUE ~ as.character(cgss2013$a3a)
) %>%
  factor(levels = c(
    "Founding Time",
    "Before Cultural Revolution",
    "Cultural Revolution",
    "Reform and Opening-Up"
  ))
cgss2013$age3 <- case_when(
  cgss2013$a3a < 1956 ~ "Before Standardization",
  cgss2013$a3a >= 1956 & cgss2013$a3a < 1980 ~ "Before 9yr Education",
  cgss2013$a3a >= 1980 ~ "After 9yr Education",
  TRUE ~ as.character(cgss2013$a3a)
) %>%
  factor(levels = c(
    "Before Standardization",
    "Before 9yr Education",
    "After 9yr Education"
  ))

cgss2013$race <- factor(cgss2013$a4, labels = c("han", "mongolian", "manchu", "hui", "tibetan", "zhuang", "others")) # no uyghur
cgss2013$minority <- ifelse(as.numeric(cgss2013$race) != 1, 1, 0)
cgss2013$minor_hhm <- ifelse(as.numeric(cgss2013$race) %in% c(1, 3, 4), 0, 1)

cgss2013$edu <- Recode(cgss2013$a7a, "6:8 = 5; 9:10 = 6; 11:12 = 7; 13 = 8; 14 = NA") - 1
cgss2013$edu9yr <- case_when(
  cgss2013$edu <= 2 ~ 0, # 0 for not taking elementary
  cgss2013$edu > 3 & 1958 - cgss2013$a3a < 16 ~ 1, # middle school and smaller than 15 in 1959
  cgss2013$edu > 2 & 1958 - cgss2013$a3a < 13 ~ 1, # elementary school and smaller than 13 in 1959
  is.na(cgss2013$edu) ~ 0
)

cgss2013$incomePer <- cgss2013$a8a
cgss2013$incomePer[cgss2013$incomePer >= 9999990] <- NA
cgss2013$incomeFam <- cgss2013$a62
cgss2013$incomeFam[cgss2013$incomeFam >= 9999990] <- NA

cgss2013$party <- if_else(cgss2013$a10 %in% c(1, 3), 1, 0) # including party and cyl
cgss2013$health <- cgss2013$a15

cgss2013$rural <- ifelse(cgss2013$s5a == 6, NA, cgss2013$s5a)
cgss2013$rural[cgss2013$rural != 1] <- 0 # only central city zones are coded as urban
cgss2013$hukouRural <- cgss2013$a18
cgss2013$hukouRural[cgss2013$hukouRural %in% c(2:6)] <- 0
cgss2013$hukouRural[cgss2013$hukouRural %in% c(7, 8)] <- NA # no hukou or others

cgss2013$migrant <- if_else(cgss2013$a21 < 3, 0, 1)

cgss2013$marriage <- if_else(cgss2013$a69 %in% c(3, 4), 1, 0)

# life style
media_ratio <- function() {
  total <- rowSums(select(cgss2013, a281:a286), na.rm = TRUE) # calculate the maximum value of using the traditional media
  tvRadio <- rowSums(select(cgss2013, a283:a284), na.rm = TRUE) ## calculate the maximum value of using the new media
  ratio <- tvRadio / total # to what extent one use new media more than traditional
  return(ratio)
}
cgss2013$mediaR <- media_ratio()

cgss2013$tvRadio <- cgss2013$a283 + cgss2013$a284
cgss2013$internet <- cgss2013$a285 + cgss2013$a286
cgss2013$paperMedia <- cgss2013$a281 + cgss2013$a282

# Social attitudes
cgss2013$trustInter <- cgss2013$a33
cgss2013$socialFair <- cgss2013$a35
cgss2013$socialFair_f <- as.factor(cgss2013$socialFair)
cgss2013$socialHappy <- cgss2013$a36
cgss2013$socialHappy_f <- as.factor(cgss2013$socialHappy)
cgss2013$socialTolerance <- select(cgss2013, a38:a40) %>%
  rowMeans(na.rm = TRUE) # average of sex prior to or outside marriage and homosexuality
cgss2013$socialLevel <- cgss2013$a43a

# Language
cgss2013$mandarinListen <- cgss2013$a49
cgss2013$mandarinSpeak <- cgss2013$a50
cgss2013$mandarin <- select(cgss2013, mandarinSpeak, mandarinListen) %>%
  rowSums(na.rm = TRUE)

cgss2013$englishListen <- cgss2013$a51
cgss2013$englishSpeak <- cgss2013$a52
cgss2013$english <- select(cgss2013, englishSpeak, englishListen) %>%
  rowSums(na.rm = TRUE)


temp <- group_by(cgss2013, county) %>%
  summarise(
    lanSphereC = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphereC_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphereC_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVarC = var(mandarin, na.rm = TRUE),
    lanSphereVarC_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVarC_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2013 <- left_join(cgss2013, temp) %>%
  mutate(
    lanDiffC = mandarin - lanSphereC,
    lanRatioC = mandarin / lanSphereC,
    lanDiffC_s = mandarinSpeak - lanSphereC_s,
    lanRatioC_s = mandarinSpeak / lanSphereC_s
  )
rm(temp)


# Occupation
cgss2013$job <- cgss2013$a59j
cgss2013$job[cgss2013$a58 > 3] <- 8 # 8 = unemployed
cgss2013$job[cgss2013$a58 %in% c(2, 3)] <- 9 # agriculture
cgss2013$job[cgss2013$job == 2 & cgss2013$a59k == 1] <- 10 # SOE
cgss2013$job[cgss2013$job == 2 & cgss2013$a59k %in% c(2, 3, 4)] <- 11 # group or private
cgss2013$job[cgss2013$job == 2 & cgss2013$a59k == 5] <- 12 # foreign capital
cgss2013$job <- recode(as.numeric(cgss2013$job), `1` = 1, `6` = 2, `10` = 3, `11` = 4, `12` = 5, `2` = 6, `3` = 7, `4` = 8, `5` = 9, `8` = 12, `9` = 11)
# reorder the categories of occupation
# no 7 (others)

cgss2013$job <- factor(cgss2013$job, labels = c("Government", "Military", "Company_SOE", "Company_Private", "Company_Foreign", "Company_Others", "Public", "Organization", "Self_Employed", "Agriculture", "Unemployed"), exclude = c(NA, NaN)) %>%
  relevel(ref = "Agriculture") # Setting the modal group as the reference group

cgss2013$job6 <- Recode(as.numeric(cgss2013$job), "1 = 'Unemployed'; 2:3 = 'Governmental'; 8 = 'Governmental'; 4 = 'SOE'; 5:7 = 'Private'; 10 = 'Private'; 9 = 'Others'; 11 = 'Agriculture'") %>%
  as.factor() %>%
  relevel(ref = "Agriculture") # reference is governmental

cgss2013$job3 <- Recode(as.numeric(cgss2013$job6), "c(1,3,4,5) = 2; 2 = 1; 6 = 0") %>%
  factor(labels = c("Unemployed", "Agriculture", "Nonarg"))
cgss2013$job2 <- Recode(as.numeric(cgss2013$job6), "c(1,3,4,5) = 1; c(2,6) = 0") %>%
  factor(labels = c("Agriculture", "Nonarg"))

## Social identity
cgss2013$trustInter <- cgss2013$a33
cgss2013$socialFair <- cgss2013$a35
cgss2013$socialLevel <- cgss2013$a43a


# 2015 ####
cgss2015 <- read_dta("D:/Data/China/surveys/CGSS/Data/cgss2015_14.dta") %>%
  filter(!is.na(weight))
cgss2015[cgss2015 < 0] <- NA
cgss2015$year <- 2015
cgss2015$wt <- cgss2015$weight

# area
cgss2015$prov <- as.character(cgss2015$s41)
cgss2015$prov <- recode(cgss2015$prov, "1" = "上海市", "2" = "云南省", "3" = "内蒙古自治区", "4" = "北京市", "5" = "吉林省", "6" = "四川省", "7" = "天津市", "8" = "宁夏回族自治区", "9" = "安徽省", "10" = "山东省", "11" = "山西省", "12" = "广东省", "13" = "广西壮族自治区", "15" = "江苏省", "16" = "江西省", "17" = "河北省", "18" = "河南省", "19" = "浙江省", "21" = "湖北省", "22" = "湖南省", "23" = "甘肃省", "24" = "福建省", "26" = "贵州省", "27" = "辽宁省", "28" = "重庆市", "29" = "陕西省", "30" = "青海省", "31" = "黑龙江省")

cgss2015$county <- cgss2015$s42


cgss2015$regionLan <- case_when(
  cgss2015$prov %in% c("北京市", "天津市", "河北省", "辽宁省", "吉林省", "黑龙江省", "山东省", "陕西省", "甘肃省", "宁夏回族自治区", "河南省", "湖北省", "重庆市", "四川省", "江苏省", "安徽省", "贵州省", "云南省") ~ "mandarin",
  cgss2015$prov %in% c("山西省") ~ "jin",
  cgss2015$prov %in% c("上海市", "浙江省") ~ "wu",
  cgss2015$prov %in% c("福建省", "海南省") ~ "min",
  cgss2015$prov %in% c("湖南省") ~ "xiang",
  cgss2015$prov %in% c("江西省") ~ "gan",
  cgss2015$prov %in% c("广东省") ~ "canHakka",
  cgss2015$prov %in% c("广西壮族自治区") ~ "ping",
  cgss2015$prov %in% c("内蒙古自治区", "新疆维吾尔自治区", "西藏自治区", "青海省") ~ "minority",
  TRUE ~ as.character(cgss2015$prov)
) %>%
  as.factor() %>%
  relevel(ref = "mandarin")

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2015", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp = ...10) %>%
  mutate(gdpp = as.numeric(gdpp)) %>%
  select(prov, gdpp)
cgss2015 <- left_join(cgss2015, prov)

prov <- read_excel("D:/Data/China/economy/provGDP.xlsx", sheet = "2010", skip = 3, col_names = FALSE) %>%
  rename(prov = ...1, gdpp5y = ...10) %>%
  mutate(gdpp5y = as.numeric(gdpp5y)) %>%
  select(prov, gdpp5y)
cgss2015 <- left_join(cgss2015, prov)

prov <- read_excel("D:/Data/China/economy/provPopulation.xlsx", sheet = "2015", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, popu = ...2) %>%
  mutate(popu = as.numeric(popu)) %>%
  select(prov, popu) %>%
  left_join(read_excel("D:/Data/China/economy/provSize.xlsx")) %>%
  mutate(density = popu * 10000 / size, densLog = log(density)) %>%
  select(prov, popu, density, densLog)
cgss2015 <- left_join(cgss2015, prov)

prov <- read_excel("D:/Data/China/economy/provTravel.xlsx", sheet = "2015", skip = 2, col_names = FALSE) %>%
  rename(prov = ...1, tourist = ...2) %>%
  mutate(tourist = as.numeric(tourist)) %>%
  select(prov, tourist)
cgss2015 <- left_join(cgss2015, prov)

prov <- read_excel("D:/Data/China/economy/provEdu.xlsx", sheet = "2015", skip = 4, col_names = FALSE) %>%
  mutate(
    prov = ...1, teacher = ...11, teachLog = log(teacher), pupil = ...5,
    teacherR = teacher / pupil, teacherRLog = log(teacherR)
  ) %>%
  select(prov, teacher, teachLog, teacherR, teacherRLog)
cgss2015 <- left_join(cgss2015, prov)

rm(prov)

cgss2015$prov <- as.factor(cgss2015$prov) %>%
  relevel(ref = "北京市")

cgss2015$female <- cgss2015$a2 - 1
cgss2015$age <- 2015 - cgss2015$a301
cgss2015$generation <- case_when(
  cgss2015$a301 < 1950 ~ "Founding Time",
  cgss2015$a301 >= 1950 & cgss2015$a301 < 1966 ~ "Before Cultural Revolution",
  cgss2015$a301 >= 1966 & cgss2015$a301 < 1978 ~ "Cultural Revolution",
  cgss2015$a301 >= 1978 ~ "Reform and Opening-Up",
  TRUE ~ as.character(cgss2015$a301)
) %>%
  factor(levels = c(
    "Founding Time",
    "Before Cultural Revolution",
    "Cultural Revolution",
    "Reform and Opening-Up"
  ))
cgss2015$age3 <- case_when(
  cgss2015$a301 < 1956 ~ "Before Standardization",
  cgss2015$a301 >= 1956 & cgss2015$a301 < 1980 ~ "Before 9yr Education",
  cgss2015$a301 >= 1980 ~ "After 9yr Education",
  TRUE ~ as.character(cgss2015$a301)
) %>%
  factor(levels = c(
    "Before Standardization",
    "Before 9yr Education",
    "After 9yr Education"
  ))


cgss2015$race <- factor(cgss2015$a4, labels = c("han", "mongolian", "manchu", "hui", "tibetan", "zhuang", "uygur", "others")) # no uyghur
cgss2015$minority <- ifelse(as.numeric(cgss2015$race) != 1, 1, 0)
cgss2015$minor_hhm <- ifelse(as.numeric(cgss2015$race) %in% c(1, 3, 4), 0, 1)

cgss2015$edu <- Recode(cgss2015$a7a, "6:8 = 5; 9:10 = 6; 11:12 = 7; 13 = 8; 14 = NA") - 1
cgss2015$edu9yr <- case_when(
  cgss2015$edu <= 2 ~ 0, # 0 for not taking elementary
  cgss2015$edu > 3 & 1958 - cgss2015$a301 < 16 ~ 1, # middle school and smaller than 15 in 1959
  cgss2015$edu > 2 & 1958 - cgss2015$a301 < 13 ~ 1, # elementary school and smaller than 13 in 1959
  is.na(cgss2015$edu) ~ 0
)

cgss2015$incomePer <- cgss2015$a8a
cgss2015$incomePer[cgss2015$incomePer >= 9999990] <- NA
cgss2015$incomeFam <- cgss2015$a62
cgss2015$incomeFam[cgss2015$incomeFam >= 9999990] <- NA
cgss2015$incomeFam_std <- rescale(cgss2015$incomeFam)

cgss2015$party <- if_else(cgss2015$a10 %in% c(1, 3), 1, 0) # including party and cyl
cgss2015$health <- cgss2015$a15

cgss2015$rural <- cgss2015$s1 - 1
cgss2015$hukouRural <- cgss2015$a18
cgss2015$hukouRural[cgss2015$hukouRural %in% c(2:6)] <- 0
cgss2015$hukouRural[cgss2015$hukouRural %in% c(7, 8)] <- NA # no hukou or others

cgss2015$migrant <- if_else(cgss2015$a21 < 3, 0, 1)

cgss2015$marriage <- if_else(cgss2015$a69 %in% c(3, 4), 1, 0)

# life style
media_ratio <- function() {
  total <- rowSums(select(cgss2015, a281:a286), na.rm = TRUE) # calculate the maximum value of using the traditional media
  tvRadio <- rowSums(select(cgss2015, a283:a284), na.rm = TRUE) ## calculate the maximum value of using the new media
  ratio <- tvRadio / total # to what extent one use new media more than traditional
  return(ratio)
}
cgss2015$mediaR <- media_ratio()

cgss2015$tvRadio <- cgss2015$a283 + cgss2015$a284
cgss2015$internet <- cgss2015$a285 + cgss2015$a286
cgss2015$paperMedia <- cgss2015$a281 + cgss2015$a282

# Social attitudes
cgss2015$trustInter <- cgss2015$a33
cgss2015$socialFair <- cgss2015$a35
cgss2015$socialFair_f <- as.factor(cgss2015$socialFair)
cgss2015$socialHappy <- cgss2015$a36
cgss2015$socialHappy_f <- as.factor(cgss2015$socialHappy)
cgss2015$socialTolerance <- select(cgss2015, a38:a40) %>%
  rowMeans(na.rm = TRUE) # average of sex prior to or outside marriage and homosexuality
cgss2015$socialLevel <- cgss2015$a431

# Language
cgss2015$mandarinListen <- cgss2015$a49
cgss2015$mandarinSpeak <- cgss2015$a50
cgss2015$mandarin <- select(cgss2015, mandarinSpeak, mandarinListen) %>%
  rowSums(na.rm = TRUE)

cgss2015$englishListen <- cgss2015$a51
cgss2015$englishSpeak <- cgss2015$a52
cgss2015$english <- select(cgss2015, englishSpeak, englishListen) %>%
  rowSums(na.rm = TRUE)


temp <- group_by(cgss2015, county) %>%
  summarise(
    lanSphereC = weighted.mean(mandarin, w = wt, na.rm = TRUE),
    lanSphereC_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphereC_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE),
    lanSphereVarC = var(mandarin, na.rm = TRUE),
    lanSphereVarC_s = var(mandarinSpeak, na.rm = TRUE),
    lanSphereVarC_l = var(mandarinListen, na.rm = TRUE)
  )

cgss2015 <- left_join(cgss2015, temp) %>%
  mutate(
    lanDiffC = mandarin - lanSphereC,
    lanRatioC = mandarin / lanSphereC,
    lanDiffC_s = mandarinSpeak - lanSphereC_s,
    lanRatioC_s = mandarinSpeak / lanSphereC_s
  )
rm(temp)


# Occupation
cgss2015$job <- cgss2015$a59j
cgss2015$job[cgss2015$a58 > 3] <- 8 # 8 = unemployed
cgss2015$job[cgss2015$a58 %in% c(2, 3)] <- 9 # agriculture
cgss2015$job[cgss2015$job == 2 & cgss2015$a59k == 1] <- 10 # SOE
cgss2015$job[cgss2015$job == 2 & cgss2015$a59k %in% c(2, 3, 4)] <- 11 # group or private
cgss2015$job[cgss2015$job == 2 & cgss2015$a59k == 5] <- 12 # foreign capital
cgss2015$job <- recode(as.numeric(cgss2015$job), `1` = 1, `6` = 2, `10` = 3, `11` = 4, `12` = 5, `2` = 6, `3` = 7, `4` = 8, `5` = 9, `8` = 12, `9` = 11)
# reorder the categories of occupation
# no 7 (others)

cgss2015$job <- factor(cgss2015$job, labels = c("Government", "Military", "Company_SOE", "Company_Private", "Company_Foreign", "Company_Others", "Public", "Organization", "Self_Employed", "Agriculture", "Unemployed"), exclude = c(NA, NaN)) %>%
  relevel(ref = "Agriculture") # Setting the modal group as the reference group

cgss2015$job6 <- Recode(as.numeric(cgss2015$job), "1 = 'Unemployed'; 2:3 = 'Governmental'; 8 = 'Governmental'; 4 = 'SOE'; 5:7 = 'Private'; 10 = 'Private'; 9 = 'Others'; 11 = 'Agriculture'") %>%
  as.factor() %>%
  relevel(ref = "Governmental") # reference is governmental

cgss2015$job3 <- Recode(as.numeric(cgss2015$job6), "c(1,3,4,5) = 2; 2 = 1; 6 = 0") %>%
  factor(labels = c("Unemployed", "Agriculture", "Nonarg"))
cgss2015$job2 <- Recode(as.numeric(cgss2015$job6), "c(1,3,4,5) = 1; c(2,6) = 0") %>%
  factor(labels = c("Agriculture", "Nonarg"))

cgss2015$id <- as.factor(cgss2015$id) # for later binding with other datasets

## Social identity
cgss2015$trustInter <- cgss2015$a33
cgss2015$socialFair <- cgss2015$a35
cgss2015$socialLevel <- cgss2015$a431

# Standardization ####

dv_ls <- c("mandarinSpeak", "mandarinListen", "socialLevel", "socialFair_f", "trustInter_f", "socialHappy_f", "socialTolerance")
ivs_num <- c("edu", "edu9yr", "age", "generation", "minor_hhm", "female", "rural", "migrant", "tvRadio", "internet", "paperMedia", "mediaR", "gdpp", "gdpp5y", "tourist", "popu", "density", "teacher", "teacherR", "incomePer")
ivs_cat <- c("race", "popu", "densLog", "teachLog", "teacherRLog", "age3", "job6", "job3", "prov", "year", "wt")

cgss1015_raw <- map2_df(
  list(cgss2010, cgss2012, cgss2013, cgss2015), c(2010, 2012, 2013, 2015),
  function(cgss, yr) {
    select(cgss, one_of(c(dv_ls, ivs_num, ivs_cat))) %>%
      mutate(year = yr)
  }
)

cgss1015 <- cgss1015_raw %>%
  mutate(
    prov = factor(prov, levels = levels(cgss2010$prov)),
    year = as.factor(year), age3 = as.factor(age3),
    gdpLog = arm::rescale(log(gdpp)),
    popuLog = arm::rescale(log(popu)),
    densLog = arm::rescale(densLog),
    teacherRLog = arm::rescale(teacherRLog),
    mandarinSpeak_f = as.factor(mandarinSpeak),
    mandarinListen_f = as.factor(mandarinListen),
    race = as.factor(race)
  ) %>%
  mutate_at(c(ivs_num), arm::rescale) # standardized individual level vars.


## ----influence, fig.cap='语言治理分层调节模型', fig.width=6, fig.align="center"----
knitr::include_graphics('figure/fig_influence.png')


## ----avgMap, fig.cap="中国大陆地区普通话听说水平分布", results="hide", fig.width=8----
provLan <- group_by(cgss1015, prov) %>%
  summarise(
    lanSphere_s = weighted.mean(mandarinSpeak, w = wt, na.rm = TRUE),
    lanSphere_l = weighted.mean(mandarinListen, w = wt, na.rm = TRUE)
  ) %>%
  gather("cat", "avg", -prov) %>%
  mutate(cat = factor(cat,
    levels = c("lanSphere_l", "lanSphere_s"),
    labels = c("听", "说")
  ))

provLan <- map_df(c("台湾省", "香港特别行政区", NA), function(term) {
  data.frame(prov = term, avg = NA, cat = unique(provLan$cat))
}) %>% # adding the missing values to avoid a third category in later facet plotting
  bind_rows(provLan) %>%
  rename(NAME = prov) # for merging to the map

provLan <- provLan %>%
  filter(!is.na(avg)) %>%
  # mutate(cat = factor(cat, levels = c("说", "听"))) %>%
  group_by(cat) %>%
  arrange(desc(avg), .by_group = TRUE) %>%
  mutate(NAME = factor(NAME, levels = filter(., cat == "说")$NAME))

provCode <- readxl::read_xlsx("D:/Data/China/spatial/administrativeCodeIndex_yufan.xlsx", sheet = "1999") %>% 
  select(province_nm, province_code) %>% 
  distinct

provLan <- left_join(provLan, provCode, by = c("NAME" = "province_nm")) %>% 
  rename(Proficiency = avg)

## Nine-dash lines
source("D:/Data/China/spatial/completeMap.R")

chinaMap <-
  completeChina(
    "D:/Data/China/spatial/easyChart/bou2_4m/bou2_4p.shp",
    "D:/Data/China/spatial/easyChart/nanhai_9lines.csv"
  )

draw_map <- function(proficiency_type){
  provLan_sub <- filter(provLan, cat == proficiency_type)
  
  chinaMap$df_China <-
    left_join(chinaMap$df_China,
              provLan_sub,
              by = c("ADCODE99" = "province_code"))
  
  # Preset for the 9lines
  
  Width <- 9
  Height <- 9
  long_Start <- 124
  lat_Start <- 16
  
  # the map
  ggplot() +
    geom_polygon(
      data = chinaMap$df_China,
      aes(
        x = long,
        y = lat,
        group = interaction(class, group),
        fill = Proficiency
      ),
      # colour = "black",
      size = 0.25
    ) +
    geom_rect(
      aes(
        xmin = long_Start,
        xmax = long_Start + Width + 0.3,
        ymin = lat_Start - 0.3,
        ymax = lat_Start + Height
      ),
      fill = NA,
      colour = "black",
      size = 0.25
    ) +
    geom_line(
      data = chinaMap$df_NanHaiLine,
      aes(x = long, y = lat, group = ID),
      colour = "black",
      size = 1
    ) +
    scale_fill_viridis_c() +
    coord_cartesian() +
    ylim(15, 55) +
    # guides(
    #   fill = guide_legend(override.aes = list(size = 1))) +
    ylab("纬度") + xlab("经度") +
    ggtitle(proficiency_type)
}

map_plots <- map(c("听", "说"), draw_map)

ggpubr::ggarrange(plotlist = map_plots, legend = "bottom", common.legend = TRUE)



## ----factor, fig.cap='影响语言治理因素', fig.width=6----
knitr::include_graphics('figure/fig_factor.png')


## ----descriptive--------------
kable(c(1:3), caption = "相关变量描述性统计")


## ----consequence, cache = TRUE----
dv_ls <- c("socialLevel", "socialTolerance", "trustInter_f", "socialHappy_f", "efficacyIn", "efficacyEx")
iv_ls <- c("mandarinListen", "mandarinSpeak", "edu", "tvRadio", "internet", "paperMedia", "age", "rural", "migrant", "minor_hhm", "incomePer", "female")
ctrl_cat <- c("prov", "year")

iv_model <- paste(c(iv_ls, ctrl_cat), collapse = " + ")

model_ls <- paste0(dv_ls, " ~ ", iv_model)


result_outcome <- map2(model_ls[1:2], dv_ls[1:2], function(model, dv) {
  result <- lm(model, data = cgss1015, weights = wt)
  result$dv <- dv
  result$n <- nobs(result)
  return(result)
})

result_outcome[3:4] <- map2(model_ls[3:4], dv_ls[3:4], function(model, dv) {
  result <- clm(model, data = cgss1015, weights = wt)
  result$dv <- dv
  result$n <- nobs(result)
  return(result)
})

result_outcome[5:6] <- map2(model_ls[5:6], dv_ls[5:6], function(model, dv) {
  result <- lm(model, data = cgss2010, weights = wt)
  result$dv <- dv
  result$n <- nobs(result)
  return(result)
})


cgss1015$rural <- as.factor(cgss1015$rural) %>% as.numeric() - 1 # rescale to 0/1
cgss1015$migrant <- as.factor(cgss1015$migrant) %>% as.numeric() - 1

iv_ls <- c("mandarinListen * migrant", "mandarinSpeak * migrant", "rural", "edu", "tvRadio", "internet", "paperMedia", "age", "minor_hhm", "incomePer", "female")
ctrl_cat <- c("prov", "year")

iv_model <- paste(c(iv_ls, ctrl_cat), collapse = " + ")

model_ls <- paste0(dv_ls[1:4], " ~ ", iv_model)

# No year var for the 2010 models
ctrl_cat <- c("prov")

iv_model <- paste(c(iv_ls, ctrl_cat), collapse = " + ")

model_ls[5:6] <- paste0(dv_ls[5:6], " ~ ", iv_model)

result_outcome[7:8] <- map2(model_ls[1:2], dv_ls[1:2], function(model, dv) {
  lm(model, data = cgss1015, weights = wt)
})

result_outcome[9:10] <- map2(model_ls[3:4], dv_ls[3:4], function(model, dv) {
  polr(model, Hess = TRUE, data = cgss1015, weights = wt)
})

result_outcome[11:12] <- map2(model_ls[5:6], dv_ls[5:6], function(model, dv) {
  lm(model, data = cgss2010, weights = wt)
})


## ----consequence-visualization----
kable(c(1:3), caption = "语言治理对国民社会政治心理影响")


## ----consequence-interact, fig.cap = "语言治理作用的本地与流动人口对比", fig.width = 7, fig.height = 8, cache = TRUE----

dv_names <- c("社会等级", "容忍度", "人际信任", "幸福感", "效能感:内", "效能感:外")

plot_cons_int <-
  map2(result_outcome[7:12], dv_names, function(result, name) {
    plot_listen <-
      interplot(result, var1 = "mandarinListen", var2 = "migrant") +
      xlab("流动人口") +
      ylab("听") +
      ggtitle(name)

    plot_speak <-
      interplot(result, var1 = "mandarinSpeak", var2 = "migrant") +
      xlab("流动人口") +
      ylab("说") +
      ggtitle(name)

    return(list(plot_listen, plot_speak))
  })


plot_cons_int <- unlist(plot_cons_int, recursive = FALSE, use.names = FALSE)

plot_text <- textGrob(str_wrap("注：本图比较了语言现代化对城市居民和流动人口社会政治态度的影响。该影响通过普通话听、说能力与流动人口身份的交互项进行测量。图中所示为该交互项的边际效应。图中实心点代表边际效用估计，横线为回归系数95%置信区间。只有当城市居民（0）与流动人口（1）边际效用置信区间有高重合度[即`CI(Max-Min)`包含0]时，二者差异才具有统计学意义。完整结果详见线上附录。数据来源：中国综合社会调查 2010-2015。", width = 113),
  gp = gpar(fontsize = 10), x = 0.05, hjust = 0
)

grid.arrange(grobs = plot_cons_int, ncol = 4, bottom = plot_text)


## ----regFull------------------
dv_ls <- c("mandarinListen_f", "mandarinSpeak_f")
iv_ls <- c("edu", "edu9yr", "tvRadio", "internet", "paperMedia", "age", "rural", "migrant", "minor_hhm", "female", "gdpLog", "teacherR", "popuLog")
ctrl_cat <- c("prov", "year")

df_reg <- select(cgss1015, one_of(c(dv_ls, iv_ls, ctrl_cat))) %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(wt = cgss1015$wt)


iv_model <- paste(c(iv_ls, ctrl_cat), collapse = " + ")


model_ls <- paste0(dv_ls, " ~ ", iv_model) %>%
  map(as.formula)

result_language <- map2(model_ls, dv_ls, function(model, dv) {
  result <- clm(model, data = df_reg, weights = wt)
  result$dv <- dv # adding dv to identify model
  result$n <- round(nobs(result), digits = 0)
  result$aic <- round(extractAIC(result)[2], digits = 1)
  result$ll <- round(as.numeric(logLik(result)), digits = 1)
  return(result)
})


## ----factorRegression---------
kable(c(1:3), caption = "语言治理对国民社会政治心理影响")


## ----appendix, include = FALSE----
vars_cgss2010 <- c("efficacyIn", "efficacyEx", "mandarinListen", "mandarinSpeak", "edu", "tvRadio", "internet", "paperMedia", "age", "rural", "migrant", "minor_hhm", "incomePer", "female")
cgss2010_rep <- select(cgss2010, !!vars_cgss2010)

vars_cgss1015 <- c("socialLevel", "socialTolerance", "trustInter_f", "socialHappy_f", "mandarinListen", "mandarinSpeak", "edu", "edu9yr", "tvRadio", "internet", "paperMedia", "age", "rural", "migrant", "minor_hhm", "incomePer", "female", "gdpp", "teacherR", "popu", "year")
cgss1015_rep <- select(cgss1015_raw, !!vars_cgss1015) %>%
  mutate_if(is.factor, as.numeric)

save(cgss1015_rep, cgss2010_rep,
  result_language, result_outcome,
  file = "language_overview_appData.RData"
)

