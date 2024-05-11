# 1. Comparison of raw inputs for 2021/2022 ----
d2022 <- read.spss("Data/Input/Sav/2022/Diet_Quality_032023_INTERNAL.sav", 
                   use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)
d2021 <- read.spss("Data/Input/Sav/2021/2021 Gallup World Poll data for public release.sav", 
                   use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)

## 1.1 Number of entries ----
n2021 <- d2021 %>%
  group_by(Country) %>%
  summarise(n2021= n())

n2022 <- d2022 %>%
  group_by(Country) %>%
  summarise(n2022= n())

left_join(x = n2022, y = n2021, "Country") %>%
  na.omit() %>%
  mutate(dif = n2022-n2021) %>% 
  filter(dif != 0) # new 1000 for 'Sierra Leone', and 1006 for USA

# 2. Comparison of result analysis for 2021 old and new ----
r21old <- readxl::read_excel("Data/Input/Excel/2021/Country level indicators_for website.xlsx")
r21new <- read_csv("Data/Output/CSV/results_public.csv"); glimpse(r21new)

## 2.1 Some minor data preparations ---- 
r21old <- r21old %>%
  mutate(`Mean/Prevalence` = ifelse(Indicator == "Food group diversity score" | Indicator == "NCD-Protect" | Indicator == "NCD-Risk" | Indicator == "GDR score",
                               `Mean/Prevalence`, round(`Mean/Prevalence`*100, digits = 2))) %>%
  select(c(Country, Subgroup, Indicator, `Mean/Prevalence`))

r21new <- r21new %>%
  filter(Year == 2021) %>%
  mutate(
    Country = case_when(ISO3 == "USA" ~ "United States", .default = Country),
    Country = case_when(ISO3 == "SLE" ~ "Sierra Leone", .default = Country)
  ) %>%
  select(c(Country, Subgroup, Indicator, `Mean/Prevalence`))

## 2.2 Check if names match ----
r21old$Indicator[!r21old$Indicator %in% r21new$Indicator] # 0
setdiff(r21new$Indicator, r21old$Indicator) # no differences
# Or
setequal(r21old$Indicator, r21new$Indicator) # returns a logical scalar: True so we are fine
setequal(r21old$Country, r21new$Country) # True

## 2.3 Duplicated rows ----
dim(r21old); dim(r21new) # 2021 has duplicated rows for "Dairy" indicator
r21old[duplicated(r21old),] %>% print(n = 300) # 205 rows for Dairy in 2021
r21new[duplicated(r21new),] # no duplicates
# remove duplicates 
r21old <- unique(r21old) # 9840
r21new <- unique(r21new) # 9840

## 2.4 Join and compare ----
### 2.4.1 Proportional indicators ----
# comparing all but "Foods made from grains" because of different equation 
left_join(r21old, r21new, by = names(r21old)[1:3]) %>% 
  mutate(Diff = `Mean/Prevalence.x` - `Mean/Prevalence.y`, value2021 = `Mean/Prevalence.x`, value2022 = `Mean/Prevalence.y`, .keep = "unused") %>%
  filter(Diff > 1, Indicator != "Foods made from grains") %>% 
  print(n = 200)

### 2.4.2 Non-proportional indicators ----
# For 4 non-proportional indicators 
left_join(r21old, r21new, by = names(r21old)[1:3]) %>% 
  mutate(Diff = `Mean/Prevalence.x` - `Mean/Prevalence.y`, value2021 = `Mean/Prevalence.x`, value2022 = `Mean/Prevalence.y`, .keep = "unused") %>%
  filter(Indicator %in% c("Food group diversity score", "NCD-Protect", "NCD-Risk", "GDR score")) %>%
  filter(Diff > 0.1, Indicator != "Foods made from grains") %>%
  print(n = 200) # 5 rows: The most noticeable diff: Turkey Urban FGDS with 0.12

# save the result as csv
compare21 <- left_join(r21old, r21new, by = names(r21old)[1:3]) %>% 
  mutate(value2021_old = `Mean/Prevalence.x`, 
         value2021_new = `Mean/Prevalence.y`, 
         Diff = `Mean/Prevalence.x` - `Mean/Prevalence.y`, .keep = "unused") %>%
  filter(Indicator != "Foods made from grains")

write_csv(compare21, "Data/Output/CSV/Compare2021_Old&New.csv")
# End ----









