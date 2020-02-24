#libraries
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)

#Download sheets (see files "the_database.xlsx" and "igo_dyads.xlsx")

pol <- read_xlsx("the_database.xlsx", sheet = 2)
eco <- read_xlsx("the_database.xlsx", sheet = 3)
pow <- read_xlsx("the_database.xlsx", sheet = 5)
base <- read_xlsx("igo_dyads.xlsx", sheet = 2)
full <- read_xlsx("igo_dyads.xlsx", sheet = 4)

#Redo political indicators
pol1 <- pol %>%
  select(3:11) %>%
  group_by(country, pol1) %>%
  summarize() %>%
  filter(pol1 != is.na(pol1)) %>%
  select(country, pol = pol1)

pol2 <- pol %>%
  select(3:11) %>%
  group_by(country2, pol2) %>%
  summarize() %>%
  filter(pol2 != is.na(pol2)) %>%
  select(country = country2, pol = pol2)

polt <- pol1 %>%
  union(pol2)

#Correct duplicated countries
correct <- which(polt$country == "Mali" & polt$pol != 7 | polt$country == "Dominican Republic" & polt$pol != 8 |
                   polt$country == "Trinidad and Tobago" & polt$pol != 10 | polt$country == "Seychelles" & polt$pol != 8 |
                   polt$country == "Paraguay" & polt$pol != 9 | polt$country == "Nigeria" & polt$pol != 4 |
                   polt$country == "Guinea" & polt$pol != -1 | polt$country == "Ghana" & polt$pol != 8 |
                   polt$country == "Togo" & polt$pol != -4)
polt <- polt[-correct,]

#Join with pow and polt
base1 <- base %>%
  select(2:4) %>%
  left_join(pow) %>%
  left_join(polt) %>%
  select(1:3, gdp, poliv = pol, open = Trade, pop = Pop, land = terr)

#Correct NA in poliv
base1$poliv[which(is.na(base1$poliv))] <- 0

#Remove duplicated and join with eco
base1 <- base1 %>%
  group_by(region, year, country, gdp, pop, open, land, poliv) %>%
  summarize() %>%
  left_join(select(eco, -gdp))

correct1 <- which(base1$region == "ESA interim" & base1$country == "Madagascar" & base1$exports != 1.9 |
                   base1$region == "ESA interim" & base1$country == "Seychelles" & base1$exports != 6.35 |
                   base1$region == "ESA interim" & base1$country == "Mauritius" & base1$exports != 10.7 |
                   base1$region == "ESA interim" & base1$country == "Zimbabwe" & base1$exports != 0.17 |
                   base1$region == "ESA full" & base1$country == "Madagascar" & base1$exports != 2.56 |
                   base1$region == "ESA full" & base1$country == "Seychelles" & base1$exports != 6.76 |
                   base1$region == "ESA full" & base1$country == "Mauritius" & base1$exports != 12.17 |
                   base1$region == "ESA full" & base1$country == "Zimbabwe" & base1$exports != 4.01 )

#Remove repeated ESA countries
base1 <- base1[-correct1,]

#Fix types of variables and create exp_imp
base2 <- base1 %>%
  ungroup() %>%
  select(region, country, year, gdp, pop, open, land, poliv, exports, imports) %>%
  mutate(gdp = as.numeric(gdp),
         pop = as.numeric(pop),
         open = as.numeric(open),
         exports = as.numeric(exports),
         imports = as.numeric(imports),
         exp_imp = (exports + imports)/2)

#Introduce country codes (ISO3, Polity IV, Vdem)
base2$iso3c <- countrycode(base2$country, origin = 'country.name', destination = 'iso3c')
base2$p4n <- countrycode(base2$iso3c, origin = 'iso3c', destination = 'p4n')
base2$vdem <- countrycode(base2$iso3c, origin = 'iso3c', destination = 'vdem')

# Create dyads
dyad_base2 <- base2 %>% 
  group_by(region) %>%
  expand(country.a = country, country.b = country) %>%
  filter(country.a < country.b) %>%
  left_join(., base2, by=c("country.a"="country", "region")) %>%
  left_join(., base2, by=c("country.b"="country", "region")) %>%
  select(region, country.a, country.b, gdp.a = gdp.x, gdp.b = gdp.y,
         poliv.a = poliv.x, poliv.b = poliv.y)

#Create Political variable
full_dataset <- base2 %>%
  group_by(region) %>%
  summarize(pol = sum(((poliv+10)/20) * gdp) / sum(gdp)) %>%
  right_join(full) %>%
  select(region, year, agreement, pref_dem = pol, institutions)

#Create Political variable2
dyad_base2 %>%
  group_by(region) %>%
  summarize(pol = 1- sum(abs((poliv.a+10)/20 - (poliv.b+10)/20) * (gdp.a + gdp.b) / (length(unique(gdp.a)))) / sum(unique(c(gdp.a, gdp.b)))) %>%

sum(abs((mercosur$poliv.a+10)/20 - (mercosur$poliv.b+10)/20) * (mercosur$gdp.a + mercosur$gdp.b) / (length(unique(mercosur$gdp.a))))
sum(unique(c(mercosur$gdp.a, mercosur$gdp.b)))

#Create Economic variable
full_dataset <- base2 %>%
  group_by(region) %>%
  summarize(eco = round((sum(exp_imp * gdp) / sum(gdp))/ 100, 3)) %>%
  right_join(full_dataset) %>%
  select(region, year, agreement, pref_trade = eco, pref_dem, institutions)

#Create Power variable
base2 <- base2 %>%
  group_by(region) %>%
  mutate(pow = gdp == max(gdp) | (open >=100 & pop < 10000 & land < 200000)) %>%
  ungroup()

full_dataset <- base2 %>%
  group_by(region) %>%
  summarize(pow = sum(pow * gdp) / sum(gdp)) %>%
  right_join(full_dataset) %>%
  select(region, year, agreement, pref_trade, pref_dem, institutions, power = pow)

results_qca <- full_dataset %>%
  mutate_at(4:7, ~if_else(. > mean(.), 1, 0)) %>%
  arrange(desc(agreement))

#Export results
write.csv(results_qca, "results_qca1.csv")
write.csv2(full_dataset, "results_qca2.csv")
