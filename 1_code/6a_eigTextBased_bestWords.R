# TODO: Update this file to do all best words tables instead of files 5a, 5b and 5c

library(data.table)
library(tidyverse)

# High Predictive Words on Cross-Section ---------------------------------------
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_CS.rds")

# High Predictive Words by Industry --------------------------------------------
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry.rds")

# High Predictive Words by Life Cycle ------------------------------------------
eigText <- readRDS("2_pipeline/2_out/5b_eigText_terms_Industry.rds")

# identical(eigText$y1999$words$ind452010, eigText[[1]]$words$ind452010)
# identical(eigText$y1999[[2]]$ind452010, eigText$y1999$words$ind452010)
# ind452010 <- lapply(bestWords, "$", "ind452010")
#
# bestWords$y1999[[match("ind452010", names(bestWords$y1999))]]
# bestWords$y1999$ind452010

highPredictiveWords <- function(list_eigText, industry) {

  bestWords <- lapply(eigText, "[[", 2)

  industryWords <- lapply(bestWords, function(x, i) x[[match(industry, names(x))]], i=industry)

  industryWords[is.na(industryWords)] <- NULL

  industryWords <- bind_rows(industryWords)

  dt1 <- industryWords %>%
    filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "cop", "dROE", "Id")) ) %>%
    group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
    arrange(-coeff) %>% top_n(10) %>% mutate(coeff = round(coeff,3)) %>%
    as.data.frame

  dt2 <- industryWords %>%
    filter( !(variables %in% c("(Intercept)", "IGt0","Ret", "cop", "dROE", "Id")) ) %>%
    group_by(words = variables) %>% summarise(coeff= mean(coefficients)) %>%
    arrange(coeff) %>% top_n(-10) %>% mutate(coeff = round(coeff,3)) %>%
    as.data.frame

  output <- rbind(dt1, data.frame(words="---", coeff=NA), dt2)

  return(output)
}
# 452010 - Communications Equipment
ind452010 <- highPredictiveWords(eigText, "ind452010")
# 452020 - Computers & Peripherals
ind452020 <- highPredictiveWords(eigText, "ind452020")
# 151040 - Metals and Mining
ind151040 <- highPredictiveWords(eigText, "ind151040")
# 352010 - Biotechnology
ind352010 <- highPredictiveWords(eigText, "ind352010")


# 452010 - Communications Equipment
# 452020 - Computers & Peripherals
# 151040 - Metals and Mining
# 352010 - Biotechnology
cbind(ind452010, ind452020, ind151040, ind352010)

highPredictiveWords(eigText, 'ind101010') # Energy Equipment & Services

highPredictiveWords(eigText, 'ind101020') # Oil, Gas & Consumable Fuels
highPredictiveWords(eigText, 'ind151010') # Chemicals
highPredictiveWords(eigText, 'ind151020') # Construction Materials
# highPredictiveWords(eigText, 'ind151030') # Containers & Packaging
highPredictiveWords(eigText, 'ind151040') # Metals & Mining
# highPredictiveWords(eigText, 'ind151050') # Paper & Forest Products
highPredictiveWords(eigText, 'ind201010') # Aerospace & Defense
highPredictiveWords(eigText, 'ind201020') # Building Products
highPredictiveWords(eigText, 'ind201030') # Construction & Engineering
highPredictiveWords(eigText, 'ind201040') # Electrical Equipment
# highPredictiveWords(eigText, 'ind201050') # Industrial Conglomerates
highPredictiveWords(eigText, 'ind201060') # Machinery
# highPredictiveWords(eigText, 'ind201070') # Trading Companies & Distributors
highPredictiveWords(eigText, 'ind202010') # Commercial Services & Supplies
highPredictiveWords(eigText, 'ind202020') # Professional Services
# highPredictiveWords(eigText, 'ind203010') # Air Freight & Logistics
# highPredictiveWords(eigText, 'ind203020') # Airlines
# highPredictiveWords(eigText, 'ind203030') # Marine
highPredictiveWords(eigText, 'ind203040') # Road & Rail
# highPredictiveWords(eigText, 'ind203050') # Transportation Infrastructure
highPredictiveWords(eigText, 'ind251010') # Auto Components
# highPredictiveWords(eigText, 'ind251020') # Automobiles
highPredictiveWords(eigText, 'ind252010') # Household Durables ## < ----
highPredictiveWords(eigText, 'ind252020') # Leisure Products
highPredictiveWords(eigText, 'ind252030') # Textiles, Apparel & Luxury Goods
highPredictiveWords(eigText, 'ind253010') # Hotels, Restaurants & Leisure
highPredictiveWords(eigText, 'ind253020') # Diversified Consumer Services
# highPredictiveWords(eigText, 'ind254010') # Media (discontinued effective close of September 30, 2018)
# highPredictiveWords(eigText, 'ind255010') # Distributors
# highPredictiveWords(eigText, 'ind255020') # Internet & Direct Marketing Retail
# highPredictiveWords(eigText, 'ind255030') # Multiline Retail
highPredictiveWords(eigText, 'ind255040') # Specialty Retail
highPredictiveWords(eigText, 'ind301010') # Food & Staples Retailing
# highPredictiveWords(eigText, 'ind302010') # Beverages
highPredictiveWords(eigText, 'ind302020') # Food Products ## <---
# highPredictiveWords(eigText, 'ind302030') # Tobacco
# highPredictiveWords(eigText, 'ind303010') # Household Products
# highPredictiveWords(eigText, 'ind303020') # Personal Products
highPredictiveWords(eigText, 'ind351010') # Health Care Equipment & Supplies
highPredictiveWords(eigText, 'ind351020') # Health Care Providers & Services
# highPredictiveWords(eigText, 'ind351030') # Health Care Technology
highPredictiveWords(eigText, 'ind352010') # Biotechnology ## <---
highPredictiveWords(eigText, 'ind352020') # Pharmaceuticals ## <---
# highPredictiveWords(eigText, 'ind352030') # Life Sciences Tools & Services
# highPredictiveWords(eigText, 'ind401010') # Banks
# highPredictiveWords(eigText, 'ind401020') # Thrifts & Mortgage Finance
# highPredictiveWords(eigText, 'ind402010') # Diversified Financial Services
# highPredictiveWords(eigText, 'ind402020') # Consumer Finance
# highPredictiveWords(eigText, 'ind402030') # Capital Markets
# highPredictiveWords(eigText, 'ind402040') # Mortgage Real Estate Investment Trusts (REITs)
# highPredictiveWords(eigText, 'ind403010') # Insurance
# highPredictiveWords(eigText, 'ind451010') # Internet Software & Services (discontinued effective close of September 30, 2018)
highPredictiveWords(eigText, 'ind451020') # IT Services
highPredictiveWords(eigText, 'ind451030') # Software ### <---
highPredictiveWords(eigText, 'ind452010') # Communications Equipment
highPredictiveWords(eigText, 'ind452020') # Technology Hardware, Storage & Peripherals
highPredictiveWords(eigText, 'ind452030') # Electronic Equipment, Instruments & Components
highPredictiveWords(eigText, 'ind453010') # Semiconductors & Semiconductor Equipment
highPredictiveWords(eigText, 'ind501010') # Diversified Telecommunication Services
# highPredictiveWords(eigText, 'ind501020') # Wireless Telecommunication Services
# highPredictiveWords(eigText, 'ind502010') # Media
# highPredictiveWords(eigText, 'ind502020') # Entertainment
# highPredictiveWords(eigText, 'ind502030') # Interactive Media & Services
highPredictiveWords(eigText, 'ind551010') # Electric Utilities
# highPredictiveWords(eigText, 'ind551020') # Gas Utilities
# highPredictiveWords(eigText, 'ind551030') # Multi-Utilities
# highPredictiveWords(eigText, 'ind551040') # Water Utilities
# highPredictiveWords(eigText, 'ind551050') # Independent Power and Renewable Electricity Producers
# highPredictiveWords(eigText, 'ind601010') # Equity Real Estate Investment Trusts (REITs)"
# highPredictiveWords(eigText, 'ind601020') # Real Estate Management & Development


highPredictiveWords(eigText, 'ind252010') # Household Durables ## <---
highPredictiveWords(eigText, 'ind302020') # Food Products ## <---
highPredictiveWords(eigText, 'ind352020') # Pharmaceuticals ## <---
