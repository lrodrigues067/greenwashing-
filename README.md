# greenwashing-
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

# importing JPMorgan's table from excel 
JPMorgan <- read_excel("Downloads/JPMorgan.xlsx")

# importing Shell's table from excel 
shell_prices <- read_excel("Downloads/shell_prices.xlsx")

# importing Exxon's table from excel 
exxon <- read_excel("Downloads/exxon.xlsx")

# merging all datasets 
new1 <- rbind(JPMorgan, shell_prices) %>%
  rbind(exxon)

colnames(new1)[2] <- "equity"
colnames(new1)[3] <- "company"
colnames(new1)[4] <- "press"

# graphic Analysis JPMorgan 
my_data <- as_tibble(new1)
my_data

# preparing data for graphs - JPMorgan 

## first analysis 
jp1 <- my_data %>% slice(45:44)

j_p1 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21",   164.35, "JPMorgan", 
  "05/28/21",   164.24, "JPMorgan" 
)

## preparing data for the graph 
jp1 <- j_p1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## second analysis 
jp2 <- my_data %>% slice(45:41)
jp_2 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21",   164.35, "JPMorgan", 
  "05/28/21",   164.24, "JPMorgan", 
  "06/01/21",   166.05, "JPMorgan", 
  "06/02/21",   166.06, "JPMorgan", 
  "06/03/21",   166.17, "JPMorgan"
)

## preparing the data for the graph
jp2 <- jp_2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("greenwashing press release" = "red", "normal" = "black")

## ploting the graph 
ggplot(data = jp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## third analysis
jp3 <- my_data %>% slice(45:35)
jp_3 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21",   164.35, "JPMorgan", 
  "05/28/21",   164.24, "JPMorgan", 
  "06/01/21",   166.05, "JPMorgan", 
  "06/02/21",   166.06, "JPMorgan", 
  "06/03/21",   166.17, "JPMorgan", 
  "06/04/21",   166.44, "JPMorgan", 
  "06/07/21",   165.66, "JPMorgan", 
  "06/08/21",   165.00, "JPMorgan", 
  "06/09/21",   162.94, "JPMorgan", 
  "06/10/21",   160.40, "JPMorgan", 
  "06/11/21",   160.29, "JPMorgan"
)

## preparing the data for the graph 
jp3 <- jp_3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

# second analysis
jpp1 <- my_data %>% slice(52:51)
j_pp1 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/18/21",   162.35, "JPMorgan", 
  "05/19/21",   161.11, "JPMorgan" 
)

## preparing data for the graph
jpp1 <- j_pp1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-18", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

# 5 days analysis
jpp2 <- my_data %>% slice(52:47)
jpp_2 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/18/21",   162.35, "JPMorgan", 
  "05/19/21",   161.11, "JPMorgan", 
  "05/20/21",   160.83, "JPMorgan", 
  "05/21/21",   162.66, "JPMorgan", 
  "05/24/21",   163.54, "JPMorgan", 
  "05/25/21",   161.85, "JPMorgan"
)

## preparing data for the graph
jpp2 <- jpp_2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-18", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

# ten day analysis 
jpp3 <- my_data %>% slice(52:42)
jpp_3 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/18/21",   162.35, "JPMorgan", 
  "05/19/21",   161.11, "JPMorgan", 
  "05/20/21",   160.83, "JPMorgan", 
  "05/21/21",   162.66, "JPMorgan", 
  "05/24/21",   163.54, "JPMorgan", 
  "05/25/21",   161.85, "JPMorgan", 
  "05/26/21",   161.83, "JPMorgan", 
  "05/27/21",   164.35, "JPMorgan", 
  "05/28/21",   164.24, "JPMorgan", 
  "06/01/21",   166.05, "JPMorgan", 
  "06/02/21",   166.06, "JPMorgan"
)

## preparing data for the graph
jpp3 <- jpp_3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-18", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


# third analysis 
## one day analysis
jppp1 <- my_data %>% slice(55:54)
j_ppp1 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/13/21",   161.50, "JPMorgan", 
  "05/14/21",   164.01, "JPMorgan" 
)

## preparing data for the graph
jppp1 <- j_ppp1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## five day analysis 
jppp2 <- my_data %>% slice(55:50)
j_ppp2 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/13/21",   161.50, "JPMorgan", 
  "05/14/21",   164.01, "JPMorgan", 
  "05/17/21",   164.67, "JPMorgan",
  "05/18/21",   162.35, "JPMorgan",
  "05/19/21",   161.11, "JPMorgan",
  "05/20/21",   160.83, "JPMorgan"
)

## preparing data for the graph
jppp2 <- j_ppp2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


jppp3 <- my_data %>% slice(55:45)
j_ppp3 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/13/21",   161.50, "JPMorgan", 
  "05/14/21",   164.01, "JPMorgan", 
  "05/17/21",   164.67, "JPMorgan",
  "05/18/21",   162.35, "JPMorgan",
  "05/19/21",   161.11, "JPMorgan",
  "05/20/21",   160.83, "JPMorgan",
  "05/21/21",   162.66, "JPMorgan",
  "05/24/21",   163.54, "JPMorgan",
  "05/25/21",   161.85, "JPMorgan",
  "05/26/21",   161.83, "JPMorgan",
  "05/27/21",   164.35, "JPMorgan"
)

## preparing data for the graph
jppp3 <- j_ppp3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

# fourth analysis
## one day 
jpppp1 <- my_data %>% slice(361:360)
j_pppp1 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/26/20",   127., "JPMorgan", 
  "02/27/20",   121., "JPMorgan" 
)

## five days 
jpppp2 <- my_data %>% slice(361:356)

## ten days
jpppp3 <- my_data %>% slice(361:351)

## one day analysis
ggplot(data = jpppp1) + geom_point(mapping = aes(Date, equity))

## five days analysis 
ggplot(data = jpppp2) + geom_point(mapping = aes(Date, equity))

## ten days analysis
ggplot(data = jpppp3) + geom_point(mapping = aes(Date, equity))

# fifth analysis
jppppp1 <- my_data %>% slice(365:364)
jppppp2 <- my_data %>% slice(365:360)
jppppp3 <- my_data %>% slice(365:355)

## one day analysis
ggplot(data = jppppp1) + geom_point(mapping = aes(Date, equity))

## five days analysis 
ggplot(data = jppppp2) + geom_point(mapping = aes(Date, equity))

## ten days analysis
ggplot(data = jppppp3) + geom_point(mapping = aes(Date, equity))

# sixth analysis
jpppppp1 <- my_data %>% slice(934:933)
jpppppp2 <- my_data %>% slice(934:929)
jpppppp3 <- my_data %>% slice(934:924)

## one day analysis
ggplot(data = jpppppp1) + geom_point(mapping = aes(Date, equity))

## five days analysis 
ggplot(data = jpppppp2) + geom_point(mapping = aes(Date, equity))

## ten days analysis
ggplot(data = jpppppp3) + geom_point(mapping = aes(Date, equity))

# shell analysis

## first analysis
shell1 <- my_data %>% slice(2457:2456)
shell2 <- my_data %>% slice(2457:2452)
shell3 <- my_data %>% slice(2457:2447)

## one day analysis
ggplot(data = shell1) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell2) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell3) + geom_point(mapping = aes(equity, Date))

## second analysis
shell4 <- my_data %>% slice(2458:2457)
shell5 <- my_data %>% slice(2458:2453)
shell6 <- my_data %>% slice(2458:2448)

## one day analysis
ggplot(data = shell4) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell5) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell6) + geom_point(mapping = aes(equity, Date))

## third analysis
shell7 <- my_data %>% slice(2480:2479)
shell8 <- my_data %>% slice(2480:2475)
shell9 <- my_data %>% slice(2480:2470)

## one day analysis
ggplot(data = shell7) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell8) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell9) + geom_point(mapping = aes(equity, Date))

## fourth analysis
shell10 <- my_data %>% slice(2482:2481)
shell11 <- my_data %>% slice(2482:2477)
shell12 <- my_data %>% slice(2482:2472)

## one day analysis
ggplot(data = shell10) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell11) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell12) + geom_point(mapping = aes(equity, Date)) 

## fifth analysis
shell13 <- my_data %>% slice(2485:2484)
shell14 <- my_data %>% slice(2485:2480)
shell15 <- my_data %>% slice(2485:2475)

## one day analysis
ggplot(data = shell13) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell14) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell15) + geom_point(mapping = aes(equity, Date))

## sixth analysis
shell16 <- my_data %>% slice(2487:2486)
shell17 <- my_data %>% slice(2487:2482)
shell18 <- my_data %>% slice(2487:2477)

## one day analysis
ggplot(data = shell16) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell17) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell18) + geom_point(mapping = aes(equity, Date))

## seventh analysis
shell19 <- my_data %>% slice(2581:2580)
shell20 <- my_data %>% slice(2581:2576)
shell21 <- my_data %>% slice(2581:2571)

## one day analysis
ggplot(data = shell19) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell20) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell21) + geom_point(mapping = aes(equity, Date))

## eighth analysis
shell22 <- my_data %>% slice(4685:4684)
shell23 <- my_data %>% slice(4685:4680)
shell24 <- my_data %>% slice(4685:4675)

## one day analysis
ggplot(data = shell22) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = shell23) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = shell24) + geom_point(mapping = aes(equity, Date))

# Exxon Analysis 
## first analysis
exxon1 <- my_data %>% slice(4903:4902)
exxon2 <- my_data %>% slice(4903:4898)
exxon3 <- my_data %>% slice(4903:4893)

## one day analysis
ggplot(data = exxon1) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon2) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon3) + geom_point(mapping = aes(equity, Date))

## second analysis
exxon4 <- my_data %>% slice(4904:4903)
exxon5 <- my_data %>% slice(4904:4899)
exxon6 <- my_data %>% slice(4904:4894)

## one day analysis
ggplot(data = exxon4) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon5) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon6) + geom_point(mapping = aes(equity, Date))

## third analysis
exxon7 <- my_data %>% slice(4928:4927)
exxon8 <- my_data %>% slice(4928:4923)
exxon9 <- my_data %>% slice(4928:4918)

## one day analysis
ggplot(data = exxon7) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon8) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon9) + geom_point(mapping = aes(equity, Date))

## fourth analysis
exxon10 <- my_data %>% slice(4930:4929)
exxon11 <- my_data %>% slice(4930:4925)
exxon12 <- my_data %>% slice(4930:4920)

## one day analysis
ggplot(data = exxon10) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon11) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon12) + geom_point(mapping = aes(equity, Date))

## fifth analysis
exxon13 <- my_data %>% slice(4931:4930)
exxon14 <- my_data %>% slice(4931:4926)
exxon15 <- my_data %>% slice(4931:4921)

## one day analysis
ggplot(data = exxon13) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon14) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon15) + geom_point(mapping = aes(equity, Date))

## sixth analysis
exxon16 <- my_data %>% slice(4931:4930)
exxon17 <- my_data %>% slice(4931:4926)
exxon18 <- my_data %>% slice(4931:4921)

## one day analysis
ggplot(data = exxon16) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon17) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon18) + geom_point(mapping = aes(equity, Date))

## seventh analysis
exxon19 <- my_data %>% slice(4932:4931)
exxon20 <- my_data %>% slice(4932:4927)
exxon21 <- my_data %>% slice(4932:4922)

## one day analysis
ggplot(data = exxon19) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon20) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon21) + geom_point(mapping = aes(equity, Date))

## seventh analysis
exxon22 <- my_data %>% slice(4934:4933)
exxon23 <- my_data %>% slice(4934:4929)
exxon24 <- my_data %>% slice(4934:4924)

## one day analysis
ggplot(data = exxon22) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon23) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon24) + geom_point(mapping = aes(equity, Date))

## eighth analysis
exxon25 <- my_data %>% slice(5271:5270)
exxon26 <- my_data %>% slice(5271:5266)
exxon27 <- my_data %>% slice(5271:5261)

## one day analysis
ggplot(data = exxon25) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon26) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon27) + geom_point(mapping = aes(equity, Date))

## nineth analysis
exxon28 <- my_data %>% slice(5419:5418)
exxon29 <- my_data %>% slice(5419:5414)
exxon30 <- my_data %>% slice(5419:5409)

## one day analysis
ggplot(data = exxon28) + geom_point(mapping = aes(equity, Date))

## five days analysis 
ggplot(data = exxon29) + geom_point(mapping = aes(equity, Date))

## ten days analysis
ggplot(data = exxon30) + geom_point(mapping = aes(equity, Date))

## Testing correlations 
## Creating a new column in the dataframe 

my_data$greenwashing <- my_data$press
my_data$greenwashing[is.na(my_data$greenwashing)] = 0 
  
cor.test(my_data$greenwashing, my_data$equity, method = "pearson")
