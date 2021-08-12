

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

# ten day analysis 
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
  "02/26/20",   126.64, "JPMorgan", 
  "02/27/20",   121.37, "JPMorgan" 
)

## preparing data for the graph
jpppp1 <- j_pppp1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## five days 
jpppp2 <- my_data %>% slice(361:356)
j_pppp2 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/26/20",   126.64, "JPMorgan", 
  "02/27/20",   121.37, "JPMorgan",
  "02/28/20",   116.11, "JPMorgan",
  "03/02/20",   121.52, "JPMorgan", 
  "03/03/20",   116.96, "JPMorgan",
  "03/04/20",   119.85, "JPMorgan"
)

## preparing data for the graph
jpppp2 <- j_pppp2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## ten days
jpppp3 <- my_data %>% slice(361:351)
j_pppp3 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/26/20",   126.64, "JPMorgan", 
  "02/27/20",   121.37, "JPMorgan",
  "02/28/20",   116.11, "JPMorgan",
  "03/02/20",   121.52, "JPMorgan", 
  "03/03/20",   116.96, "JPMorgan",
  "03/04/20",   119.85, "JPMorgan", 
  "03/05/20",   113.97, "JPMorgan",
  "03/06/20",   108.08, "JPMorgan",
  "03/09/20",    93.44, "JPMorgan",
  "03/10/20",   100.70, "JPMorgan",
  "03/11/20",    95.96, "JPMorgan"
)

## preparing data for the graph
jpppp3 <- j_pppp3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


# fifth analysis
jppppp1 <- my_data %>% slice(365:364)
j_ppppp1 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/20/20",   137.49, "JPMorgan", 
  "02/21/20",   135.81, "JPMorgan" 
)

## preparing data for the graph
jppppp1 <- j_ppppp1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-20", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppppp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

jppppp2 <- my_data %>% slice(365:360)
j_ppppp2 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/20/20",   137.49, "JPMorgan", 
  "02/21/20",   135.81, "JPMorgan", 
  "02/24/20",   132.16, "JPMorgan", 
  "02/25/20",   126.26, "JPMorgan",
  "02/26/20",   126.64, "JPMorgan",
  "02/27/20",   121.37, "JPMorgan"
)

## preparing data for the graph
jppppp2 <- j_ppppp2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-20", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppppp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

jppppp3 <- my_data %>% slice(365:355)
j_ppppp3 <- tribble(
  ~Date,   ~equity, ~company,  
  "02/20/20",   137.49, "JPMorgan", 
  "02/21/20",   135.81, "JPMorgan", 
  "02/24/20",   132.16, "JPMorgan", 
  "02/25/20",   126.26, "JPMorgan",
  "02/26/20",   126.64, "JPMorgan",
  "02/27/20",   121.37, "JPMorgan", 
  "02/28/20",   116.11, "JPMorgan",
  "03/02/20",   121.52, "JPMorgan",
  "03/03/20",   116.96, "JPMorgan",
  "03/04/20",   119.85, "JPMorgan",
  "03/05/20",   113.97, "JPMorgan"
)

## preparing data for the graph
jppppp3 <- j_ppppp3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-02-20", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jppppp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

# sixth analysis
## one day analysis 
jpppppp1 <- my_data %>% slice(934:933)
j_pppppp1 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/13/17",   97.86, "JPMorgan", 
  "11/14/17",   97.27, "JPMorgan" 
)

## preparing data for the graph
jpppppp1 <- j_pppppp1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2017-11-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppppp1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## five day analysis
jpppppp2 <- my_data %>% slice(934:929)
j_pppppp2 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/13/17",   97.86, "JPMorgan", 
  "11/14/17",   97.27, "JPMorgan",
  "11/15/17",   98.19, "JPMorgan", 
  "11/16/17",   98.47, "JPMorgan",
  "11/17/17",   98.14, "JPMorgan", 
  "11/20/17",   99.01, "JPMorgan"
)

## preparing data for the graph
jpppppp2 <- j_pppppp2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2017-11-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppppp2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## ten day analysis 
jpppppp3 <- my_data %>% slice(934:924)
j_pppppp3 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/13/17",   97.86, "JPMorgan", 
  "11/14/17",   97.27, "JPMorgan",
  "11/15/17",   98.19, "JPMorgan", 
  "11/16/17",   98.47, "JPMorgan",
  "11/17/17",   98.14, "JPMorgan", 
  "11/20/17",   99.01, "JPMorgan", 
  "11/21/17",   98.93, "JPMorgan",
  "11/22/17",   98.64, "JPMorgan",
  "11/24/17",   98.32, "JPMorgan",
  "11/27/17",   97.93, "JPMorgan",
  "11/28/17",  101.36, "JPMorgan"
)

## preparing data for the graph
jpppppp3 <- j_pppppp3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2017-11-13", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = jpppppp3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


# shell analysis

## first analysis
### one day
shell1 <- my_data %>% slice(2457:2456)
shell_1 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21", 15.888, "Shell", 
  "05/28/21", 15.828, "Shell" 
)

## preparing data for the graph
shell1 <- shell_1 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell1, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### five days
shell2 <- my_data %>% slice(2457:2452)
shell_2 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21", 15.888, "Shell", 
  "05/28/21", 15.828, "Shell",
  "05/31/21", 15.684, "Shell",
  "06/01/21", 16.094, "Shell",
  "06/02/21", 16.460, "Shell",
  "06/03/21", 16.488, "Shell"
)

## preparing data for the graph
shell2 <- shell_2 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell2, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

shell3 <- my_data %>% slice(2457:2447)
shell_3 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/27/21", 15.888, "Shell", 
  "05/28/21", 15.828, "Shell",
  "05/31/21", 15.684, "Shell",
  "06/01/21", 16.094, "Shell",
  "06/02/21", 16.460, "Shell",
  "06/03/21", 16.488, "Shell", 
  "06/04/21", 16.394, "Shell",
  "06/07/21", 16.350, "Shell",
  "06/08/21", 16.256, "Shell",
  "06/09/21", 16.428, "Shell",
  "06/10/21", 16.576, "Shell"
)

## preparing data for the graph
shell3 <- shell_3 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-27", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell3, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## second analysis
### one day analysis 
shell4 <- my_data %>% slice(2458:2457)
shell_4 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/26/21", 16.094, "Shell",
  "05/27/21", 15.888, "Shell"
)

## preparing data for the graph
shell4 <- shell_4 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell4, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

shell5 <- my_data %>% slice(2458:2453)
shell_5 <- tribble(
  ~Date,   ~equity, ~company,
  "05/26/21", 16.094, "Shell",
  "05/27/21", 15.888, "Shell", 
  "05/28/21", 15.828, "Shell",
  "05/31/21", 15.684, "Shell",
  "06/01/21", 16.094, "Shell",
  "06/02/21", 16.460, "Shell"
)

## preparing data for the graph
shell5 <- shell_5 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell5, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis 
shell6 <- my_data %>% slice(2458:2448)
shell_6 <- tribble(
  ~Date,   ~equity, ~company,  
  "05/26/21", 16.094, "Shell",
  "05/27/21", 15.888, "Shell", 
  "05/28/21", 15.828, "Shell",
  "05/31/21", 15.684, "Shell",
  "06/01/21", 16.094, "Shell",
  "06/02/21", 16.460, "Shell",
  "06/03/21", 16.488, "Shell", 
  "06/04/21", 16.394, "Shell",
  "06/07/21", 16.350, "Shell",
  "06/08/21", 16.256, "Shell",
  "06/09/21", 16.428, "Shell"
)

## preparing data for the graph
shell6 <- shell_6 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-05-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell6, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## third analysis
shell7 <- my_data %>% slice(2480:2479)
shell_7 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell"
)

## preparing data for the graph
shell7 <- shell_7 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell7, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### five day analysis 
shell8 <- my_data %>% slice(2480:2475)
shell_8 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell", 
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell", 
  "04/30/21", 15.840, "Shell",
  "05/03/21", 16.000, "Shell"
)

## preparing data for the graph
shell8 <- shell_8 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell8, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

shell9 <- my_data %>% slice(2480:2470)
shell_9 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell", 
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell", 
  "04/30/21", 15.840, "Shell",
  "05/03/21", 16.000, "Shell",
  "05/04/21", 16.126, "Shell",
  "05/05/21", 16.494, "Shell",
  "05/06/21", 16.510, "Shell",
  "05/07/21", 16.612, "Shell",
  "05/10/21", 16.650, "Shell"
)

## preparing data for the graph
shell9 <- shell_9 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-26", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell9, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


## fourth analysis
shell10 <- my_data %>% slice(2482:2481)
shell_10 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell"
)

## preparing data for the graph
shell10 <- shell_10 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-22", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## ploting the graph
ggplot(data = shell10, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## five day analysis 
shell11 <- my_data %>% slice(2482:2477)
shell_11 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell", 
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell",
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell"
)

## preparing data for the graph
shell11 <- shell_11 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-22", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell11, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis 
shell12 <- my_data %>% slice(2482:2472)
shell_12 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell", 
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell",
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell", 
  "04/30/21", 15.840, "Shell",
  "05/03/21", 16.000, "Shell",
  "05/04/21", 16.126, "Shell",
  "05/05/21", 16.494, "Shell",
  "05/06/21", 16.510, "Shell",
)

## preparing data for the graph
shell12 <- shell_12 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-22", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell12, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## fifth analysis
shell13 <- my_data %>% slice(2485:2484)
shell_13 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/19/21", 16.386, "Shell",
  "04/20/21", 15.788, "Shell"
)

## preparing data for the graph
shell13 <- shell_13 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-19", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell13, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### five day analysis 
shell14 <- my_data %>% slice(2485:2480)
shell_14 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/19/21", 16.386, "Shell",
  "04/20/21", 15.788, "Shell", 
  "04/21/21", 15.974, "Shell",
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell", 
  "04/26/21", 15.906, "Shell",
)

## preparing data for the graph
shell14 <- shell_14 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-19", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell14, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis 
shell15 <- my_data %>% slice(2485:2475)
shell_15 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/19/21", 16.386, "Shell",
  "04/20/21", 15.788, "Shell", 
  "04/21/21", 15.974, "Shell",
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell", 
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell",
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell", 
  "04/30/21", 15.840, "Shell",
  "05/03/21", 16.000, "Shell"
)

## preparing data for the graph
shell15 <- shell_15 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-19", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell15, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## sixth analysis
### one day analysis
shell16 <- my_data %>% slice(2487:2486)
shell_16 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/15/21", 16.636, "Shell",
  "04/16/21", 16.444, "Shell"
)

## preparing data for the graph
shell16 <- shell_16 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-15", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell16, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### five day analysis 
shell17 <- my_data %>% slice(2487:2482)
shell_17 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/15/21", 16.636, "Shell",
  "04/16/21", 16.444, "Shell",
  "04/19/21", 16.386, "Shell",
  "04/20/21", 15.788, "Shell", 
  "04/21/21", 15.974, "Shell",
  "04/22/21", 15.862, "Shell",
)

## preparing data for the graph
shell17 <- shell_17 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-15", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell17, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis
shell18 <- my_data %>% slice(2487:2477)
shell_18 <- tribble(
  ~Date,   ~equity, ~company,  
  "04/15/21", 16.636, "Shell",
  "04/16/21", 16.444, "Shell",
  "04/19/21", 16.386, "Shell",
  "04/20/21", 15.788, "Shell", 
  "04/21/21", 15.974, "Shell",
  "04/22/21", 15.862, "Shell",
  "04/23/21", 15.720, "Shell", 
  "04/26/21", 15.906, "Shell",
  "04/27/21", 15.770, "Shell",
  "04/28/21", 16.078, "Shell",
  "04/29/21", 15.904, "Shell", 
)

## preparing data for the graph
shell18 <- shell_18 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2021-04-15", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell18, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## seventh analysis
shell19 <- my_data %>% slice(2581:2580)
shell_19 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/30/20", 14.386, "Shell",
  "12/01/20", 14.728, "Shell"
)

## preparing data for the graph
shell19 <- shell_19 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-11-30", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell19, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

shell20 <- my_data %>% slice(2581:2576)
shell_20 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/30/20", 14.386, "Shell",
  "12/01/20", 14.728, "Shell",
  "12/02/20", 15.134, "Shell",
  "12/03/20", 15.256, "Shell",
  "12/04/20", 15.782, "Shell",
  "12/07/20", 15.592, "Shell"
)

## preparing data for the graph
shell20 <- shell_20 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-11-30", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell20, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis 
shell21 <- my_data %>% slice(2581:2571)
shell_21 <- tribble(
  ~Date,   ~equity, ~company,  
  "11/30/20", 14.386, "Shell",
  "12/01/20", 14.728, "Shell",
  "12/02/20", 15.134, "Shell",
  "12/03/20", 15.256, "Shell",
  "12/04/20", 15.782, "Shell",
  "12/07/20", 15.592, "Shell",
  "12/08/20", 15.528, "Shell",
  "12/09/20", 15.512, "Shell",
  "12/10/20", 15.936, "Shell",
  "12/11/20", 15.414, "Shell",
  "12/14/20", 15.100, "Shell",
  "12/15/20", 15.284, "Shell"
)

## preparing data for the graph
shell21 <- shell_21 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2020-11-30", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell21, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)


## eighth analysis
shell22 <- my_data %>% slice(4685:4684)
shell_22 <- tribble(
       ~Date,~equity, ~company,  
  "09/05/12", 27.565, "Shell",
  "09/06/12", 28.045, "Shell"
)

## preparing data for the graph
shell22 <- shell_22 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2012-09-05", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell22, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

## five day analysis
shell23 <- my_data %>% slice(4685:4680)
shell_23 <- tribble(
  ~Date,~equity, ~company,  
  "09/05/12", 27.565, "Shell",
  "09/06/12", 28.045, "Shell",
  "09/07/12", 27.940, "Shell",
  "09/10/12", 27.905, "Shell", 
  "09/11/12", 27.950, "Shell",
  "09/12/12", 27.845, "Shell"
)

## preparing data for the graph
shell23 <- shell_23 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2012-09-05", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell23, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)

### ten day analysis 
shell24 <- my_data %>% slice(4685:4675)
shell_24 <- tribble(
  ~Date,~equity, ~company,  
  "09/05/12", 27.565, "Shell",
  "09/06/12", 28.045, "Shell",
  "09/07/12", 27.940, "Shell",
  "09/10/12", 27.905, "Shell", 
  "09/11/12", 27.950, "Shell",
  "09/12/12", 27.845, "Shell", 
  "09/13/12", 28.135, "Shell",
  "09/14/12", 27.925, "Shell",
  "09/17/12", 27.910, "Shell", 
  "09/18/12", 27.850, "Shell",
  "09/19/12", 27.830, "Shell"
)

## preparing data for the graph
shell24 <- shell_24 %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(highlight = ifelse(Date == "2012-09-05", "Greenwashing Press Release", "Normal"))

my_colors <- c("Greenwashing Press Release" = "red", "Normal" = "black")

## plotting the graph
ggplot(data = shell24, aes(x = Date, y = equity)) +
  geom_point(size = 3, aes(colour = highlight)) +
  scale_color_manual("Status", values = my_colors)
