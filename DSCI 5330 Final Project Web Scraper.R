### Liam Frank
### DSCI 5340 Final Project Web Scraping
### 11/11/2023

library(dplyr)
library(rvest)
library(robotstxt)
library(glue)
library(tidyverse)

##########################################################################################

# scraping allowed?
paths_allowed("https://www.fifplay.com/fifa-23/players/?page=1")

# getting each page link
root <- "https://www.fifplay.com/fifa-23/players/?page="
numbers <- seq(from = 1, to = 372, by = 1)
urls <- glue("{root}{numbers}")

# scraping each page for href attributes to each individual player page
scrape_page1 <- function(url){
  page <- read_html(url)
  namelink = page %>% html_nodes(".name a") %>% html_attr("href")
  tibble(namelink)
}

names_df <- map_dfr(urls, scrape_page1)



# scraping each individual player page for ratings 

# This is the section I was running into errors in 

scrape_page2 <- function(url){
  prac <- read_html(url)
  pname = prac %>% html_nodes(".margin-bottom-3px") %>% html_text()
  cardtype = prac %>% html_nodes(".font-lighter") %>% html_text()
  position = prac %>% html_nodes(".playercard-position") %>% html_text()
  age = prac %>% html_nodes(".margin-bottom+ .col-xs-12 tr:nth-child(2) .bold") %>% html_text()
  rating = prac %>% html_nodes(".margin-bottom+ .col-xs-12 tr:nth-child(3) .bold") %>% html_text()
  club = prac %>% html_nodes("tr:nth-child(4) .bold a") %>% html_text()
  league = prac %>% html_nodes("tr:nth-child(5) .bold a") %>% html_text()
  nation = prac %>% html_nodes("tr:nth-child(6) a") %>% html_text()
  heightandweight = prac %>% html_nodes("tr:nth-child(1) .alignright") %>% html_text()
  foot = prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(2) .bold") %>% html_text()
  weakfoot = prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(3) .bold") %>% html_text()
  attackworkrate = prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(4) .bold") %>% html_text()
  defenseworkrate = prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(5) .bold") %>% html_text()
  pace = prac %>% html_nodes(".col-xs-12:nth-child(1) thead .alignright") %>% html_text()
  dribbling = prac %>% html_nodes(".col-xs-12:nth-child(2) thead .alignright") %>% html_text()
  shooting = prac %>% html_nodes(".col-xs-12:nth-child(3) thead .alignright") %>% html_text()
  defending = prac %>% html_nodes("thead .bg-red .alignright") %>% html_text()
  passing = prac %>% html_nodes(".col-xs-12:nth-child(5) thead .alignright") %>% html_text()
  physical = prac %>% html_nodes(".col-xs-12:nth-child(6) thead .alignright") %>% html_text()
  print(pname)
  tibble(pname,
         cardtype,
         position,
         age,
         rating,
         club,
         league,
         nation,
         heightandweight,
         foot,
         weakfoot,
         attackworkrate,
         defenseworkrate,
         pace,
         dribbling,
         shooting,
         defending,
         passing,
         physical)
}


fifa_df2 <- map_dfr(names_df$namelink, scrape_page2)
names_dfp <- names_df[1:10,]
names_df2 <- names_dfp$namelink
View(names_df2)



################################################################################################
## First website that did not contain price data for each card

get_player = function(link) {
  prac <- read_html(link)
  pnameel <-  prac %>% html_nodes(".margin-bottom-3px")
  if (length(pnameel) == 0){
    pname <- "NA"
  }else {
    pname <- html_text(pnameel)
  }
  print(pname)
  return(pname)
}

get_cardtype = function(link) {
  prac <- read_html(link)
  cardtypeel <-  prac %>% html_nodes(".font-lighter")
  if (length(cardtypeel) == 0){
    cardtype <- "NA"
  }else {
    cardtype <- html_text(cardtypeel)
  }
  print(cardtype)
  return(cardtype)
}

get_position = function(link) {
  prac <- read_html(link)
  positionel <-  prac %>% html_nodes(".playercard-position")
  if (length(positionel) == 0){
    position <- "NA"
  }else {
    position <- html_text(positionel)
  }
  print(position)
  return(position)
}

get_age = function(link) {
  prac <- read_html(link)
  ageel <-  prac %>% html_nodes(".margin-bottom+ .col-xs-12 tr:nth-child(2) .bold")
  if (length(ageel) == 0){
    age <- "NA"
  }else {
    age <- html_text(ageel)
  }
  print(age)
  return(age)
}

get_rating = function(link) {
  prac <- read_html(link)
  ratingel <-  prac %>% html_nodes(".margin-bottom+ .col-xs-12 tr:nth-child(3) .bold")
  if (length(ratingel) == 0){
    rating <- "NA"
  }else {
    rating <- html_text(ratingel)
  }
  print(rating)
  return(rating)
}

get_club = function(link) {
  prac <- read_html(link)
  clubel <-  prac %>% html_nodes("tr:nth-child(4) .bold a")
  if (length(clubel) == 0){
    club <- "NA"
  }else {
    club <- html_text(clubel)
  }
  print(club)
  return(club)
}

get_league = function(link) {
  prac <- read_html(link)
  leagueel <-  prac %>% html_nodes("tr:nth-child(5) .bold a")
  if (length(leagueel) == 0){
    league <- "NA"
  }else {
    league <- html_text(leagueel)
  }
  print(league)
  return(league)
}

get_nation = function(link) {
  prac <- read_html(link)
  nationel <-  prac %>% html_nodes("#first-row tr:nth-child(6) a")
  if (length(nationel) == 0){
    nation <- "NA"
  }else {
    nation <- html_text(nationel)
  }
  print(nation)
  return(nation)
}

get_heightandweight = function(link) {
  prac = read_html(link)
  hwel <-  prac %>% html_nodes("#first-row tr:nth-child(1) .alignright")
  if (length(hwel) == 0){
    heightandweight <- "NA"
  }else {
    heightandweight <- html_text(hwel)
  }
  print(heightandweight)
  return(heightandweight)
}

get_foot = function(link) {
  prac <- read_html(link)
  footel <-  prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(2) .bold")
  if (length(footel) == 0){
    foot <- "NA"
  }else {
    foot <- html_text(footel)
  }
  print(foot)
  return(foot)
}

get_weakfoot = function(link) {
  prac <- read_html(link)
  wfootel <-  prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(3) .bold")
  if (length(wfootel) == 0){
    weakfoot <- "NA"
  }else {
    weakfoot <- html_text(wfootel)
  }
  print(weakfoot)
  return(weakfoot)
}

get_attackworkrate = function(link) {
  prac <- read_html(link)
  atel <-  prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(4) .bold")
  if (length(atel) == 0){
    attackworkrate <- "NA"
  }else {
    attackworkrate <- html_text(atel)
  }
  print(attackworkrate)
  return(attackworkrate)
}

get_defenseworkrate = function(link) {
  prac <- read_html(link)
  dfel <-  prac %>% html_nodes(".margin-bottom~ .col-xs-12+ .col-xs-12 tr:nth-child(5) .bold")
  if (length(dfel) == 0){
    defenseworkrate <- "NA"
  }else {
    defenseworkrate <- html_text(dfel)
  }
  print(defenseworkrate)
  return(defenseworkrate)
}

get_pace = function(link) {
  prac <- read_html(link)
  paceel <-  prac %>% html_nodes(".col-xs-12:nth-child(1) thead .alignright")
  if (length(paceel) == 0){
    pace <- "NA"
  }else {
    pace <- html_text(paceel)
  }
  print(pace)
  return(pace)
}

get_dribbling = function(link) {
  prac <- read_html(link)
  dbel <-  prac %>% html_nodes(".col-xs-12:nth-child(2) thead .alignright")
  if (length(dbel) == 0){
    dribbling <- "NA"
  }else {
    dribbling <- html_text(dbel)
  }
  print(dribbling)
  return(dribbling)
}

get_shooting = function(link) {
  prac <- read_html(link)
  shel <-  prac %>% html_nodes(".col-xs-12:nth-child(3) thead .alignright")
  if (length(shel) == 0){
    shooting <- "NA"
  }else {
    shooting <- html_text(shel)
  }
  print(shooting)
  return(shooting)
}

get_defending = function(link) {
  prac <- read_html(link)
  dfel <-  prac %>% html_nodes(".col-xs-12:nth-child(4) thead .alignright")
  if (length(dfel) == 0){
    defending <- "NA"
  }else {
    defending <- html_text(dfel)
  }
  print(defending)
  return(defending)
}

get_passing = function(link) {
  prac <- read_html(link)
  pael <-  prac %>% html_nodes(".col-xs-12:nth-child(5) thead .alignright")
  if (length(pael) == 0){
    passing <- "NA"
  }else {
    passing <- html_text(pael)
  }
  print(passing)
  return(passing)
}

get_physical = function(link) {
  prac <- read_html(link)
  phel <-  prac %>% html_nodes(".col-xs-12:nth-child(6) thead .alignright")
  if (length(phel) == 0){
    physical <- "NA"
  }else {
    physical <- html_text(phel)
  }
  print(physical)
  return(physical)
}



fifa_df2 = data.frame()

for (page_result in seq(from = 1, to = 20, by = 1)) {
  link = paste0("https://www.fifplay.com/fifa-23/players/?page=", 
                page_result)
  
  page = read_html(link)
  
  player_links = page %>% html_nodes(".name a") %>% html_attr("href")
  
  playername = page %>% html_nodes(".name a") %>% html_text()
  
  skills = page %>% html_nodes("#skill-moves .stats") %>% html_text()
  
  player = sapply(player_links, FUN = get_player, USE.NAMES = FALSE)
  
  cardtype = sapply(player_links, FUN = get_cardtype, USE.NAMES = FALSE)
  
  position = sapply(player_links, FUN = get_position, USE.NAMES = FALSE)
  
  age = sapply(player_links, FUN = get_age, USE.NAMES = FALSE)
  
  rating = sapply(player_links, FUN = get_rating, USE.NAMES = FALSE)
  
  club = sapply(player_links, FUN = get_club, USE.NAMES = FALSE)
  
  league = sapply(player_links, FUN = get_league, USE.NAMES = FALSE)
  
  nation = sapply(player_links, FUN = get_nation, USE.NAMES = FALSE)
  
  heightandweight = sapply(player_links, FUN = get_heightandweight, USE.NAMES = FALSE)
  
  foot = sapply(player_links, FUN = get_foot, USE.NAMES = FALSE)
  
  weakfoot = sapply(player_links, FUN = get_weakfoot, USE.NAMES = FALSE)
  
  attackworkrate = sapply(player_links, FUN = get_attackworkrate, USE.NAMES = FALSE)
  
  defenseworkrate = sapply(player_links, FUN = get_defenseworkrate, USE.NAMES = FALSE)
  
  pace = sapply(player_links, FUN = get_pace, USE.NAMES = FALSE)
  
  dribbling = sapply(player_links, FUN = get_dribbling, USE.NAMES = FALSE)
  
  shooting = sapply(player_links, FUN = get_shooting, USE.NAMES = FALSE)
  
  defending = sapply(player_links, FUN = get_defending, USE.NAMES = FALSE)
  
  passing = sapply(player_links, FUN = get_passing, USE.NAMES = FALSE)
  
  physical = sapply(player_links, FUN = get_physical, USE.NAMES = FALSE)
  
  fifa_df2 = rbind(fifa_df2, data.frame(player,
                                      playername,
                                      skills,
                                      cardtype,
                                      position,
                                      age, 
                                      rating, 
                                      club, 
                                      league,
                                      nation, 
                                      heightandweight,
                                      foot, 
                                      weakfoot,
                                      attackworkrate,
                                      defenseworkrate,
                                      pace,
                                      dribbling,
                                      shooting,
                                      defending,
                                      passing,
                                      physical,
                                      stringsAsFactors = FALSE))
}


write.csv(fifa_df2, "fifa_df2.csv")
tidy_fifa <- read.csv("fifa_df2.csv")

View(tidy_fifa)
View(fifa_df2)

#########################################################################
## Second Website that contained price data for each card 

get_player = function(link) {
  prac <- read_html(link)
  pname = prac %>% html_nodes("tr:nth-child(1) .table-row-text") %>% html_text()
  print(pname)
  return(pname)
}

get_cardtype = function(link) {
  prac <- read_html(link)
  cardtype = prac %>% html_nodes("tr:nth-child(2) .table-row-text") %>% html_text()
  print(cardtype)
  return(cardtype)
}

get_position = function(link) {
  prac <- read_html(link)
  position = prac %>% html_nodes(".pcdisplay-pos") %>% html_text()
  print(position)
  return(position)
}

get_age = function(link) {
  prac <- read_html(link)
  age = prac %>% html_nodes(".info_tr_1 .table-row-text") %>% html_text()
  print(age)
  return(age)
}

get_rating = function(link) {
  prac <- read_html(link)
  rating = prac %>% html_nodes(".pcdisplay-rat") %>% html_text()
  print(rating)
  return(rating)
}

get_club = function(link) {
  prac <- read_html(link)
  club = prac %>% html_nodes("tr:nth-child(3) .table-row-text") %>% html_text()
  print(club)
  return(club)
}

get_league = function(link) {
  prac <- read_html(link)
  league = prac %>% html_nodes("tr:nth-child(5) .table-row-text") %>% html_text()
  print(league)
  return(league)
}

get_nation = function(link) {
  prac <- read_html(link)
  nation = prac %>% html_nodes("tr:nth-child(4) .table-row-text") %>% html_text()
  print(nation)
  return(nation)
}

get_height = function(link) {
  prac <- read_html(link)
  height = prac %>% html_nodes("tr:nth-child(10) .table-row-text") %>% html_text()
  print(height)
  return(height)
}

get_weight = function(link) {
  prac <- read_html(link)
  weight = prac %>% html_nodes("tr:nth-child(11) .table-row-text") %>% html_text()
  print(weight)
  return(weight)
}

get_foot = function(link) {
  prac <- read_html(link)
  foot = prac %>% html_nodes("tr:nth-child(9) .table-row-text") %>% html_text()
  print(foot)
  return(foot)
}

get_weakfoot = function(link) {
  prac <- read_html(link)
  weakfoot = prac %>% html_nodes("tr:nth-child(7) .table-row-text") %>% html_text()
  print(weakfoot)
  return(weakfoot)
}

get_attackworkrate = function(link) {
  prac <- read_html(link)
  attackworkrate = prac %>% html_nodes("tr:nth-child(13) .table-row-text") %>% html_text()
  print(attackworkrate)
  return(attackworkrate)
}

get_defenseworkrate = function(link) {
  prac <- read_html(link)
  defenseworkrate = prac %>% html_nodes("tr:nth-child(14) .table-row-text") %>% html_text()
  print(defenseworkrate)
  return(defenseworkrate)
}

get_pace = function(link) {
  prac <- read_html(link)
  pace = prac %>% html_nodes("#main-pace-val-0 .stat_val") %>% html_text()
  print(pace)
  return(pace)
}

get_dribbling = function(link) {
  prac <- read_html(link)
  dribbling = prac %>% html_nodes("#main-dribblingp-val-0 .stat_val") %>% html_text()
  print(dribbling)
  return(dribbling)
}

get_shooting = function(link) {
  prac <- read_html(link)
  shooting = prac %>% html_nodes("#main-shooting-val-0 .stat_val") %>% html_text()
  print(shooting)
  return(shooting)
}

get_defending = function(link) {
  prac <- read_html(link)
  defending = prac %>% html_nodes("#main-defending-val-0 .stat_val") %>% html_text()
  print(defending)
  return(defending)
}

get_passing = function(link) {
  prac <- read_html(link)
  passing = prac %>% html_nodes("#main-passing-val-0 .stat_val") %>% html_text()
  print(passing)
  return(passing)
}

get_physical = function(link) {
  prac <- read_html(link)
  physical = prac %>% html_nodes("#main-heading-val-0 .stat_val") %>% html_text()
  print(physical)
  return(physical)
}

get_skills = function(link) {
  prac <- read_html(link)
  skills = prac %>% html_nodes("tr:nth-child(6) .table-row-text") %>% html_text()
  print(skills)
  return(skills)
}



fifa_df3 = data.frame()

for (page_result in seq(from = 1, to = 34, by = 1)) {
  link = paste0("https://www.futbin.com/23/players?page=", 
                page_result)
  
  page = read_html(link)
  
  player_links = page %>% html_nodes(".get-tp") %>% html_attr("href") %>% paste0("https://www.futbin.com/",.)
  
  pname1 = page %>% html_nodes(".get-tp") %>% html_text()
  print(pname1)
  
  Sys.sleep(30)
  
  pprice = page %>% html_nodes("span.font-weight-bold") %>% html_text()
  print(pprice)
  
  Sys.sleep(30)
  
  player = sapply(player_links, FUN = get_player, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  cardtype = sapply(player_links, FUN = get_cardtype, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  position = sapply(player_links, FUN = get_position, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  age = sapply(player_links, FUN = get_age, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  rating = sapply(player_links, FUN = get_rating, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  club = sapply(player_links, FUN = get_club, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  league = sapply(player_links, FUN = get_league, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  nation = sapply(player_links, FUN = get_nation, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  height = sapply(player_links, FUN = get_height, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  weight = sapply(player_links, FUN = get_weight, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  foot = sapply(player_links, FUN = get_foot, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  weakfoot = sapply(player_links, FUN = get_weakfoot, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  attackworkrate = sapply(player_links, FUN = get_attackworkrate, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  defenseworkrate = sapply(player_links, FUN = get_defenseworkrate, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  pace = sapply(player_links, FUN = get_pace, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  dribbling = sapply(player_links, FUN = get_dribbling, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  shooting = sapply(player_links, FUN = get_shooting, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  defending = sapply(player_links, FUN = get_defending, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  passing = sapply(player_links, FUN = get_passing, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  physical = sapply(player_links, FUN = get_physical, USE.NAMES = FALSE)
  
  Sys.sleep(30)
  
  skills = sapply(player_links, FUN = get_skills, USE.NAMES = FALSE)
  
  Sys.sleep(120)
  
  
  fifa_df3 = rbind(fifa_df3, data.frame(player,
                                      pname1,
                                      pprice,
                                      cardtype,
                                      position,
                                      age, 
                                      rating, 
                                      club, 
                                      league,
                                      nation, 
                                      height,
                                      weight,
                                      foot, 
                                      weakfoot,
                                      skills,
                                      attackworkrate,
                                      defenseworkrate,
                                      pace,
                                      dribbling,
                                      shooting,
                                      defending,
                                      passing,
                                      physical,
                                      stringsAsFactors = FALSE))
}

write.csv(fifa_df3, "fifa_df3.csv")
tidy_fifaprice <- read.csv("fifa_df3.csv")
View(tidy_fifaprice)

###############################################################################

fifa <- read.csv("fifa_df3.csv")
View(fifa)

library(dplyr)
library(tidyverse)
library(tidyr)

## Removing the 7 observations where the recorded data was not formatted correctly  

fifa_tidy <- fifa %>% filter(trimws(foot) %in% c("Right","Left"))
View(fifa_tidy)
nrow(fifa_tidy)
nrow(fifa)

## Removing whitespace from all observations

fifa_tidy <- fifa_tidy %>% mutate_all(trimws)
View(fifa_tidy)

## Getting height in cm 

fifa_tidy$height <- parse_number(fifa_tidy$height)
View(fifa_tidy)

## Adding labels to height and weight

names(fifa_tidy)[names(fifa_tidy) == "height"] <- "height cm"
names(fifa_tidy)[names(fifa_tidy) == "weight"] <- "weight kg"
View(fifa_tidy)

## Changing more variable names

names(fifa_tidy)[names(fifa_tidy) == "pprice"] <- "price"
names(fifa_tidy)[names(fifa_tidy) == "pname1"] <- "playername"
names(fifa_tidy)[names(fifa_tidy) == "cardtype"] <- "attribute"

## Converting age to standardized format

standardize_age <- function(age) {
  if (grepl("\\d{2}-\\d{2}-\\d{4}", age)) {
    birth_date <- as.Date(age, format = "%d-%m-%Y")
    age_in_years <- as.numeric(difftime(Sys.Date(), birth_date, units = "days") / 365)
    return(paste(round(age_in_years), "years old"))
  } else {
    return(age)
  }
}

fifa_tidy$age <- sapply(fifa_tidy$age, standardize_age)
fifa_tidy$age <- parse_number(fifa_tidy$age)
View(fifa_tidy)
max(fifa_tidy$age)

## Separating price variable label

fifa_tidy <- separate(data = fifa_tidy, col = price, into = c("value", "label"), sep = -1)

## Converting Price to integer

fifa_tidy <- fifa_tidy %>% 
  mutate(value = case_when(
    label == "M" ~ as.numeric(value) * 1e+06,
    label == "K" ~ as.numeric(value) * 1e+03,
    TRUE ~ as.numeric(value)
  ))
  
  
## Deleting label column

fifa_tidy <- fifa_tidy[, -which(names(fifa_tidy) == "label")]

## Converting a 0 in price to an NA value 

fifa_tidy$price[fifa_tidy$price == 0] <- NA

## Imputing missing price data 

fifa_tidy <- fifa_tidy %>% 
  group_by(rating, league) %>%
  mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value))

fifa_tidy <- fifa_tidy %>% filter(value != "NaN")  

View(fifa_tidy)
View(fifa)
  
  