library(tidyverse)
library(XML)
library(rvest)
library(stringr)

## This function works for all CPBL sesasons, but is preferred for completed seasons
## Please note that it only works for single seasons; enter year as YYYY format

CPBL_Old_Stats <- function(year) {
  
  bat_list <- list()
  pit_list <- list()
  
  ## This for loop scrapes raw data from the stats page on cpbl.com.tw
  
  for(i in 1:7) {
    
    bat_link <- paste0("http://www.cpbl.com.tw/stats/all.html?year=", year,
                       "&stat=pbat&online=0&sort=G&order=desc&per_page=", i)
    pit_link <- paste0("http://www.cpbl.com.tw/stats/all.html?year=", year,
                       "&stat=ppit&online=0&sort=G&order=desc&per_page=", i)
    
    bat_page <- read_html(bat_link)
    pit_page <- read_html(pit_link)
    
    ## The HTML link will give us the player ID and team ID
    
    bat_id <- data.frame(bat_page %>%
                           html_nodes(xpath = "//td/a") %>%
                           html_attr("href"))
    pit_id <- data.frame(pit_page %>%
                           html_nodes(xpath = "//td/a") %>%
                           html_attr("href"))
    
    colnames(bat_id) <- "link_id"
    colnames(pit_id) <- "link_id"
    
    ## Scraping the stats table
    
    bat_table <- readHTMLTable(bat_link, which = 1)
    pit_table <- readHTMLTable(pit_link, which = 1)
    
    bat_data <- cbind(bat_table, bat_id)
    pit_data <- cbind(pit_table, pit_id)
    
    assign(paste0("bat_", i), bat_data)
    assign(paste0("pit_", i), pit_data)
    
    bat_list[[i]] <- get(paste0("bat_", i))
    pit_list[[i]] <- get(paste0("pit_", i))
    
  }
  
  ## Collecting and combining the stats pages
  
  bat_stats <- do.call(rbind, bat_list)
  pit_stats <- do.call(rbind, pit_list)
  
  colnames(bat_stats)[2] <- "chi_name"
  colnames(pit_stats)[2] <- "chi_name"
  
  ## Manually fix a unique error from stats page (jersey number missing)
  
  bat_stats$chi_name <- as.character(bat_stats$chi_name)
  bat_stats[bat_stats$chi_name == "*  張建銘", "chi_name"] <- "66 張建銘"
  
  ## Downloading English team names and players names from database
  
  CPBL_names <- read_csv("https://raw.githubusercontent.com/anchengyoung/CPBL_Database/master/CPBL_Names.csv",
                        col_types = cols())
  CPBL_teams <- read_csv("https://raw.githubusercontent.com/anchengyoung/CPBL_Database/master/CPBL_Teams.csv",
                        col_types = cols())
  CPBL_names <- CPBL_names %>% select(-chi_name)
  CPBL_teams <- CPBL_teams %>% select(-team_name)
  
  ## Cleaning the data to desired formatting
  
  bat_stats <- bat_stats %>%
    mutate(chi_name = str_remove_all(chi_name, "[*#]"),
           chi_name = str_trim(chi_name),
           string = gsub("^[^=]*=", "", link_id),
           player_id = gsub("\\&.*", "", string),
           team_id = gsub("^[^=]*=", "", string)) %>%
    left_join(CPBL_teams, by = "team_id") %>%
    left_join(CPBL_names, by = "player_id") %>%
    mutate(team = team_name_en) %>%
    separate(chi_name, c("num", "chi_name"), sep = " ", extra = "merge") %>%
    mutate(chi_name = gsub("\\s", "", chi_name)) %>%
    select(-NUM, -link_id, -string, -team_id, -team_name_en) %>%
    select(num, chi_name, eng_name, team, everything())
  
  pit_stats <- pit_stats %>%
    mutate(chi_name = str_remove_all(chi_name, "[*#]"),
           chi_name = str_trim(chi_name),
           string = gsub("^[^=]*=", "", link_id),
           player_id = gsub("\\&.*", "", string),
           team_id = gsub("^[^=]*=", "", string)) %>%
    left_join(CPBL_teams, by = "team_id") %>%
    left_join(CPBL_names, by = "player_id") %>%
    mutate(team = team_name_en) %>%
    separate(chi_name, c("num", "chi_name"), sep = " ", extra = "merge") %>%
    mutate(chi_name = gsub("\\s", "", chi_name)) %>%
    select(-NUM, -link_id, -string, -team_id, -team_name_en) %>%
    select(num, chi_name, eng_name, team, everything())
  
  ## Gettings the standings to see how many games each team played
  
  standings_url <- paste0("http://www.cpbl.com.tw/en/standing/year/", year, ".html")
  
  test_table <- try(readHTMLTable(standings_url, which = 3), silent = TRUE)
  
  if (class(test_table) == "try-error") {
    standings <- readHTMLTable(standings_url, which = 1)
  } else {
    standings <- readHTMLTable(standings_url, which = 3)
  }
  
  ## Fixing the team name formatting
  
  standings$TEAM <- as.character(standings$TEAM)
  standings$TEAM[standings$TEAM == "SINON"] <- "Bulls"
  standings$TEAM[standings$TEAM == "U-Lions"] <- "Lions"
  
  standings <- standings %>%
    mutate(Team = ifelse(str_detect(TEAM, "^[:upper:]+$") == TRUE,
                         str_to_title(TEAM), TEAM),
           team = ifelse(str_sub(Team, start = -1) != "s",
                         paste0(Team, "s"), Team))
  
  standings$TEAM[standings$TEAM == "dmedia T-REXs"] <- "T-Rex"
  
  Team_G <- standings %>% select("team","G") %>% rename("TeamG" = "G")
  
  bat_stats <- bat_stats %>% left_join(Team_G, by = "team")
  pit_stats <- pit_stats %>% left_join(Team_G, by = "team")
  
  ## Fixing data formatting to desired class
  
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], asNumeric))
  
  pit_stats$IP <- as.character(pit_stats$IP)
  
  pit_stats <- pit_stats %>% 
    separate(IP, c("IP_F","IP_P"), sep = "\\.") 
  
  pit_stats$IP_F <- as.numeric(pit_stats$IP_F)
  pit_stats$IP_P <- as.numeric(pit_stats$IP_P)
  
  bat_stats <- factorsNumeric(bat_stats)
  pit_stats <- factorsNumeric(pit_stats)
  
  ## Setting qualifiers based on team games played
  
  bat_stats <- bat_stats %>%
    mutate(Q = ifelse(PA >= TeamG*3.1, "Q", NA))
  
  pit_stats <- pit_stats %>%
    mutate(IP_P = round(IP_P/3, 5), IP = IP_F + IP_P,
           Q = ifelse(IP >= TeamG, "Q", NA))
  
  pit_stats <- pit_stats %>%
    select(-IP_F,-IP_P,-ERA,-WHIP) %>%
    mutate(ERA = round(ER/IP*9, 5),
           WHIP = round((H+BB+IBB)/IP, 5))
  
  ## Calculating league stats for league-adjustment stats
  
  LeaguePA <- sum(bat_stats$PA)
  LeagueAB <- sum(bat_stats$AB)
  LeagueH <- sum(bat_stats$H)
  LeagueHR <- sum(bat_stats$HR)
  LeagueK <- sum(bat_stats$SO)
  LeagueBB <- sum(bat_stats$BB)
  LeagueIBB <- sum(bat_stats$IBB)
  LeagueHBP <- sum(bat_stats$HBP)
  LeagueSF <- sum(bat_stats$SF)
  LeagueTB <- sum(bat_stats$TB)
  
  LeagueAVG <- LeagueH/LeagueAB
  LeagueBABIP <- (LeagueH-LeagueHR)/(LeagueAB-LeagueK-LeagueHR+LeagueSF)
  LeagueOBP <- (LeagueH+LeagueBB+LeagueIBB+LeagueHBP)/(LeagueAB+LeagueBB+LeagueIBB+LeagueHBP+LeagueSF)
  LeagueSLG <- LeagueTB/LeagueAB
  LeagueISO <- LeagueSLG - LeagueAVG
  
  LeagueK_rate <- LeagueK/LeaguePA
  LeagueBB_rate <- (LeagueBB + LeagueIBB)/LeaguePA
  
  ## Creating league-adjusted stats and other advanced metrics
  
  bat_stats <- bat_stats %>% select(-AVG,-OBP,-SLG) %>%
    mutate(AVG = H/AB, "AVG+" = round(AVG/LeagueAVG*100),
           BABIP = (H-HR)/(AB-SO-HR+SF), "BABIP+" = round(BABIP/LeagueBABIP*100),
           OBP = (H+BB+IBB+HBP)/(AB+BB+IBB+HBP+SF), "OBP+" = round(OBP/LeagueOBP*100),
           SLG = TB/AB, "SLG+" = round(SLG/LeagueSLG*100),
           ISO = SLG-AVG, "ISO+" = round(ISO/LeagueISO*100),
           OPS = OBP+SLG, "OPS+" = round(((OBP/LeagueOBP)+(SLG/LeagueSLG)-1)*100),
           "K%" = round(SO/PA, 3), "BB%" = round((BB+IBB)/PA, 3),
           "K%+" = round(`K%`/LeagueK_rate*100), "BB%+" = round(`BB%`/LeagueBB_rate*100))
  
  bat_stats$AVG <- round(bat_stats$AVG, 3)
  bat_stats$BABIP <- round(bat_stats$BABIP, 3)
  bat_stats$OBP <- round(bat_stats$OBP, 3)
  bat_stats$SLG <- round(bat_stats$SLG, 3)
  bat_stats$ISO <- round(bat_stats$ISO, 3)
  bat_stats$OPS <- round(bat_stats$OPS, 3)
  
  bat_stats <- bat_stats %>%
    select(num,chi_name,eng_name,team,TeamG,Q,G,PA,AB,AVG,"AVG+",
           BABIP,"BABIP+",OBP,"OBP+",SLG,"SLG+",ISO,"ISO+",OPS,"OPS+",
           "K%","K%+","BB%","BB%+",everything()) %>% arrange(desc(PA))
  
  ## Doing the same things for pitchers' stats
  
  LeagueIP <- sum(pit_stats$IP)
  LeagueERA <- sum(pit_stats$ER)/LeagueIP*9
  LeagueHR <- sum(pit_stats$HR)
  LeagueK_BB_rate <- (LeagueK-(LeagueBB+LeagueIBB))/LeaguePA
  LeagueHR9 <- LeagueHR/LeagueIP*9
  
  FIP_Constant <- LeagueERA - (((13*LeagueHR)+(3*(LeagueBB+LeagueHBP))-(2*LeagueK))/LeagueIP)
  
  pit_stats <- pit_stats %>%
    mutate(FIP = round((((13*HR)+(3*(BB+HBP))-(2*SO))/IP) + FIP_Constant, 5),
           "ERA-" = round(ERA/LeagueERA*100), "FIP-" = round(FIP/LeagueERA*100),
           "K%" = round(SO/BF, 3), "K%+" = round(`K%`/LeagueK_rate*100),
           "BB%" = round((BB+IBB)/BF, 3), "BB%+" = round(`BB%`/LeagueBB_rate*100),
           "K-BB%" = round((SO-(BB+IBB))/BF, 3), "K-BB%+" = round(`K-BB%`/LeagueK_BB_rate*100),
           "HR/9" = round(HR/IP*9, 2), "HR/9+" = round(`HR/9`/LeagueHR9*100))
  
  ##pit_stats$IP <- round(pit_stats$IP, 1)
  pit_stats$ERA <- round(pit_stats$ERA, 2)
  pit_stats$FIP <- round(pit_stats$FIP, 2)
  pit_stats$WHIP <- round(pit_stats$WHIP, 2)
  
  ## These lines are only needed if you intend the write the data frame
  ## to somewhere, such as Google Sheets, that would coerce data types
  ##pit_stats$ERA[pit_stats$ERA == "Inf"] <- NA
  ##pit_stats$`ERA-`[pit_stats$`ERA-` == "Inf"] <- NA
  ##pit_stats$FIP[pit_stats$FIP == "Inf"] <- NA
  ##pit_stats$`FIP-`[pit_stats$`FIP-` == "Inf"] <- NA
  ##pit_stats$WHIP[pit_stats$WHIP == "Inf"] <- NA
  ##pit_stats$`HR/9`[pit_stats$`HR/9` == "Inf"] <- NA
  ##pit_stats$`HR/9+`[pit_stats$`HR/9+` == "Inf"] <- NA
  
  pit_stats <- pit_stats %>%
    select(num,chi_name,eng_name,team,TeamG,Q,G,GS,GR,IP,BF,ERA,"ERA-",FIP,"FIP-",
           "K%","K%+","BB%","BB%+","K-BB%","K-BB%+","HR/9","HR/9+",
           WHIP,SO,BB,IBB,HBP,HR,everything()) %>% arrange(desc(IP))
  
  ## Writing the data frames to the Global Environment
  
  assign(paste0("CPBL_bat_stats_",year), bat_stats, envir = .GlobalEnv)
  assign(paste0("CPBL_pit_stats_",year), pit_stats, envir = .GlobalEnv)
  
}
