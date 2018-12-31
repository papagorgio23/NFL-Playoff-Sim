

###################################
###   Monte Carlo Simulation   ####
###################################



# This script will generate probabilities for each team to advance through each round of the NFL Playoffs.


##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################

library(knitr)
library(data.table)
library(tidyverse)
library(magrittr)


##############################
## Load necessary Datafiles ## --------------------------------------------------------------------------
##############################


seeds <- fread("Data/playoff_seeds.csv")
teams <- fread("Data/playoff_games.csv")



###################################
#####                         #####
##### ALL NECESSARY FUNCTIONS ##### ---------------------------------------------------------------------
#####                         #####
###################################

## convert_pct
convert_pct <- function(x)paste(round(100*x, 2), "%", sep="")

#Convert cities/nicknames to team abbreviations
convertTeamAbbreviation <- function(x){
  x[grep("Arizona", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("Cardinals", x, ignore.case=TRUE)] <- "ARZ"
  
  x[grep("Atlanta", x, ignore.case=TRUE)] <- "ATL"
  x[grep("Falcons", x, ignore.case=TRUE)] <- "ATL"
  
  x[grep("Baltimore", x, ignore.case=TRUE)] <- "BAL"
  x[grep("Ravens", x, ignore.case=TRUE)] <- "BAL"
  
  x[grep("Buffalo", x, ignore.case=TRUE)] <- "BUF"
  x[grep("Bills", x, ignore.case=TRUE)] <- "BUF"
  
  x[grep("Carolina", x, ignore.case=TRUE)] <- "CAR"
  x[grep("Panthers", x, ignore.case=TRUE)] <- "CAR"
  
  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Bears", x, ignore.case=TRUE)] <- "CHI"
  
  x[grep("Cincinnati", x, ignore.case=TRUE)] <- "CIN"
  x[grep("Bengals", x, ignore.case=TRUE)] <- "CIN"
  
  x[grep("Cleveland", x, ignore.case=TRUE)] <- "CLE"
  x[grep("Browns", x, ignore.case=TRUE)] <- "CLE"
  
  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Cowboys", x, ignore.case=TRUE)] <- "DAL"
  
  x[grep("Denver", x, ignore.case=TRUE)] <- "DEN"
  x[grep("Broncos", x, ignore.case=TRUE)] <- "DEN"
  
  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Lions", x, ignore.case=TRUE)] <- "DET"
  
  x[grep("Free", x, ignore.case=TRUE)] <- "FA"
  x[grep("Agent", x, ignore.case=TRUE)] <- "FA"
  
  x[grep("Green Bay", x, ignore.case=TRUE)] <- "GB"
  x[grep("Packers", x, ignore.case=TRUE)] <- "GB"
  
  x[grep("Houston", x, ignore.case=TRUE)] <- "HOU"
  x[grep("Texans", x, ignore.case=TRUE)] <- "HOU"
  
  x[grep("Indianapolis", x, ignore.case=TRUE)] <- "IND"
  x[grep("Colts", x, ignore.case=TRUE)] <- "IND"
  
  x[grep("Jacksonville", x, ignore.case=TRUE)] <- "JAX"
  x[grep("Jaguars", x, ignore.case=TRUE)] <- "JAX"
  
  x[grep("Kansas City", x, ignore.case=TRUE)] <- "KC"
  x[grep("Chiefs", x, ignore.case=TRUE)] <- "KC"
  
  x[grep("Miami", x, ignore.case=TRUE)] <- "MIA"
  x[grep("Dolphins", x, ignore.case=TRUE)] <- "MIA"
  
  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Vikings", x, ignore.case=TRUE)] <- "MIN"
  
  x[grep("New England", x, ignore.case=TRUE)] <- "NE"
  x[grep("Patriots", x, ignore.case=TRUE)] <- "NE"
  
  x[grep("New Orleans", x, ignore.case=TRUE)] <- "NO"
  x[grep("Saints", x, ignore.case=TRUE)] <- "NO"
  
  x[grep("Jets", x, ignore.case=TRUE)] <- "NYJ"
  
  x[grep("Giants", x, ignore.case=TRUE)] <- "NYG"
  
  x[grep("Oakland", x, ignore.case=TRUE)] <- "OAK"
  x[grep("Raiders", x, ignore.case=TRUE)] <- "OAK"
  
  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("Eagles", x, ignore.case=TRUE)] <- "PHI"
  
  x[grep("Pittsburgh", x, ignore.case=TRUE)] <- "PIT"
  x[grep("Steelers", x, ignore.case=TRUE)] <- "PIT"
  
  x[grep("San Diego", x, ignore.case=TRUE)] <- "LAC"
  x[grep("Chargers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("L.A. Chargers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("SD", x, ignore.case=TRUE)] <- "LAC"
  
  x[grep("Saint Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St. Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("Rams", x, ignore.case=TRUE)] <- "LAR"
  x[grep("Los Angeles", x, ignore.case=TRUE)] <- "LAR"
  x[grep("L.A. Rams", x, ignore.case=TRUE)] <- "LAR"
  x[grep("STL", x, ignore.case=TRUE)] <- "LAR"
  
  x[grep("San Francisco", x, ignore.case=TRUE)] <- "SF"
  x[grep("49ers", x, ignore.case=TRUE)] <- "SF"
  
  x[grep("Seattle", x, ignore.case=TRUE)] <- "SEA"
  x[grep("Seahawks", x, ignore.case=TRUE)] <- "SEA"
  
  x[grep("Tampa Bay", x, ignore.case=TRUE)] <- "TB"
  x[grep("Buccaneers", x, ignore.case=TRUE)] <- "TB"
  
  x[grep("Tennessee", x, ignore.case=TRUE)] <- "TEN"
  x[grep("Titans", x, ignore.case=TRUE)] <- "TEN"
  
  x[grep("Washington", x, ignore.case=TRUE)] <- "WAS"
  x[grep("Redskins", x, ignore.case=TRUE)] <- "WAS"
  
  return(x)
}

## simulate regular game
simulate.game <- function(team1, team2){
  Team1Seed <- seeds[seeds$TeamAbb == team1, "Seed"][[1]]
  Team2Seed <- seeds[seeds$TeamAbb == team2, "Seed"][[1]]
  
  if(Team1Seed > Team2Seed){
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Extract Probabilities for each team in the matchup
  p.1.2 <- teams[teams$home.team == team1 & teams$away.team == team2, "Avg.Prob"][[1]]
  p.2.1 <- 1 - p.1.2
  
  
  # simulate Game
  game.result <- sample(c(team1, team2), size = 1, prob = c(p.1.2, p.2.1), replace=TRUE)
  
  # return the winner
  game.result
}


## chance.df
chance.df <- function(series){
  
  tbl <- table(sim.results.df[ , series])
  df <- data.frame(team = names(tbl), chance = as.numeric(tbl)/sum(tbl))
  df <- df[order(df$chance, decreasing=TRUE), ]
  df
}



#############################
#####                   #####
##### SET EVERYTHING UP ##### ---------------------------------------------------------------------------
#####                   #####
#############################


# dataframe for results
simulation.results <- c()

# fix team names
seeds$TeamAbb <- convertTeamAbbreviation(seeds$Team)
teams$home.team <- convertTeamAbbreviation(teams$home.team)
teams$away.team <- convertTeamAbbreviation(teams$away.team)

# Set number of simulations at 1,000,001  -> Better than Goldman Sachs
## feel free to lower this number to 10,000-50,000 for it to complete the simulation in a reasonable timeframe
num_sims <- 1000001
i <- 1

# AFC Playoff Teams
afc.1 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 1, "TeamAbb"][[1]]
afc.2 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 2, "TeamAbb"][[1]]
afc.3 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 3, "TeamAbb"][[1]]
afc.4 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 4, "TeamAbb"][[1]]
afc.5 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 5, "TeamAbb"][[1]]
afc.6 <- seeds[seeds$Conference == "AFC" & seeds$Seed == 6, "TeamAbb"][[1]]

# NFC Playoff Teams
nfc.1 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 1, "TeamAbb"][[1]]
nfc.2 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 2, "TeamAbb"][[1]]
nfc.3 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 3, "TeamAbb"][[1]]
nfc.4 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 4, "TeamAbb"][[1]]
nfc.5 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 5, "TeamAbb"][[1]]
nfc.6 <- seeds[seeds$Conference == "NFC" & seeds$Seed == 6, "TeamAbb"][[1]]




###############################
#####                     #####
##### Run the Simulations ##### ------------------------------------------------------------------------
#####                     #####
###############################



set.seed(1234)
while (i <= num_sims) {
  
  ##### Wild Card Weekend
  # AFC
  wc.1.afc <- simulate.game(afc.3, afc.6)
  wc.2.afc <- simulate.game(afc.4, afc.5)
  
  # NFC
  wc.1.nfc <- simulate.game(nfc.3, nfc.6)
  wc.2.nfc <- simulate.game(nfc.4, nfc.5)
  
  ##### Divisional Weekend
  # AFC
  dw.1.afc <- simulate.game(afc.1, wc.2.afc)
  dw.2.afc <- simulate.game(afc.2, wc.1.afc)
  
  # NFC
  dw.1.nfc <- simulate.game(nfc.1, wc.2.nfc)
  dw.2.nfc <- simulate.game(nfc.2, wc.1.nfc)
  
  ##### Championship Weekend
  # AFC
  cw.1.afc <- simulate.game(dw.1.afc, dw.2.afc)
  
  # NFC
  cw.1.nfc <- simulate.game(dw.1.nfc, dw.2.nfc)
  
  ##### Superbowl
  champ <- simulate.game(cw.1.afc, cw.1.nfc)
  
  #print(paste0("This is the winner ",champ))
  
  results.all <- c( 
    i,  
    wc.1.afc,
    wc.2.afc,
    wc.1.nfc,
    wc.2.nfc,
    dw.1.afc,
    dw.2.afc,
    dw.1.nfc,
    dw.2.nfc,
    cw.1.afc,
    cw.1.nfc,
    champ
  )
  
  simulation.results <- c(simulation.results, results.all)
  
  i <- i + 1 
}



# Results
sim.results.mat <- matrix(simulation.results, ncol=12, byrow=TRUE)
sim.results.df <- as.data.frame(sim.results.mat)
names(sim.results.df) <- c( 
  "sim", 
  "wc.1.afc",
  "wc.2.afc",
  "wc.1.nfc",
  "wc.2.nfc",
  "dw.1.afc",
  "dw.2.afc",
  "dw.1.nfc",
  "dw.2.nfc",
  "cw.1.afc",
  "cw.1.nfc",
  "champ"
)


#################################################
#####                                       #####
##### Create a table with all probabilities ##### -----------------------------------------------------------------------------------------------
#####                                       #####
#################################################

# Superbowl Champions
champs.df <- chance.df("champ")

# Conference Champions
AFCC.df <- chance.df("cw.1.afc")
NFCC.df <- chance.df("cw.1.nfc")
Superbowl <- rbind(AFCC.df, NFCC.df)

# Divisional Champions
afc.1.df <- chance.df("dw.1.afc")
afc.2.df <- chance.df("dw.2.afc")
nfc.1.df <- chance.df("dw.1.nfc")
nfc.2.df <- chance.df("dw.2.nfc")
Conference <- rbind(afc.1.df, afc.2.df, nfc.1.df, nfc.2.df)

# Wildcard Games
afc.wc1.df <- chance.df("wc.1.afc")
afc.wc2.df <- chance.df("wc.2.afc")
nfc.wc1.df <- chance.df("wc.1.nfc")
nfc.wc2.df <- chance.df("wc.2.nfc")
Divisional <- rbind(afc.wc1.df, afc.wc2.df, nfc.wc1.df, nfc.wc2.df)

allteams <- unique(teams$home.team)
allteams <- as.data.frame(allteams)
colnames(allteams) <- "team"

# Merge all probabilities
all.chances.df <- allteams %>% 
  left_join(Divisional, by = "team") %>%
  rename(Divisional = chance) %>% 
  left_join(Conference, by = "team") %>%
  rename(Conference = chance) %>%
  left_join(Superbowl, by = "team") %>%
  rename(Superbowl = chance) %>%
  left_join(champs.df, by = "team") %>%
  rename(Champion = chance) %>%
  arrange(desc(Champion), desc(Superbowl), desc(Conference), desc(Divisional))


# Fix percentages
all.chances.df$Divisional <- ifelse(is.na(all.chances.df$Divisional), 0, all.chances.df$Divisional)
all.chances.df$Conference <- ifelse(is.na(all.chances.df$Conference), 0, all.chances.df$Conference)
all.chances.df$Superbowl <- ifelse(is.na(all.chances.df$Superbowl), 0, all.chances.df$Superbowl)
all.chances.df$Champion <- ifelse(is.na(all.chances.df$Champion), 0, all.chances.df$Champion)

all.chances.df[,2:5] <- sapply(all.chances.df[,2:5], convert_pct)


# View results
kable(all.chances.df)

# Write to a file
output_filename <- "NFLPlayoff_probs_2019.csv"
write.csv(all.chances.df, output_filename, row.names=FALSE)

