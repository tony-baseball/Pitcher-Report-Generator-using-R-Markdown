# POSTGAME REPORTS
suppressWarnings(suppressMessages({
  library(plyr)
  library(tidyverse)
  library(baseballr)
  library(discordr)
  library(cowplot)
  library(emojifont)
  library(flextable)
  library(GeomMLBStadiums)
  library(DT)
  library(ggplot2)
  library(grDevices)
  library(grid)
  library(gridExtra)
  library(gtsummary)
  library(janitor)
  library(kableExtra)
  library(knitr)
  library(magick)
  library(openxlsx)
  library(pak)
  # library(pandoc)
  library(stringi)
  library(stringr)
  library(tools)
  library(webshot)
  library(tinytex)
  library(RSQLite)# tinytex::install_tinytex()
  library(slackr)
  library(googledrive)
  library(ggh4x)
  
}))


# CREATE A SLACK WEBHOOK FOR AUTOMATED NOTIFICATIONS

# slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
#              incoming_webhook_url = 'https://hooks.slack.com/services/your_webhook', 
#              token = 'your_token', echo = FALSE)
# 
# 
# 
# slackr_msg(txt = "Generating Boomers Post Game Reports", 
#            channel = "#reports-boomers",
#            username = paste("FLASHBOT",emoji('robot'))
# )


file <- read.csv('sample_data_v2.csv')

# rename the dataset and filter by the boomers most recent game ---- 
yakker_day <- file %>% 
  arrange(desc(Date), GameID, Time)  %>%
  mutate(Count = paste(Balls,Strikes,sep = '-'),
         HitType = recode(HitType, FlyBall = 'FB', LineDrive = 'LD', GroundBall = 'GB', PopUp = 'PU'),
         Result = 
           case_when(
             PitchCall == 'InPlay' ~ paste(PlayResult, HitType, sep = '-'),
             PitchCall != 'InPlay' & !grepl('Strikeout|Walk', KorBB) ~ PitchCall,
             PitchCall != 'InPlay' & grepl('Strikeout',KorBB) ~ paste('K',PitchCall),
             PitchCall != 'InPlay' & KorBB == 'Walk' ~ paste('BB'),
             T ~ NA
           ),
         Result = str_replace_all(Result, c('StrikeCalled' = 'Strike', 'StrikeSwinging' = 'Whiff', 
                                            'BallCalled' = 'Ball', 'HitByPitch' = 'HBP', 'K Strike' = 'K-Look')) ,
         swing_result = 
           case_when(
             PitchCall == 'Foul' ~ 'Foul',
             PitchCall == 'StrikeSwinging' ~ 'Whiff',
             PitchCall == 'InPlay' ~ paste(PlayResult)
           ),
         sweet_spot = 
           case_when(
             Angle >= 40 ~ 'Under',
             between(Angle, 33, 39) ~ 'Flare',
             between(Angle, 8, 32) ~ 'SweetSpot',
             between(Angle, 0, 7) ~ 'Burner',
             Angle <= 0 ~ 'Topped'
           ),
         PitcherThrows = str_replace_all(PitcherThrows, c('Right' = 'RHP', 'Left' = 'LHP')),
         StolenStrike = ifelse(
           PitchCall == "StrikeCalled" & in_zone == 0, 1, 0),
         StrikeLost = ifelse(PitchCall == "BallCalled" & in_zone == 1, 1, 0)
  )   %>%
  group_by(Batter) %>%
  mutate(PAofGame = cumsum(!duplicated(paste(Inning, PAofInning))) ) %>%
  ungroup() %>%
  group_by(Pitcher) %>%
  mutate(Pitcher_PAofGame = cumsum(!duplicated(paste(Inning, PAofInning))) ) %>%
  ungroup() %>%
  dplyr::mutate(TaggedPitchType = factor(TaggedPitchType, levels= c("Fastball", 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup',
                                                                    'Splitter', 'Knuckleball', 'Other' ))
  )



game_date <- unique(yakker_day$Date)
print (game_date)
# ************************* change team name below for manual
pitchers <- unique(yakker_day$Pitcher[grepl('boomers|Boomers',yakker_day$PitcherTeam)])
pitchers

# This script loads all the helper functions to create the reports
source('postgame pitcher git v2_functions.R')
#
# PITCHERS --------------------------------------------------------------------------------------------------------------------------------------------

# # this will send a message telling you how many reports are being made
# slackr_msg(txt = paste(length(pitchers),"new pitcher reports being generated."), 
#            channel = "#reports-boomers",
#            username = paste("FLASHBOT",emoji('robot')))




for (pitcher in pitchers) {
  
  master_postgame_boomers_pitcher_report(yakker_day = yakker_day, pitcher)
  
  
}
#  


