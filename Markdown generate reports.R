library(plyr)
library(tidyverse)
library(stringr)
library(baseballr)
library(discordr)
library(emojifont)
library(janitor)
library(tidyr)
library(ggplot2)
library(tools)
library(pandoc)
# THIS DOCUMNET WILL ALLOW YOU TO MAKE POST GAME PITCHER REPORTS USING YAKKERTECH DATA. IT COULD BE USED TO MAKE REPORTS FOR TRACKMAN DATA AS WELL SINCE THE NAMING CONVENTIONS ARE VERY SIMILAR, BUT MENTION OF COLUMN 'yt_Efficiency' NEEDS TO BE REMOVED

# OPTIONAL - CREATE A DISCORD WEBHOOK FOR AUTOMATED NOTIFICATIONS
conn_obj <- create_discord_connection(webhook = 'link-to-webhook', 
                                      username = paste('bot-name', emoji('robot')), set_default = TRUE)

send_webhook_message(paste("Generating Boomers Pitchers Game Reports"))

#this loads the new CSV and transforms it to add more columns and clean up some player names
yak_23 <-  read.csv("path to csv")

# rename the dataset and filter by the boomers most recent game 
yakker <- yak_23 %>%
  # FILTER
  filter(PitcherTeam == 'Schaumburg Boomers') %>%
  # CHANGE DATE WHEN NECESSARY
  filter(Date == max(Date)) 

# ------------------------
unique(yakker$PitcherTeam)
# set a factor to manually order the pitch types
yakker$TaggedPitchType <- factor(yakker$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other'))

# extract the date for the title of the report and change to different format
game_date <- unique(format(as.Date(yakker$Date), "%b %d, %Y"))
#each pitcher that pitched in the previous game
pitchers <- unique(yakker$Pitcher)
# this will send a message telling you how many reports are being made
send_webhook_message(paste(length(pitchers),"new pitcher reports being generated."))

# LOOP ---------

# For each pitcher in the dataset, run through the following code
for (pitcher in pitchers) {
  # Filter the data for the current pitcher
  pitcher_data <- yakker[yakker$Pitcher == pitcher, ]
  
  # pull the opponent's name for the report
  opponent <- pitcher_data$BatterTeam[1]
  
  # Generate the game summary / pitch characteristics table
  game_summary_table <- 
    pitcher_data %>%
    # using recode will allow us to save space on the document
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) ) %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage' = n(),
                     'Usage %' = n(),
                     'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                     'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                     'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
                     'Time' = sapply(`Tilt`, function(x) if (is.na(x)){return(NA)}
                                     else if(x > 180 & x <= 360){(x/30)-6}
                                     else if(x == 180){12}
                                     else{(x/30)+6}),
                     'HH' = as.integer(Time),
                     'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                   else if(x == 0){x+12}
                                   else if(x > 12){x-12}
                                   else{x+0}),
                     "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                     'Tilt' = paste0(HH,":", MM),
                     'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                     'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                     'Vert' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                     'Horz' = round(mean(HorzBreak, na.rm = TRUE),1),
                     'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),    
                     'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                     'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                     'Ext' = round(mean(Extension, na.rm = TRUE),1)
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
    dplyr::select(-Usage,-Time,-HH,-MM)

  # Generate the pitch usage table
  pitch_usage_table <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     'Use%' = n(),
                     "1P" = sum(PitchofPA == 1, na.rm = TRUE),
                     "1P%" = sum(`1P`, na.rm = TRUE),
                     '2K' = sum(Strikes == 2, na.rm = TRUE),
                     '2K%' = sum(`2K`, na.rm = TRUE),
                     'Strk%' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"), na.rm = TRUE)/n(),3)*100,
                     'Whiff%' = round(sum(PitchCall %in% c("StrikeSwinging"), na.rm = TRUE)/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay"), na.rm = TRUE),3)*100 
    ) %>%
    mutate(`Use%` = round(`Use%`/sum(`Use%`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`) 

  # Generate the game stat table
  game_stats <- 
    pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    group_by(Date) %>%
    summarise('IP' = round( (sum(OutsOnPlay, na.rm = TRUE))/3, 1),
              'BF' = n_distinct(Inning, Batter, PAofInning),
              'K' = sum(KorBB =="Strikeout"),
              'BB' = sum(KorBB =="Walk"),
              'HBP' = sum(PlayResult == 'HitByPitch'),
              'BIP' = sum(PitchCall == 'InPlay') ,
              'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
              'XBH' = sum(PlayResult %in% c('Double','Triple','HomeRun')),
              'R' = sum(RunsScored, na.rm = TRUE)
              ) %>%
    mutate(Date = format(as.Date(Date), "%m-%d"))
  
  # PITCH USAGE VS RHH
  usage_r <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    filter(BatterSide == 'Right') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage %' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  # Stats vs RHH
  stats_vs_r <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    filter(BatterSide=='Right')%>%
    dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                     'K' = sum(KorBB =="Strikeout"),
                     'BB' = sum(KorBB =="Walk"),
                     'HBP' = sum(PlayResult == 'HitByPitch'),
                     'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
                     'R' = sum(RunsScored, na.rm = TRUE),
                     '1B' = sum(PlayResult=='Single'),
                     '2B' = sum(PlayResult=='Double'),
                     '3B' = sum(PlayResult=='Triple'),
                     'HR' = sum(PlayResult=='HomeRun'),
                     'AVG' = round(H / (BF-BB-HBP),3),
                     'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
    dplyr::select(BF, K, BB, HBP, H, R,HR, AVG, SLG)
  
  # USAGE VS LHH
  usage_l <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    filter(BatterSide == 'Left') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage %' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  
  # STATS VS LHH
  stats_vs_l <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    filter(BatterSide=='Left')%>%
    dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                     'K' = sum(KorBB =="Strikeout"),
                     'BB' = sum(KorBB =="Walk"),
                     'HBP' = sum(PlayResult == 'HitByPitch'),
                     'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
                     'R' = sum(RunsScored, na.rm = TRUE),
                     '1B' = sum(PlayResult=='Single'),
                     '2B' = sum(PlayResult=='Double'),
                     '3B' = sum(PlayResult=='Triple'),
                     'HR' = sum(PlayResult=='HomeRun'),
                     'AVG' = round(H / (BF-BB-HBP),3),
                     'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
    dplyr::select(BF, K, BB, HBP, H, R,HR, AVG, SLG) 
  
  
  # BATTED BALL DATA
  batted_ball <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'BIP' = sum(PitchCall == 'InPlay'),
                     'XBH' = sum(PlayResult %in% c("Double","Triple","HomeRun")),
                     'Avg EV' = round(mean(ExitSpeed, na.rm= TRUE),0),
                     'EV 90+' = sum(ExitSpeed >= 90, na.rm= TRUE),
                     'EV -90' = sum(ExitSpeed < 90, na.rm= TRUE) 
    )
  
  
  # Generate the pitch movement plot
  pitch_movement_plot <- 
     ggplot(data =  
             pitcher_data %>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) ),
           
           
           aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" )  
    xlim(-22, 22) + ylim(-22, 22) +
    geom_segment(aes(x = 0, y = -22, xend = 0, yend = 22), size = 1, color = "grey55") +
    geom_segment(aes(x = -22, y = 0, xend = 22, yend = 0), size = 1, color = "grey55") +
    geom_point(size =4, alpha = .75) +
      # we manually set the pitch colors so that they are uniform across each plot and tables
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'OT' = 'black')) +  
    theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
  
  
  # Pitch velo table and plot
  pvp_game <- pitcher_data %>%
      group_by(Date, Pitcher, TaggedPitchType, Inning) %>%
      summarise(Avg = mean(RelSpeed, na.rm = TRUE), Max = max(RelSpeed, na.rm = T), min = min(RelSpeed, na.rm = T)) %>%
      arrange(Inning, desc(Max)) %>%
      dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) )
  length(unique(pvp_game$Inning))
  
  pvp_game_plot <- 
    # This loop says if they pitched more than one inning, then to add geom_line(), if they only pitched one inning, then use only geom_point()
    if(length(unique(pvp_game$Inning)) >1) {
      ggplot(data = pvp_game, aes(x = Inning, y = Avg, color = TaggedPitchType) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$Inning)) +
        geom_line() +
        scale_x_continuous(labels = as.numeric(pvp_game$Inning), breaks = pvp_game$Inning) +
        # xlim(min(velo_inn$Inning),max(velo_inn$Inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
        labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      
    } else{
      
      ggplot(data = pvp_game, aes(x = Inning, y = Avg, color = TaggedPitchType) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$Inning)) +
      #  geom_line() +
        scale_x_continuous(labels = as.numeric(pvp_game$Inning), breaks = pvp_game$Inning) +
        # xlim(min(velo_inn$Inning),max(velo_inn$Inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
        labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
    }
    
  
  
  # Pitch location plot vs rhh with a facet wrap on one of the created columns
  plp_rhh <- 
    ggplot(data = pitcher_data %>% filter(filter_col !='Take' & filter_col !=''& !is.na(filter_col), BatterSide=='Right' )%>%
           dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                   Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ), 
         aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs RHH" ) )+
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(size =3, alpha = .75) +
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
    facet_wrap(~filter_col, nrow = 1)+
    theme(strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
    )
  
  
  
  # Pitch location plot vs lhh with a facet wrap on one of the created columns
  
  plp_lhh <- 
    ggplot(data = pitcher_data %>% filter(filter_col !='Take' & filter_col !=''& !is.na(filter_col), BatterSide=='Left' )%>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ), 
           aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75) +
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
    
  
  
  
  
  
  # CREATES ANOTHER USAGE BREAKDOWN
  
  bd <- pitcher_data%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ) %>%
    mutate(Count = paste0(Balls,"-",Strikes),
           filter_2 = ifelse(Count == "0-0", 'FirstPitch',
                             ifelse(Strikes == 2, '2 Strikes',
                                    ifelse(Count %in% c('1-0','2-0','3-0','2-1','3-1'), 'PitcherBehind',
                                           ifelse(Count %in% c('0-1','0-2', '1-2' ), 'PitcherAhead', ''
                                           ))  ))    
    )%>%
    filter(filter_2 != '') %>%
    group_by(TaggedPitchType, filter_2, BatterSide) %>%
    summarise(P = n() ) %>%
    group_by(filter_2, BatterSide) %>%
    mutate(percentage = round(P / sum(P),3)*100) %>%
    mutate(BatterSide = gsub("Left",'LHH',BatterSide),
           BatterSide = gsub('Right','RHH',BatterSide))
  
  
  
  breakdown<-ggplot(bd %>%
           mutate(filter_2 = factor(filter_2, levels = c('FirstPitch', '2 Strikes', 'PitcherAhead', 'PitcherBehind') )), 
         aes(x = "", y = percentage, fill = TaggedPitchType)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3)+
    theme_void()+
    theme(strip.text = element_text(size = 11, face = 'bold'))+
    facet_wrap(BatterSide~filter_2, nrow=2) + 
    theme(legend.position="none")+
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black'))
  
  
  
  # SET THE PARAMETERS FOR THE R MARKDOWN FILE
  params <- list(
    game_summary_table = game_summary_table,
    pitch_usage_table = pitch_usage_table,
    pitch_movement_plot = pitch_movement_plot,
    pitcher = pitcher,
    game_stats = game_stats,
    usage_r = usage_r,
    stats_vs_r = stats_vs_r,
    usage_l = usage_l,
    stats_vs_l= stats_vs_l,
    date = game_stats$Date[1],
    opponent = pitcher_data$BatterTeam[1],
    pvp_game_plot = pvp_game_plot,
    batted_ball = batted_ball,
    plp_lhh = plp_lhh,
    plp_rhh = plp_rhh,
    breakdown =breakdown
    
  )
  
  # SETS THE DATE FOR THE FILE NAME
  file_date <- format(as.Date(pitcher_data$Date[1]), "%m-%d")
  
  # Knit the R Markdown file to PDF
  rmarkdown::render(input = "path-to/Scouting-Reports-P.Rmd",
                    output_file = paste0("path-to-save/",file_date," ",pitcher, " report",".pdf"),
                    params = params)
}

send_webhook_message(paste("New reports for", paste(unique(pitchers), collapse = ", "),"from", game_date, "created."))
