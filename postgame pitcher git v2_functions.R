# PITCHERS ---------------------------------------

pitcher_arm_angle_plot <- function(pitcher_name, pitch_data_df) {
  
  pitch_data_df$PitcherThrows <- ifelse(pitch_data_df$PitcherThrows == 'RHP', 'Right', ifelse(pitch_data_df$PitcherThrows == 'LHP', 'Left', pitch_data_df$PitcherThrows))
  
  circleFun <- function(center = c(0, 0), radius = 24, npoints = 100) {
    tt <- seq(0, 2 * pi, length.out = npoints)
    data.frame(
      x = center[1] + radius * cos(tt),
      y = center[2] + radius * sin(tt)
    )
  }
  
  # creats a circle for the SAVANT plot
  circle <- circleFun(center = c(0, 0), radius = 24)
  
  { # code for the pitcher's mound in the ARM ANGLE plot
    df <- data.frame(x = 0.5, y = 0)
    theta <- seq(0, pi, length.out = 100)  # Change the range to create a semi-circle that is right-side up for the mound!
    r <- 40  # The horizontal range from -40 to 40 for the mound!
    # Calculate the x and y coordinates
    mound <- data.frame(
      x = r * cos(theta),
      y = 4 * sin(theta)
    )
  }
  
  pitch_colors = data.frame(TaggedPitchType = c("Fastball", "Sinker", "Cutter", "Curveball", 
                                                "Slider", "Changeup", "Splitter", "Knuckleball", "Other"),
                            PitchCode = c('FB', 'SI', 'CT', 'CB', 'SL', 'CH', 'SPL', 'KN', 'OT'),
                            Color = c('red', '#a34700', 'gold', 'darkgreen', 'cornflowerblue',
                                      'violet',  'black',  'black',  'black'))
  
  arm_angle_categories <- function(df) {
    bins <- c(0, 30, 60, 90, 120, 180)
    # bins <- c(90, 60, 30, 0, -30, -90)
    
    labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
    
    df <- df %>%
      mutate(
        arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
      )
    
    return(df)
  }
  
  if(pitcher_name %in% pitch_data_df$Pitcher){
    # INDIVIDUAL PITCHER PITCH DATA -----
    p <- pitch_data_df %>%
      filter(Pitcher == pitcher_name) %>%
      filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
      mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
             TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                             Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) 
    
    # INDIVIDUAL PITCHER AVERAGE PITCH METRICS ----
    p_mean <- suppressMessages(
      pitch_data_df %>%
        filter(Pitcher == pitcher_name) %>%
        filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
        mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
               TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                               Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
        group_by(Pitcher, PitcherThrows, TaggedPitchType) %>%
        summarise(across(c(InducedVertBreak, HorzBreak, SpinAxis, arm_angle_savant, arm_length, shoulder_pos), ~ mean(.,na.rm = T)),
                  across(c(RelHeight, RelSide), ~ mean(. * 12,na.rm = T)),
                  usage = n()) %>%
        mutate(usage = round(usage / sum(usage),3)*100,
               scaled_usage = (usage - min(usage)) / (max(usage) - min(usage)) * (40 - 20) + 20,
        )
    )
    
    # LEAGUE AVERAGE PITCH METRICS MATCHING INDIVIDUAL PITCHERS ARSENAL----
    # p_lg <- pitch_data_lg_avg <- dbGetQuery(db, 
    #                                         "SELECT * FROM pitch_data where TaggedPitchType <> '' and SEASON = 2024 ")%>%
    #   mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
    #          TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
    #                                   Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN' , Splitter = 'SPL'  ) ) %>%
    #   group_by(PitcherThrows, TaggedPitchType) %>%
    #   dplyr::summarise(usage = n(),
    #                    across(c(InducedVertBreak, HorzBreak, SpinAxis, arm_angle_savant, arm_length, shoulder_pos), ~ mean(.,na.rm = T)),
    #   ) %>%
    #   filter(PitcherThrows %in% p_mean$PitcherThrows) %>%
    #   filter(TaggedPitchType %in% p_mean$TaggedPitchType)
    
    # INDIVIDUAL PITCHER AVERAGE ARM ANGLE AND RELEASE DATA----
    p_arm <<- suppressMessages(
      pitch_data_df %>%
        filter(Pitcher == pitcher_name) %>%
        group_by(Pitcher, PitcherThrows,TaggedPitchType ) %>%
        summarise(PitcherTeam = paste(unique(PitcherTeam), collapse = ', '),
                  height_inches = mean(height_inches, na.rm = T),
                  shoulder_pos = mean(shoulder_pos, na.rm = T),
                  release_pos_x = median(RelSide * 12, na.rm = T),
                  release_pos_z = median(RelHeight * 12, na.rm = T),
                  arm_angle = median(arm_angle, na.rm = T),
                  arm_angle_savant = median(median(arm_angle_savant, na.rm = T), na.rm = T)
        ) %>%
        arm_angle_categories() %>%
        # This is to scale the arm angle line/point to fit into the savant and movement plots
        mutate(relx = case_when(
          release_pos_x > 20 ~ 20,
          release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
          TRUE ~ release_pos_x
        ),
        relz = case_when(
          release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
          release_pos_z > 20 ~ 20,
          TRUE ~ release_pos_z
        ),
        arm_path = 'Arm Path'
        ) %>%
        mutate(arm_length = height_inches * .39, # Average arm length is roughlt 39% of height
               slope = (release_pos_z - shoulder_pos) / (release_pos_x - 0),
               arm_dist = sqrt((release_pos_x - 0)^2 + (release_pos_z - shoulder_pos)^2),
               arm_scale = arm_length / arm_dist,
               should_x = case_when(
                 arm_angle_savant >= 40 ~ 0,
                 between(arm_angle_savant, 10, 40)  ~ 0,
                 arm_angle_savant < 10 ~ 0,
               ),
               should_y = case_when( # changes the height of the shoulder based on which arm angle / arm angle png 
                 median(arm_angle_savant) >= 40 ~ 62.5,
                 between(median(arm_angle_savant), 10, 40)  ~ 56,
                 median(arm_angle_savant) < 10 ~ 45,
                 
               ),
               rel_x = should_x + (arm_scale * (release_pos_x - should_x)), # calculates new release point along the original slope
               rel_z = shoulder_pos + (arm_scale * (release_pos_z - shoulder_pos)) + should_y - (shoulder_pos), # calculates new release point along the original slope
               arm_path = 'Arm Path'
        )%>%
        mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
               TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                        Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN' , Splitter = 'SPL'  ) )
    )
    
    p <- p  %>% filter(!is.na(HorzBreak) & !is.na(InducedVertBreak))
    
    # this sets the pitch colors in the plotly versions of the code
    p_c <- pitch_colors %>%
      filter(PitchCode %in% p_mean$TaggedPitchType)
    
    p_c <- setNames(as.character(p_c$Color), p_c$PitchCode)
    
    
    if (nrow(p) > 0) { # ARM ANGLE PLOT CODE ----
      
      # PITCHING RUBBER COORDS
      rubber_xmin <- -9 
      rubber_xmax <- 9   
      rubber_ymin <- 4   
      rubber_ymax <- 4.5 
      
      # base of the arm_angle plot
      base_plot <- ggplot(df, aes(x, y)) + 
        geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
        xlim(-50,50) + ylim(0,100)+
        geom_rect(aes(xmin = rubber_xmin, xmax = rubber_xmax, ymin = rubber_ymin, ymax = rubber_ymax), 
                  fill = "white", color = "black") 
      
      if(unique(p_arm$PitcherThrows) == 'Right') {
        
        if(p_med_arm_angle >= 40) { 
          
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_top_right_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         size = 4, color = "#6892a2", alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
          
        } else if (between(p_med_arm_angle, 10,40)) {
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_mid_right_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         size = 4, color = "#6892a2", alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
        } else if(p_med_arm_angle < 10){
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_low_right_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         size = 4, color = "#6892a2", alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
        }
      } else if(unique(p_arm$PitcherThrows) == 'Left'){
        if(p_med_arm_angle >= 40) { 
          
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_top_left_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         size = 4, color = "#6892a2", alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
        } else if (between(p_med_arm_angle, 10,40)) {
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_mid_left_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         color = '#6892a2', 
                         size = 3, alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            
            scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                          'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
        } else if(p_med_arm_angle < 10){
          image_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-analytics/www/SavantPitchers_low_left_back-svg.png"
          
          base_plot +
            ggimage::geom_image(image = image_path, size = .6, x = 0, y =45)+
            geom_segment(data = p_arm, aes(x=0, y =  shoulder_pos + (should_y - shoulder_pos), 
                                           xend = rel_x, 
                                           yend = rel_z) , 
                         size = 4, color = "#6892a2", alpha = .4) +
            # geom_point(x = p_arm$rel_x, y =p_arm$rel_z, fill = 'white', color = 'red', pch = 21, size =5, stroke = 2)+
            geom_point(data = p_arm, alpha = .7,
                       aes( x =rel_x, y =rel_z, fill = TaggedPitchType),
                       color = 'black', pch = 21, size =4)+
            labs(
              title = paste0('Arm Angle: ', round(p_med_arm_angle), "°")
            )  +
            scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                         'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
            theme_minimal()+
            theme(plot.title = element_text(hjust = .5, face = 'bold', size = 10),
                  legend.position = "none")+
            coord_fixed()
          
          
        }
      } 
      
      
    } else {
      print('Pitcher not found. Try again!')
    }
    
  } else {
    print('Pitcher not found. Try again!')
  }
  
}

pitcher_pitch_mvmt_plot <- function(pitcher_data){
  # Generate the pitch movement plot
  
  p_2 <- pitcher_data %>%
    group_by(Pitcher, PitcherThrows) %>%
    summarise(PitcherTeam = unique(PitcherTeam),
              height_inches = mean(height_inches, na.rm = T),
              shoulder_pos = mean(shoulder_pos, na.rm = T),
              release_pos_x = median(RelSide * 12, na.rm = T),
              release_pos_z = median(RelHeight * 12, na.rm = T),
              arm_angle = median(arm_angle, na.rm = T),
              arm_angle_180 = median(arm_angle_180, na.rm = T),
              arm_angle_savant = median(arm_angle_savant, na.rm = T)
    ) %>%
    # tonybaseball::arm_angle_categories() %>%
    mutate(relx = case_when(
      release_pos_x > 20 ~ 20,
      release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
      TRUE ~ release_pos_x
    ),
    relz = case_when(
      release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
      release_pos_z > 20 ~ 20,
      TRUE ~ release_pos_z
    ),
    arm_path = 'Arm Path',
    # relx = ifelse(PitcherThrows %in% c('Right','RHP'), relx, ifelse(PitcherThrows %in% c('Left','LHP'), -relx, relx))
    )
  
  p_med_arm_angle <<-p_2$arm_angle_savant
  
  pitch_movement_plot <- 
    ggplot(data =
             pitcher_data %>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) ),
           aes(x = HorzBreak, y = InducedVertBreak)) +
    labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" )  +
    xlim(-25, 25) + ylim(-25, 25) +
    geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
    geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
    coord_fixed()+
    geom_segment(x=0, xend= p_2$relx, y= 0, yend=p_2$relz, color='grey55', linetype = 2)+
    geom_point(aes(fill = TaggedPitchType), size =4, alpha = .75, color= 'black', pch = 21) +
    # we manually set the pitch colors so that they are uniform across each plot and tables
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
    theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) # axis.title = element_text(size = 6))
  
  if(pitcher_data$PitcherThrows[1] %in% c('Right', 'RHP')) {
    
    pitch_movement_plot <- pitch_movement_plot +
      annotate('text', x = -12.5, y = 15, label= paste0(round(p_2$arm_angle_savant),"° Arm Angle\n", gsub("Three-Quarters","3/4",p_2$arm_angle_type)))
    
  } else if (pitcher_data$PitcherThrows[1] %in% c('Left', 'LHP')) {
    pitch_movement_plot <- pitch_movement_plot +
      annotate('text', x = 12.5, y = 15, label= paste0(round(p_2$arm_angle_savant),"° Arm Angle\n", gsub("Three-Quarters","3/4",p_2$arm_angle_type)))
  }
  
}

pitcher_velocity_plot <- function(pitcher_data){
  # Pitch velo table and plot
  pvp_game <- pitcher_data %>%
    dplyr::group_by(Date, Pitcher, TaggedPitchType, Inning) %>%
    dplyr::summarise(Avg = mean(RelSpeed, na.rm = TRUE), Max = max(RelSpeed, na.rm = T), min = min(RelSpeed, na.rm = T)) %>%
    dplyr::arrange(Inning, desc(Max)) %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL', Kunckleball = 'KN'  ) )
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
        scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                      'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
        labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
        theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) #element_text(size = 8))
      
    } else{
      
      ggplot(data = pvp_game, aes(x = Inning, y = Avg, color = TaggedPitchType) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$Inning)) +
        #  geom_line() +
        scale_x_continuous(labels = as.numeric(pvp_game$Inning), breaks = pvp_game$Inning) +
        # xlim(min(velo_inn$Inning),max(velo_inn$Inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                      'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
        labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
        theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) #element_text(size = 8))
    }
}

pitcher_rolling_stuff_plot <- function(pitcher_data){
  # ROLLING STUFF + 
  
  rolling_stuff  <- pitcher_data %>%
    filter(!is.na(tj_stuff_plus)) %>%
    dplyr::select(PitchNo,TaggedPitchType,InducedVertBreak,HorzBreak,PitchCall,PlayResult,run_value,tj_stuff_plus) %>%
    # tail(100) %>%
    mutate(pitch_num = row_number(),
           `Stuff+` = round(tj_stuff_plus)) %>%
    group_by(TaggedPitchType) %>%
    mutate(rolling_stuff_plus = 
             zoo::rollapply(tj_stuff_plus, width = 5, FUN = mean, align = "right", fill = NA, na.rm = TRUE))%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL', Kunckleball = 'KN'  ) )%>%
    ungroup()%>%
    tail(40)
  
  
  rolling_stuff_plot <- ggplot(data = rolling_stuff  , aes(x = pitch_num, y = `rolling_stuff_plus`, color = TaggedPitchType) ) +
    geom_point( size = 1.5, alpha = .75) +
    geom_line(linewidth= 1 ) +
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
    labs(title = "Rolling 5 Pitch Stuff+", x = "", y = "Chase%", color = " " ) + #, title = "Pitch Velocity by Inning") +
    theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank())+
    ylim(min(rolling_stuff$rolling_stuff_plus,na.rm = T)-10,
         max(rolling_stuff$rolling_stuff_plus,na.rm = T)+10)
  
  rolling_stuff_plot
}

pitcher_pitch_velo_dist <- function(pitcher_data){
  pitch_dist_plot <-
    ggplot(data =  
             pitcher_data %>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' )))  +
    geom_density(aes(x = RelSpeed, fill = TaggedPitchType), alpha = .7) +
    geom_point(data = pitcher_data %>%
                 dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                         Cutter = 'CT', Changeup = 'CH', Other = 'OT' )) %>% group_by(TaggedPitchType) %>% filter(n() < 5), 
               aes(x = RelSpeed, y = 0.03 ,fill = TaggedPitchType), color = 'black', size = 4, pch = 21, alpha = .7) +
    facet_wrap2(~TaggedPitchType, ncol = 1, strip.position = 'left', scales = 'free_y') +
    
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
    labs(title = "Pitch Velo Distribution" ,fill = "") + # , na.rm = TRUE)+
    theme_bw() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      # strip.text = element_text(color = 'black', face = 'bold'),
      strip.text = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"),
      panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "grey"),
      # panel.background = element_rect(fill = '#f5f5f5'),
      # plot.background = element_rect(fill = '#f5f5f5'),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5) #,
      # aspect.ratio = 1/3
    ) 
}

pitcher_pitch_loc_rhh <- function(pitcher_data){
  # Pitch location plot vs rhh with a facet wrap on one of the created columns
  vs_rhh <- pitcher_data %>% filter(filter_col !='Take' & filter_col !=''& !is.na(filter_col), BatterSide=='Right' )%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT',  Splitter = 'SPL', Kunckleball = 'KN'  ) )
  if(nrow(vs_rhh) > 0) {
    plp_rhh <- 
      ggplot(data = vs_rhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs RHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75, pch = 21) +
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  } else {
    plp_rhh <- 
      ggplot(data = vs_rhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs RHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      annotate(x = 0, y = 2.5, geom = 'text', label = 'No Pitch Data')+
      # facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  }
}

pitcher_pitch_loc_lhh <- function(pitcher_data){
  # Pitch location plot vs lhh with a facet wrap on one of the created columns
  vs_lhh <- pitcher_data %>% filter(filter_col !='Take' & filter_col !=''& !is.na(filter_col), BatterSide=='Left' )%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN'  ) )
  if(nrow(vs_lhh) > 0) {
    
    plp_lhh <- 
      ggplot(data = vs_lhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75, pch = 21) +
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~filter_col, nrow = 1)+
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
      )
  } else {
    plp_lhh <- 
      ggplot(data = vs_lhh, 
             aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      annotate(x = 0, y = 2.5, geom = 'text', label = 'No Pitch Data')+
      # facet_wrap(~filter_col, nrow = 1)+
      theme(#strip.text = element_text(size = 7, face = 'bold'),
        axis.text.x=element_blank(), #remove x axis labels
        axis.text.y=element_blank(),  #remove y axis labels
      )
  }
  
}

pitcher_pitch_breakdown <- function(pitcher_data){
  # CREATES ANOTHER USAGE BREAKDOWN
  
  test <- pitcher_data%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN'  ) ) %>%
    mutate(Count = paste0(Balls,"-",Strikes),
           filter_2 = ifelse(Count == "0-0", 'FirstPitch',
                             ifelse(Strikes == 2, '2 Strikes',
                                    ifelse(Count %in% c('1-0','2-0','3-0','2-1','3-1'), 'PitcherBehind',
                                           ifelse(Count %in% c('0-1','0-2', '1-2' ), 'PitcherAhead', ''
                                           ))  ))    
    )%>%
    filter(filter_2 != '') %>%
    group_by(TaggedPitchType, filter_2, BatterSide) %>%
    dplyr::summarise(P = n() ) %>%
    group_by(filter_2, BatterSide) %>%
    mutate(percentage = round(P / sum(P),3)*100) %>%
    mutate(BatterSide = gsub("Left",'LHH',BatterSide),
           BatterSide = gsub('Right','RHH',BatterSide))
  
  
  
  breakdown<-ggplot(test %>%
                      mutate(filter_2 = factor(filter_2, levels = c('FirstPitch', '2 Strikes', 'PitcherAhead', 'PitcherBehind') )), 
                    aes(x = "", y = percentage, fill = TaggedPitchType)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
    #  coord_polar(theta = "y")+
    theme_void()+
    theme(strip.text = element_text(size = 11, face = 'bold'))+
    facet_wrap(BatterSide~filter_2, nrow=2) + 
    theme(legend.position="none")+
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black'))
}

pitcher_spin_axis_plot <- function(pitcher_data) {
  spin_axis <- 
    pitcher_data %>%
    filter(TaggedPitchType != '') %>% 
    filter(!is.na(SpinAxis)) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
           TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                    Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
    mutate(SpinAxis_rounded = round(SpinAxis / 5) * 5,
           SpinAxis_inferred =  atan2(HorzBreak,InducedVertBreak) * (180 / pi) + 180,
           SpinAxis_inferred_rounded = round(SpinAxis_inferred/5)*5
           # SpinAxis_inferred2 = ifelse(SpinAxis_inferred2 < 0, SpinAxis_inferred2 + 360, SpinAxis_inferred2)
    ) 
  
  spin_axis_spin_based <-  spin_axis%>%
    group_by(Pitcher, TaggedPitchType, SpinAxis_rounded) %>%
    dplyr::summarise(count = n())
  
  spin_axis_observed <- spin_axis%>%
    group_by(Pitcher, TaggedPitchType, SpinAxis_inferred_rounded) %>%
    dplyr::summarise(count = n())
  
  spin_axis_plot <- ggplot(spin_axis_spin_based, aes(x = SpinAxis_rounded, y = 1)) +
    geom_hline(yintercept = 1)+
    geom_hline(yintercept = .7)+      
    geom_point(alpha = .7, aes(size = count, fill = TaggedPitchType), color = 'black',  shape = 21) +
    geom_point(data = spin_axis_observed, alpha = .7, aes(x = SpinAxis_inferred_rounded, y = .7, size = count, fill = TaggedPitchType), color = 'black',  shape = 21) +
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) + 
    scale_y_continuous(limits = c(0, 1)) +
    annotate(geom = 'text', x = 0, y = 0, label = 'Outer Ring is\nObserved Spin Axis\n\nInner Ring is\nSpin Based Axis')+
    coord_polar(theta = "x", start = pi) +  
    theme_minimal() + 
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black'))+
    labs(title = 'Spin Based Axis vs Observed Spin Axis' ,x = "Spin Axis (degrees)", y = "", fill = '') +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),  
      axis.ticks.y = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA, size = .5),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = 'none',
      legend.text = element_text(size = 12)
    )  +
    scale_size_continuous(range = c(2, 10), guide = 'none') +
    guides(fill = guide_legend(override.aes = list(size = 6), 
                               ncol = 1) ) 
}


master_postgame_boomers_pitcher_report <- function(yakker_day,pitcher){
  
  
  # Filter the data for the current pitcher
  pitcher_data <<- yakker_day[yakker_day$Pitcher == pitcher, ] %>%
    dplyr::arrange(PitchNo)
  
  # pull the opponent's name for the report
  opponent <- pitcher_data$BatterTeam[1]
  
  
  # Generate the game stat table
  game_stats <<- 
    pitcher_data  %>% 
    dplyr::mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                           Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ), )%>%
    dplyr::group_by(Date) %>%
    summarise(# Date = unique(Date),
      'IP' = round( (sum(OutsOnPlay, na.rm = TRUE))/3, 1),
      'P' = n(),
      'BF' = n_distinct(Inning, Batter, PAofInning),
      'K' = sum(KorBB =="Strikeout"),
      'BB' = sum(KorBB =="Walk"),
      'HBP' = sum(PlayResult == 'HitByPitch'),
      'BIP' = sum(PitchCall == 'InPlay') ,
      'H' = sum(total_bases > 0, na.rm = T),
      'XBH' = sum(PlayResult %in% c('Double','Triple','HomeRun')),
      'R' = sum(RunsScored, na.rm = TRUE),
      Whiffs = sum(whiff==1, na.rm = T),
      BAA = round(H / (BF - BB - HBP - sum(PlayResult =='Sacrifice')),3),
      'Hard%' = round(sum(hardhit,na.rm = T) / sum(bbe,na.rm = T) * 100,1),
      'Stuff+' = round(median(tj_stuff_plus, na.rm = T))
      
    ) %>% 
    ungroup() %>%
    mutate(#Date = as.Date(Date).
    )
  
  
  # Generate the game summary / pitch characteristics table
  game_summary_table <<- 
    pitcher_data %>%
    # using recode will allow us to save space on the document
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Kunckleball = 'KN', Other = 'OT' ) ) %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     'Usage' = n(),
                     '%' = n(),
                     'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                     'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                     'IVB' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                     'HB' = round(mean(HorzBreak, na.rm = TRUE),1),
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
                     # 'Axis°' = round(mean(SpinAxis, na.rm = T)),
                     'Arm°' = round(mean(arm_angle_savant, na.rm = T)),
                     'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),    
                     'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                     'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                     'Ext' = round(mean(Extension, na.rm = TRUE),1),
                     # 'Chase%' = round(median(tj_stuff_plus, na.rm = T))
    ) %>%
    mutate(`%` = round(`%`/sum(`%`),3)*100) %>%
    dplyr::select(-Usage,-Time,-HH,-MM)
  
  
  
  # Generate the pitch usage table
  pitch_usage_table <<- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize(
      '#' = n(),
      'CStrk' = sum(PitchCall == "StrikeCalled", na.rm = T),
      'Swings' = sum(swing, na.rm = T),
      'Whiffs' = sum(whiff == 1, na.rm = T),
      'Stuff+' = round(median(tj_stuff_plus, na.rm = T)),
      'Zone%' = round(sum(in_zone, na.rm = T) / n() * 100,1), 
      'Strk%' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"), na.rm = TRUE)/n(),3)*100,
      'Chase%' = round(sum(in_zone==0 & swing == 1, na.rm = T) / n() *100,1 ),
      'Whiff%' = round(sum(whiff == 1, na.rm = T)/
                         sum(swing == 1, na.rm = T),3)*100 ,
      'CSW%' = round((sum(PitchCall=='StrikeCalled', na.rm= T) + sum(whiff, na.rm = T)) / n() * 100,1),
      'BIP' = sum(PitchCall == 'InPlay'),
      'Avg EV' = round(mean(ExitSpeed[bbe==1], na.rm= TRUE),0),
      'Hard%' = round(sum(hardhit,na.rm = T) / sum(bbe,na.rm = T)*100,1)
    ) 
  
  
  
  
  
  # BATTED BALL DATA
  batted_ball <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     'BIP' = sum(PitchCall == 'InPlay'),
                     'XBH' = sum(PlayResult %in% c("Double","Triple","HomeRun")),
                     
                     'EV 90+' = sum(ExitSpeed >= 90, na.rm= TRUE),
                     'EV -90' = sum(ExitSpeed < 90, na.rm= TRUE) 
    )
  
  
  # Stats vs RHH
  stats_vs_r <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    filter(BatterSide=='Right')%>%
    dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                     'K' = sum(KorBB =="Strikeout"),
                     'BB' = sum(KorBB =="Walk"),
                     'Whiffs' = sum(whiff==1,na.rm = T),
                     'HBP' = sum(PlayResult == 'HitByPitch'),
                     'H' = sum(total_bases > 0,na.rm = T),
                     'R' = sum(RunsScored, na.rm = TRUE),
                     '1B' = sum(PlayResult=='Single'),
                     '2B' = sum(PlayResult=='Double'),
                     '3B' = sum(PlayResult=='Triple'),
                     'HR' = sum(PlayResult=='HomeRun'),
                     XBH = sum(total_bases >1, na.rm = T),
                     'AVG' = round(H / (BF-BB-HBP),3),
                     'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
    dplyr::select(BF, K,Whiffs, BB, HBP, H,XBH,  R,HR, AVG, SLG)
  
  
  # PITCH USAGE VS RHH
  usage_r <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    filter(BatterSide == 'Right') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     '%' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 ,
                     'Stuff+' = round(median(tj_stuff_plus, na.rm = T))
                     
    ) %>%
    mutate(`%` = round(`%`/sum(`%`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  
  
  # USAGE VS LHH
  usage_l <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    filter(BatterSide == 'Left') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     '%' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 ,
                     'Stuff+' = round(median(tj_stuff_plus, na.rm = T))
                     
    ) %>%
    mutate(`%` = round(`%`/sum(`%`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  
  # STATS VS LHH
  stats_vs_l <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) )%>%
    filter(BatterSide=='Left')%>%
    dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                     'K' = sum(grepl('Strikeout', PlayResult)),
                     'BB' = sum(KorBB =="Walk"),
                     'HBP' = sum(PlayResult == 'HitByPitch'),
                     'Whiffs' = sum(whiff==1,na.rm = T),
                     'H' = sum(total_bases > 0,na.rm = T),
                     'R' = sum(RunsScored, na.rm = TRUE),
                     '1B' = sum(PlayResult=='Single'),
                     '2B' = sum(PlayResult=='Double'),
                     '3B' = sum(PlayResult=='Triple'),
                     'HR' = sum(PlayResult=='HomeRun'),
                     XBH = sum(total_bases >1, na.rm = T),
                     'AVG' = round(H / (BF-BB-HBP),3),
                     'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
    dplyr::select(BF, K,Whiffs, BB, HBP, H,XBH, R,HR, AVG, SLG) 
  
  
  
  
  
  pitch_movement_plot <- pitcher_pitch_mvmt_plot(pitcher_data = pitcher_data)
  
  pitch_movement_plot
  
  pvp_game_plot <- pitcher_velocity_plot(pitcher_data)
  
  pvp_game_plot
  
  rolling_stuff<- pitcher_rolling_stuff_plot(pitcher_data)
  rolling_stuff
  # PITCH VELO DISTRIBUTION
  
  pitch_dist_plot <- pitcher_pitch_velo_dist(pitcher_data)
  pitch_dist_plot
  
  plp_rhh <- pitcher_pitch_loc_rhh(pitcher_data)
  plp_rhh
  
  plp_lhh <- pitcher_pitch_loc_lhh(pitcher_data)
  plp_lhh
  
  breakdown <- pitcher_pitch_breakdown(pitcher_data)
  breakdown
  
  spin_axis_plot <- pitcher_spin_axis_plot(pitcher_data)
  spin_axis_plot
  
  arm_angle_plot <- pitcher_arm_angle_plot(pitcher_name = pitcher, pitch_data_df = pitcher_data)
  arm_angle_plot
  
  # SET THE PARAMETERS FOR THE R MARKDOWN FILE
  params <<- list(
    game_summary_table = game_summary_table,
    pitch_usage_table = pitch_usage_table,
    pitch_movement_plot = pitch_movement_plot,
    pitcher = pitcher,
    game_stats = game_stats,
    usage_r = usage_r,
    stats_vs_r = stats_vs_r,
    usage_l = usage_l,
    stats_vs_l= stats_vs_l,
    date = game_date,
    opponent = opponent,
    pvp_game_plot = pvp_game_plot,
    pitch_dist_plot = pitch_dist_plot,
    batted_ball = batted_ball,
    plp_lhh = plp_lhh,
    plp_rhh = plp_rhh,
    spin_axis_plot = spin_axis_plot,
    breakdown =breakdown,
    rolling_stuff = rolling_stuff,
    arm_angle_plot = arm_angle_plot,
    colors_df_ex = colors_df_ex
    
  )
  
  # SETS THE DATE FOR THE FILE NAME
  file_date <<- pitcher_data$Date[1]
  p <<- gsub(" ", "_", pitcher)
  # Knit the R Markdown file to PDF
  rmarkdown::render(input = "postgame pitcher git v2_markdown.Rmd",
                    output_file = paste0(file_date,"_",p, "_report.pdf"),
                    params = params)
}

# master_postgame_boomers_pitcher_report(yakker_day = yakker_day, pitcher)

kable_pitch_char_cond_frmt <- function(df){
  
  df <- df %>%
    column_spec(4, # Velo
                background = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo <= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Velo >= quantiles_summary$Velo[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    column_spec(6, # IVB
                background = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'FB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'FB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$IVB >= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$IVB <= quantiles_summary$IVB[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    # column_spec(7, # HB
    #             background = case_when(
    #               game_summary_table$Pitch == 'FB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][1]) ~ hot_cold[1],
    #               game_summary_table$Pitch == 'FB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][2]) ~ hot_cold[2],
    #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][3]) ~ hot_cold[3],
    #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][4]) ~ hot_cold[4],
    #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][5]) ~ hot_cold[5],
    #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][6]) ~ hot_cold[6],
    #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][6]) ~ hot_cold[7],
    #               game_summary_table$Pitch == 'SI' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][1]) ~ hot_cold[1],
    #               game_summary_table$Pitch == 'SI' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][2]) ~ hot_cold[2],
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][3]) ~ hot_cold[3],
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][4]) ~ hot_cold[4],
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][5]) ~ hot_cold[5],
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][6]) ~ hot_cold[6],
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][6]) ~ hot_cold[7],
  #               game_summary_table$Pitch == 'CT' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][1]) ~ hot_cold[1],
  #               game_summary_table$Pitch == 'CT' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][2]) ~ hot_cold[2],
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][3]) ~ hot_cold[3],
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][4]) ~ hot_cold[4],
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][5]) ~ hot_cold[5],
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][6]) ~ hot_cold[6],
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][6]) ~ hot_cold[7],
  #               game_summary_table$Pitch == 'CB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][1]) ~ hot_cold[1],
  #               game_summary_table$Pitch == 'CB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][2]) ~ hot_cold[2],
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][3]) ~ hot_cold[3],
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][4]) ~ hot_cold[4],
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][5]) ~ hot_cold[5],
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][6]) ~ hot_cold[6],
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][6]) ~ hot_cold[7],
  #               game_summary_table$Pitch == 'SL' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][1]) ~ hot_cold[1],
  #               game_summary_table$Pitch == 'SL' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][2]) ~ hot_cold[2],
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][3]) ~ hot_cold[3],
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][4]) ~ hot_cold[4],
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][5]) ~ hot_cold[5],
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][6]) ~ hot_cold[6],
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][6]) ~ hot_cold[7],
  #               game_summary_table$Pitch == 'CH' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][1]) ~ hot_cold[1],
  #               game_summary_table$Pitch == 'CH' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][2]) ~ hot_cold[2],
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][3]) ~ hot_cold[3],
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][4]) ~ hot_cold[4],
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][5]) ~ hot_cold[5],
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][6]) ~ hot_cold[6],
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][6]) ~ hot_cold[7],
  #               T ~ '#ffffff'
  #             ),
  #             color = case_when(
  #               game_summary_table$Pitch == 'FB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'FB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'FB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='FB'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'SI' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'SI' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'SI' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SI'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CT' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'CT' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CT' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CT'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'CB' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CB' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CB'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'SL' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'SL' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'SL' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='SL'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CH' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][1]) ~ 'white',
  #               game_summary_table$Pitch == 'CH' &  abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][2]) ~ 'white',
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][3]) ~ 'black',
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][4]) ~ 'black',
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][5]) ~ 'black',
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][6]) ~ 'white',
  #               game_summary_table$Pitch == 'CH' &   abs(game_summary_table$HB) <= abs(quantiles_summary$HB[quantiles_summary$TaggedPitchType=='CH'][[1]][6]) ~ 'white',
  #               T ~ '#000000'
  #             )
  # )  %>%
  column_spec(9, # SpinRate
              background = case_when(
                game_summary_table$Pitch == 'FB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'FB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                game_summary_table$Pitch == 'SI' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'SI' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                game_summary_table$Pitch == 'CT' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'CT' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                game_summary_table$Pitch == 'CB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'CB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                game_summary_table$Pitch == 'SL' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'SL' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                game_summary_table$Pitch == 'CH' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                game_summary_table$Pitch == 'CH' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                T ~ '#ffffff'
              ),
              color = case_when(
                game_summary_table$Pitch == 'FB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'FB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'FB' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'SI' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'SI' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'SI' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CT' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'CT' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CT' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'CB' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CB' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'SL' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'SL' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'SL' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CH' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                game_summary_table$Pitch == 'CH' &  game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin <= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                game_summary_table$Pitch == 'CH' &   game_summary_table$Spin >= quantiles_summary$Spin[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                T ~ '#000000'
              )
  ) %>%
    column_spec(15, # EXT
                background = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'FB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'FB' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SI' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SI' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CT' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CT' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CB' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CB' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'SL' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'SL' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  game_summary_table$Pitch == 'CH' &  game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext <= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  game_summary_table$Pitch == 'CH' &   game_summary_table$Ext >= quantiles_summary$Ext[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) 
}


kable_pitch_stats_cond_frmt <- function(df) {
  
  df<- df %>%
    column_spec(6, # Stuff
                background = case_when(
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` <= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Stuff+` >= quantiles_summary$`Stuff+`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    column_spec(7, # zone
                background = case_when(
                  pitch_usage_table$`Zone%` == 0 ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  pitch_usage_table$`Zone%` == 0 ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` <= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Zone%` >= quantiles_summary$`Zone%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    column_spec(9, # chaae
                background = case_when(
                  pitch_usage_table$`Chase%` == 0 ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  pitch_usage_table$`Chase%` == 0 ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` <= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Chase%` >= quantiles_summary$`Chase%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    column_spec(10, # whiff
                background = case_when(
                  pitch_usage_table$`Whiff%` == 0 ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  pitch_usage_table$`Whiff%` == 0 ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` <= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`Whiff%` >= quantiles_summary$`Whiff%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) %>%
    column_spec(11, # csw
                background = case_when(
                  pitch_usage_table$`CSW%` == 0 ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ hot_cold[7],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ hot_cold[1],
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ hot_cold[2],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ hot_cold[3],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ hot_cold[4],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ hot_cold[5],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[6],
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ hot_cold[7],
                  T ~ '#ffffff'
                ),
                color = case_when(
                  pitch_usage_table$`CSW%` == 0 ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'FB' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='FB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SI' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SI'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CT' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CT'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CB' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CB'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'SL' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='SL'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][1] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &  pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][2] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][3] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][4] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][5] ~ 'black',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` <= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  pitch_usage_table$Pitch == 'CH' &   pitch_usage_table$`CSW%` >= quantiles_summary$`CSW%`[quantiles_summary$TaggedPitchType=='CH'][[1]][6] ~ 'white',
                  T ~ '#000000'
                )
    ) 
  
}




# hot_cold <<- c('#242766', '#4e55d9',  '#babeff',  "#ebebeb", '#ffbabf', '#bd3e47', "#780008")
hot_cold <<- c('#0066cc', '#80bfff',  '#cce6ff',  "#ebebeb", '#ffbabf', '#bd3e47', "#780008")

# higher_lower <<- c('#C3B700', '#E7DE55', '#F8FFC1', "#ebebeb", '#E6FFE8', '#A2CEA6', '#00840D')
# higher_lower <<- c('#ff6600', '#ff944d', '#ffd1b3', "#ebebeb", '#E6FFE8', '#A2CEA6', '#00840D')
# higher_lower <<- c('#b300b3', '#ff80ff', '#ffccff', "#ebebeb",  '#E6FFE8', '#A2CEA6', '#00840D')
# higher_lower <<- c('#0066cc', '#80bfff', '#cce6ff', "#ebebeb", '#ffd1b3', '#ff944d', '#ff6600')
higher_lower <<- c('#0066cc', '#80bfff',  '#cce6ff',  "#ebebeb", '#ffbabf', '#bd3e47', "#780008")

calculate_quantiles <- function(data, col) {
  quantile(data[[col]], c(.1, .3, .45, .55, .70, .9), na.rm = TRUE)
}

quantiles_summary <- readRDS('quantiles_summary.RDS') 


get_quantiles <- function(pitch_type, quantiles_summary, velo_or_spin) {
  quantiles_summary %>%
    filter(TaggedPitchType == pitch_type) %>%
    select(starts_with(velo_or_spin)) %>%
    unlist() %>%
    as.numeric()
}

colors_df_ex <<- t(data.frame(hot_cold, higher_lower)) %>%
  as.data.frame() %>%
  mutate(Color_code = c('better/worse\nthan avg', 'higher/lower\nthan avg'), .before = 1) %>%
  mutate(V1 = '< 10%',
         V2 = '10% - 30%',
         V3 = '30% - 45%',
         V4 = '45% - 55%',
         V5 = '55% - 70%',
         V6 = '70% - 90%',
         V7 = '> 90%'
  )

kable_pitch_stats_cond_frmt_hot_cold <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- hot_cold
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#ffffff'
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'
    )
  }
  
  df <- df %>%
    column_spec(6, # Stuff+
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Stuff+`, MoreArgs = list(metric_type = 'Stuff+')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Stuff+`, MoreArgs = list(metric_type = 'Stuff+'))) %>%
    column_spec(10, # Whiff%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Whiff%`, MoreArgs = list(metric_type = 'Whiff%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Whiff%`, MoreArgs = list(metric_type = 'Whiff%')))%>%
    column_spec(11, # CSW%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`CSW%`, MoreArgs = list(metric_type = 'CSW%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`CSW%`, MoreArgs = list(metric_type = 'CSW%')))
  
  
  df
}


kable_pitch_metrics_cond_frmt_hot_cold <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- hot_cold
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#FFFFFF'  # Fallback color for unexpected cases
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'  # Fallback color for unexpected cases
    )
  }
  
  
  df <- df %>%
    column_spec(4, # Velo
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Velo`, MoreArgs = list(metric_type = 'Velo')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Velo`, MoreArgs = list(metric_type = 'Velo')))  %>%
    column_spec(15, # Ext
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Ext`, MoreArgs = list(metric_type = 'Ext')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Ext`, MoreArgs = list(metric_type = 'Ext')))
  
  
  df
}


kable_pitch_stats_cond_frmt_hi_low <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- higher_lower
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#ffffff'
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'
    )
  }
  
  df <- df %>%
    column_spec(7, # Zone%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Zone%`, MoreArgs = list(metric_type = 'Zone%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Zone%`, MoreArgs = list(metric_type = 'Zone%'))) %>%
    column_spec(8, # Strk%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Strk%`, MoreArgs = list(metric_type = 'Strk%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Strk%`, MoreArgs = list(metric_type = 'Strk%'))) %>%
    column_spec(9, # Chase%
                background = mapply(get_background_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Chase%`, MoreArgs = list(metric_type = 'Chase%')),
                color = mapply(get_text_color, as.character(pitch_usage_table$Pitch), pitch_usage_table$`Chase%`, MoreArgs = list(metric_type = 'Chase%')))
  
  df
}


kable_pitch_metrics_cond_frmt_hi_low <- function(df) {
  
  get_background_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    colors <- higher_lower
    case_when(
      metric == 0 ~ 'white',
      metric <= quantiles[1] ~ colors[1],
      metric <= quantiles[2] ~ colors[2],
      metric <= quantiles[3] ~ colors[3],
      metric <= quantiles[4] ~ colors[4],
      metric <= quantiles[5] ~ colors[5],
      metric <= quantiles[6] ~ colors[6],
      metric >= quantiles[6] ~ colors[7],
      TRUE ~ '#ffffff'
    )
  }
  
  get_text_color <- function(pitch, metric, metric_type) {
    quantiles <- quantiles_summary[[metric_type]][as.character(quantiles_summary$TaggedPitchType) == as.character(pitch)][[1]]
    case_when(
      metric == 0 ~ 'black',
      metric <= quantiles[1] ~ 'white',
      metric <= quantiles[4] ~ 'black',
      metric >= quantiles[5] ~ 'white',
      TRUE ~ 'black'
    )
  }
  
  df <- df %>%
    column_spec(6, # IVB
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`IVB`, MoreArgs = list(metric_type = 'IVB')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`IVB`, MoreArgs = list(metric_type = 'IVB')))%>%
    column_spec(9, # Spin
                background = mapply(get_background_color, as.character(game_summary_table$Pitch), game_summary_table$`Spin`, MoreArgs = list(metric_type = 'Spin')),
                color = mapply(get_text_color, as.character(game_summary_table$Pitch), game_summary_table$`Spin`, MoreArgs = list(metric_type = 'Spin')))
  
  df
}
