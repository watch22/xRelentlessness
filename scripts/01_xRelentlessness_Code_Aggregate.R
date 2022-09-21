#load packages & functions ----
pacman::p_load(tidyverse, extrafont, ggupset, tibbletime, ggtext, ggrepel, glue, here, zoo, knitr, kableExtra, ggforce)
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.001 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

#import data ----
match_df <- list.files(path = here::here("data"), full.names = T, recursive = T)
season_df <- tibble(File = match_df) %>%
  extract(File, "Site", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data)

#clean data ----
season_clean <- season_df %>% 
  rename(competition = File) %>% 
  mutate(competition = str_split(basename(competition),"_")) %>% 
  rowwise() %>% 
  mutate(competition = competition[[1]][1]) %>% 
  mutate(competition = case_when(competition == "bundesliga" ~ "Bundesliga",
                                 competition == "epl" ~ "Premier League",
                                 competition == "seriea" ~ "Serie A",
                                 competition == "laliga" ~ "La Liga",
                                 competition == "ligue1" ~ "Ligue 1"),
         season = case_when(season == 2016 ~ "2016/17",
                            season == 2017 ~ "2017/18",
                            season == 2018 ~ "2018/19",
                            season == 2019 ~ "2019/20",
                            season == 2020 ~ "2020/21")) %>% 
  arrange(date, match_id, minute) %>% 
  select(competition, season, date, match_id, minute, h_team, a_team, h_a, everything()) %>% 
  select(-c(X, Y, id, player_id))

#1. Get xG per Game State ----
rlnt_xg <- season_clean %>% 
  group_by(match_id) %>% 
  select(1,2, 4:11, 15:16) %>% 
  select(-c(Site)) %>% 
  mutate(minute = minute,
         h_a = case_when(result == "OwnGoal" & h_a == "h" ~ "a",
                         result == "OwnGoal" & h_a == "a" ~ "h",
                         TRUE ~ h_a),
         xG = round(xG, 3),
         #set goal flags --
         goal = ifelse(result == "Goal" | result == "OwnGoal", 1, 0),
         goal_h = case_when(goal == 1 & h_a == "h" ~ 1,
                            TRUE ~ 0),
         goal_a = case_when(goal == 1 & h_a == "a" ~ 1,
                            TRUE ~ 0),
         sum_goal_h = case_when(is.na(lag(cumsum(goal_h))) ~ 0,
                                row_number() == max(row_number()) ~ cumsum(goal_h),      
                                TRUE ~ cumsum(goal_h)),
         sum_goal_a = case_when(is.na(lag(cumsum(goal_a))) ~ 0,
                                row_number() == max(row_number()) ~ cumsum(goal_a),      
                                TRUE ~ cumsum(goal_a)),
         #calculate various xg --
         xg_h = ifelse(h_a == "h", round(xG,3), 0),
         xg_a = ifelse(h_a == "a", round(xG,3), 0),
         xg_behind_h = ifelse(sum_goal_h - sum_goal_a < 0, xg_h,0),
         xg_draw_h = ifelse(sum_goal_h - sum_goal_a == 0, xg_h,0),
         xg_ahead1_h = ifelse(sum_goal_h - sum_goal_a == 1, xg_h,0),
         xg_ahead2_h = ifelse(sum_goal_h - sum_goal_a == 2, xg_h,0),
         xg_aheadn_h = ifelse(sum_goal_h - sum_goal_a > 2, xg_h,0),
         xg_behind_a = ifelse(sum_goal_a - sum_goal_h < 0, xg_a,0),
         xg_draw_a = ifelse(sum_goal_a - sum_goal_h == 0, xg_a,0),
         xg_ahead1_a = ifelse(sum_goal_a - sum_goal_h == 1, xg_a,0),
         xg_ahead2_a = ifelse(sum_goal_a - sum_goal_h == 2, xg_a,0),
         xg_aheadn_a = ifelse(sum_goal_a - sum_goal_h > 2, xg_a,0)) %>% 
  select(competition, season, match_id, h_team, a_team, h_a, xG, everything())

#group games and get totals
rlnt_xg_totals <- rlnt_xg %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(goals_h = sum(goal_h),
            goals_a = sum(goal_a),
            tot_xg_h = sum(xg_h),
            tot_behind_h = sum(xg_behind_h),
            tot_draw_h = sum(xg_draw_h),
            tot_ahead1_h = sum(xg_ahead1_h),
            tot_ahead2_h = sum(xg_ahead2_h),
            tot_aheadn_h = sum(xg_aheadn_h),
            tot_xg_a = sum(xg_a),
            tot_behind_a = sum(xg_behind_a),
            tot_draw_a = sum(xg_draw_a),
            tot_ahead1_a = sum(xg_ahead1_a),
            tot_ahead2_a = sum(xg_ahead2_a),
            tot_aheadn_a = sum(xg_aheadn_a))

#split home and away
rlnt_xg_h <- rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:4,6,8:13)

rlnt_xg_a <- rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:3,5,7,14:19)

colnames(rlnt_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")

colnames(rlnt_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG","total_xG_behind", "total_xG_level", "total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")

rlnt_xg_all <- rbind(rlnt_xg_h, rlnt_xg_a) %>% 
  left_join(league_tables %>% select(2:4), by = c("team", "season"))

#Switch to weighted xG model ----
wt_rlnt_xg <- season_clean %>% 
  group_by(match_id) %>% 
  select(1,2, 4:11, 15:16) %>% 
  select(-c(Site)) %>% 
  mutate(minute = minute,
         h_a = case_when(result == "OwnGoal" & h_a == "h" ~ "a",
                         result == "OwnGoal" & h_a == "a" ~ "h",
                         TRUE ~ h_a),
         xG = round(xG, 3),
         #set goal flags --
         goal = ifelse(result == "Goal" | result == "OwnGoal", 1, 0),
         goal_h = case_when(goal == 1 & h_a == "h" ~ 1,
                            TRUE ~ 0),
         goal_a = case_when(goal == 1 & h_a == "a" ~ 1,
                            TRUE ~ 0),
         sum_goal_h = case_when(is.na(lag(cumsum(goal_h))) ~ 0,
                                row_number() == max(row_number()) ~ cumsum(goal_h),      
                                TRUE ~ cumsum(goal_h)),
         sum_goal_a = case_when(is.na(lag(cumsum(goal_a))) ~ 0,
                                row_number() == max(row_number()) ~ cumsum(goal_a),      
                                TRUE ~ cumsum(goal_a)),
         #calculate various xg --
         sum_gd_h = sum_goal_h - sum_goal_a,
         sum_gd_a = sum_goal_a - sum_goal_h,
         xg_h = ifelse(h_a == "h", round(xG,3), 0),
         xg_a = ifelse(h_a == "a", round(xG,3), 0),
         xg_behind_h = ifelse(sum_goal_h - sum_goal_a < 0, xg_h,0),
         xg_draw_h = ifelse(sum_goal_h == sum_goal_a, xg_h,0),
         xg_ahead_h = ifelse(sum_goal_h - sum_goal_a > 0, xg_h,0),
         weight_factor_h = case_when(sum_gd_h > 0 ~ (sum_gd_h/10) + 1,
                                     TRUE ~ 1),
         xg_ahead_h_wt = ifelse(sum_goal_h - sum_goal_a > 0, xg_h * weight_factor_h,0),
         xg_behind_a = ifelse(sum_goal_a - sum_goal_h < 0, xg_a,0),
         xg_draw_a = ifelse(sum_goal_a == sum_goal_h, xg_a,0),
         xg_ahead_a = ifelse(sum_goal_a - sum_goal_h > 0, xg_a,0),
         weight_factor_a = case_when(sum_gd_a > 0 ~ 1 + (sum_gd_a/10),
                                     TRUE ~ 1),
         xg_ahead_a_wt = ifelse(sum_goal_a - sum_goal_h > 0, xg_a  * weight_factor_a,0)) %>% 
  select(competition, season, match_id, h_team, a_team, h_a, xG, everything())

#group games and get totals
wt_rlnt_xg_totals <- wt_rlnt_xg %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(goals_h = sum(goal_h),
            goals_a = sum(goal_a),
            tot_xg_h = sum(xg_h),
            tot_behind_h = sum(xg_behind_h),
            tot_draw_h = sum(xg_draw_h),
            tot_ahead_h = sum(xg_ahead_h),
            tot_ahead_h_wt = sum(xg_ahead_h_wt),
            wt_xg_diff_h = ifelse(tot_ahead_h != 0, round((tot_ahead_h_wt - tot_ahead_h) * 100/tot_ahead_h,3),0),
            tot_xg_a = sum(xg_a),
            tot_behind_a = sum(xg_behind_a),
            tot_draw_a = sum(xg_draw_a),
            tot_ahead_a = sum(xg_ahead_a),
            tot_ahead_a_wt = sum(xg_ahead_a_wt),
            twt_xg_diff_a = ifelse(tot_ahead_a != 0, round((tot_ahead_a_wt - tot_ahead_a) * 100/tot_ahead_a,3),0))

#split home and away
wt_rlnt_xg_h <- wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:4,6,8:13)

wt_rlnt_xg_a <- wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:3,5,7,14:19)

colnames(wt_rlnt_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead", "weighted_xG_ahead", "percentage_diff_ahead")

colnames(wt_rlnt_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead", "weighted_xG_ahead", "percentage_diff_ahead")

wt_rlnt_xg_all <- rbind(wt_rlnt_xg_h, wt_rlnt_xg_a)%>% 
  left_join(league_tables %>% select(2:4), by = c("team", "season"))

#2. GET MINUTES PER STATE ----
wt_rlnt_min <- wt_rlnt_xg %>%
  select(1:5,8,10:16,24,29) %>% 
  # FIX DUPLICATE MINUTES
  mutate(minute = c(minute[-n()], max(minute) + 1))

wt_rlnt_min <- wt_rlnt_min %>% 
  distinct() %>% 
  mutate(
    #HOME STATE AND MINUTE COUNTER
    state_h = case_when(sum_goal_h == sum_goal_a ~ "level",
                        (goal_h == 1 & lead(sum_goal_h) > lead(sum_goal_a)) | sum_goal_h > sum_goal_a ~ "lead",
                        sum_goal_h < sum_goal_a ~ "behind"),
    min_ahead_h = case_when(state_h == "lead" & minute == 0 ~ 1,
                            state_h == "lead" & lag(state_h) == "level" | state_h == "lead" & minute == min(minute) ~ minute,
                            (state_h == "level" | state_h == "behind") & minute == min(minute) ~ 999,
                            state_h == "lead" & lag(state_h) == "level" ~ minute,
                            state_h == "level" & lag(state_h) == "lead" ~ minute,
                            state_h == "behind" | (state_h == "level" & lag(state_h) == "level") | (state_h == "level" & lag(state_h) == "behind") ~ 999, 
                            TRUE ~ 0),
    min_ahead_h = na_if(min_ahead_h,0),
    min_ahead_h = na.locf(min_ahead_h, na.rm = FALSE),
    lead_time_h = case_when(state_h == "level" & lag(state_h) == "lead" ~ min_ahead_h - lag(min_ahead_h),
                            state_h == "lead" & minute == max(minute) ~ max(minute) - min_ahead_h,
                            TRUE ~ 0),
    #AWAY STATE AND MINUTE COUNTER
    state_a = case_when(sum_goal_a == sum_goal_h ~ "level",
                        (goal_a == 1 & lead(sum_goal_a) > lead(sum_goal_h)) | sum_goal_a > sum_goal_h ~ "lead",
                        sum_goal_a < sum_goal_h ~ "behind"),
    min_ahead_a = case_when(state_a == "lead" & minute == 0 ~ 1,
                            state_a == "lead" & lag(state_a) == "level" | state_a == "lead" & minute == min(minute) ~ minute,
                            (state_a == "level" | state_a == "behind") & minute == min(minute) ~ 999,
                            state_a == "lead" & lag(state_a) == "level" ~ minute,
                            state_a == "level" & lag(state_a) == "lead" ~ minute,
                            state_a == "behind" | (state_a == "level" & lag(state_a) == "level") | (state_a == "level" & lag(state_a) == "behind") ~ 999, 
                            TRUE ~ 0),
    min_ahead_a = na_if(min_ahead_a,0),
    min_ahead_a = na.locf(min_ahead_a, na.rm = FALSE),
    lead_time_a = case_when(state_a == "lead" & minute == max(minute) ~ max(minute) - min_ahead_a,
                            state_a == "level" & lag(state_a) == "lead" ~ min_ahead_a - lag(min_ahead_a),
                            TRUE ~ 0))


#GET TOTALS -----
wt_rlnt_min_totals <- wt_rlnt_min %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(tot_min_ahead_h = sum(lead_time_h),
            tot_min_ahead_a = sum(lead_time_a))

#CHECK VALUES -----
wt_rlnt_min_totals %>% filter(tot_min_ahead_h > 100 |
                                tot_min_ahead_a > 100 |
                                tot_min_ahead_h < 0 |
                                tot_min_ahead_a < 0 |
                                is.na(tot_min_ahead_h) |
                                is.na(tot_min_ahead_a))

#split home and away ----
wt_rlnt_min_h <- wt_rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:4,6)

wt_rlnt_min_a <- wt_rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:3, 5 ,7)

colnames(wt_rlnt_min_h) <- c("competition","season","match_id", "team", "min_ahead")

colnames(wt_rlnt_min_a) <- c("competition","season","match_id", "team", "min_ahead")

wt_rlnt_min_all <- rbind(wt_rlnt_min_h, wt_rlnt_min_a)

#Join tables ----
wt_rlnt_all <- left_join(wt_rlnt_xg_all, select(wt_rlnt_min_all,c(3:5)), by = c("match_id", "team"))

#Get xGpMA values ----
  wt_rlnt_all_by_team_season <- wt_rlnt_all %>% 
  group_by(competition, team, season, Pos) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(xGpMA = round(total_xG_ahead/min_ahead,4),
         XGpMA_wt = round(weighted_xG_ahead/min_ahead,4),
         percent_diff = (signif((XGpMA_wt-xGpMA)/xGpMA,3)*100)) %>% 
  arrange(desc(percent_diff), desc(XGpMA_wt)) %>% 
  select(1:4,10,11,13:16) %>% 
  left_join(league_tables %>% select(competition, season, team, M), by = c("competition", "season", "team")) %>% 
  distinct() %>% 
  #normalise values per average minutes ahead per match
  mutate(min_ahead_per_game = round(min_ahead/M,1),
         normalised_xGpMA = round(total_xG_ahead/min_ahead_per_game,2),
         normalised_xGpMA_wt = round(weighted_xG_ahead/min_ahead_per_game,2),
         normalised_percent_diff = (signif((normalised_xGpMA_wt-normalised_xGpMA)/normalised_xGpMA,3)*100))