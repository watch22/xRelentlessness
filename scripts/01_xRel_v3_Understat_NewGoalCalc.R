pacman::p_load(tidyverse, zoo, here)


#Import Understat shot data for Serie A 18/19

seriea_1819 <- read_csv("/Users/SikSik/Documents/Data Science/Datasets/Understat/Datasets/Serie A/shots_serie_a_18-19.csv")
 #clean up data ----
seriea_1819_clean <- seriea_1819 %>% 
  arrange(date, match_id, minute) %>% 
  select(date, match_id, minute, h_team, a_team, h_a, everything()) %>% 
  select(-c(season, X, Y, id, player_id))


#split xG by state of game (drawing, losing, ahead by 1,2, or more goals) ----
seriea_1819_rlnt <- seriea_1819_clean %>% 
  group_by(match_id) %>% 
  select(2:8, 12:13) %>% 
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
                                TRUE ~ lag(cumsum(goal_h))),
         sum_goal_a = case_when(is.na(lag(cumsum(goal_a))) ~ 0,
                                row_number() == max(row_number()) ~ cumsum(goal_a),      
                                TRUE ~ lag(cumsum(goal_a))),
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
  select(match_id, h_team, a_team, h_a, xG, everything())

#check algorithm
seriea_1819_rlnt %>% select(4:5,9,11,13,15:19) %>% filter(match_id == 9867 & h_a == "h") %>% group_by(match_id) %>% 
       summarise_at(vars(5:10), ~ sum(.x))

seriea_1819_rlnt %>% select(4:5,10,12,14,20:24) %>% filter(match_id == 9867 & h_a == "a") %>% 
group_by(match_id) %>% 
  summarise_at(vars(5:10), ~ sum(.x))

#group games and get totals
rlnt_all_1819 <- seriea_1819_rlnt %>% 
  group_by(match_id, h_team, a_team) %>% 
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

#split home and away ----
rlnt_1819_h <- rlnt_all_1819 %>% 
  ungroup() %>% 
  select(1,2,4,6:11)

rlnt_1819_a <- rlnt_all_1819 %>% 
  ungroup() %>% 
  select(1,3,5,12:17)

colnames(rlnt_1819_h) <- c("match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")

colnames(rlnt_1819_a) <- c("match_id", "team", "goals", "total_xG","total_xG_behind", "total_xG_level", "total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")
  
rlnt_all_1819_by_team <- rbind(rlnt_1819_h, rlnt_1819_a)

rlnt_all_1819_by_team %>% 
  select(-1) %>% 
  #rowwise %>% 
  #mutate(check = sum(c_across(4:8))) %>% 
  group_by(team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  arrange(desc(total_xG)) %>% 
  select(1,4:8,3) %>% 
  mutate(`xG_behind_%` = total_xG_behind * 100/total_xG,
         `xG_level_%` = total_xG_level * 100/total_xG,
         `xG_ahead_1_%` = total_xG_ahead_1 * 100/total_xG,
         `xG_ahead_2_%` = total_xG_ahead_2 * 100/total_xG,
         `xG_ahead_N_%` = total_xG_ahead_N * 100/total_xG,
         `total_xG_ahead_%` = `xG_ahead_1_%` + `xG_ahead_2_%` + `xG_ahead_N_%`) %>% 
  select(1,8:13) %>% 
  arrange(desc(`total_xG_ahead_%`))

#get minutes per game state ----
seriea_1819_rlnt_min <- seriea_1819_rlnt %>%
  select(1:3,6,10:14) %>% 
  distinct() %>% 
  mutate(
    # FIX DUPLICATE MINUTES
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

seriea_1819_rlnt_min %>% select(1:9,10,13,11,14,12,15)

View(seriea_1819_rlnt_min %>% filter(match_id %in% check_matches$match_id))

#get totals
rlnt_min_totals <- seriea_1819_rlnt_min %>% 
  group_by(match_id, h_team, a_team) %>% 
  summarise(tot_min_ahead_h = sum(lead_time_h),
            tot_min_ahead_a = sum(lead_time_a))

check_matches <- rlnt_min_totals %>% filter(tot_min_ahead_h < 0 | tot_min_ahead_a < 0 | tot_min_ahead_h > 90 | tot_min_ahead_a > 90 )



#2. get xG per minute ahead
#3. replicate for past 5 seasons of top 5 leagues

