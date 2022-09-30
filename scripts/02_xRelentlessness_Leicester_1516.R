lei1516_df <- read_csv(here::here("data","understat_shot_xg","epl_15-16.csv"))

lei1516_df <- lei1516_df %>% mutate(season = "2015/16",
                                    competition = "Premier League") %>% 
  arrange(date, match_id, minute) %>% 
  select(competition, season, date, match_id, minute, h_team, a_team, h_a, everything()) %>% 
  select(-c(X, Y, id, player_id))

lei1516_df <- lei1516_df %>% 
  filter(h_team == "Leicester" | a_team == "Leicester")

lei_xg <- lei1516_df %>% 
  select(1,2, 4:10, 14:15) %>% 
  group_by(match_id) %>% 
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
         weight_factor_h = case_when(sum_gd_h > 0 ~ (sum_gd_h/10) + 1,
                                     TRUE ~ 1),
         weight_factor_a = case_when(sum_gd_a > 0 ~ 1 + (sum_gd_a/10),
                                     TRUE ~ 1),
         #home team - xG
         xg_behind_h = ifelse(sum_goal_h - sum_goal_a < 0, xg_h,0),
         xg_draw_h = ifelse(sum_goal_h == sum_goal_a, xg_h,0),
         xg_ahead_h = ifelse(sum_goal_h - sum_goal_a > 0, xg_h,0),
         xg_one_ahead_h = ifelse(sum_goal_h - sum_goal_a == 1, xg_h,0),
         xg_ahead_h_wt = ifelse(sum_goal_h - sum_goal_a > 0, xg_h * weight_factor_h,0),
         #away team - xG
         xg_behind_a = ifelse(sum_goal_a - sum_goal_h < 0, xg_a,0),
         xg_draw_a = ifelse(sum_goal_a == sum_goal_h, xg_a,0),
         xg_ahead_a = ifelse(sum_goal_a - sum_goal_h > 0, xg_a,0),
         xg_one_ahead_a = ifelse(sum_goal_a - sum_goal_h == 1, xg_a,0),
         xg_ahead_a_wt = ifelse(sum_goal_a - sum_goal_h > 0, xg_a  * weight_factor_a,0),
         #home team - xGA
         xga_behind_h = xg_ahead_a,
         xga_ahead_h = xg_behind_a,
         xga_one_ahead_h = ifelse(sum_goal_h - sum_goal_a == 1, xg_a,0),
         xga_ahead_h_wt = ifelse(sum_goal_h - sum_goal_a > 0, xg_a / weight_factor_h,0),
         #away team - xGA
         xga_behind_a = xg_ahead_h,
         xga_ahead_a = xg_behind_h,
         xga_one_ahead_a = ifelse(sum_goal_h - sum_goal_a == -1, xg_h,0),
         xga_ahead_a_wt = ifelse(sum_goal_a - sum_goal_h > 0, xg_h  / weight_factor_a,0)) %>% 
  select(competition, season, match_id, h_team, a_team, h_a, xG, everything())

lei_xg_total <- lei_xg %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(goals_h = sum(goal_h),
            goals_a = sum(goal_a),
            #home - xG
            tot_xg_h = sum(xg_h),
            tot_behind_h = sum(xg_behind_h),
            tot_draw_h = sum(xg_draw_h),
            tot_ahead_h = sum(xg_ahead_h),
            tot_one_ahead_h = sum(xg_one_ahead_h),
            tot_ahead_h_wt = sum(xg_ahead_h_wt),
            wt_xg_diff_h = ifelse(tot_ahead_h != 0, round((tot_ahead_h_wt - tot_ahead_h) * 100/tot_ahead_h,3),0),
            #home - xGA
            tot_a_behind_h = sum(xga_behind_h),
            tot_a_ahead_h = sum(xga_ahead_h),
            tot_a_ahead_h_wt = sum(xga_ahead_h_wt),
            tot_a_one_ahead_h = sum(xga_one_ahead_h),
            wt_xga_diff_h = ifelse(tot_ahead_h != 0, round((- tot_a_ahead_h_wt + tot_a_ahead_h) * 100/tot_a_ahead_h,3),0),
            #away - xG
            tot_xg_a = sum(xg_a),
            tot_behind_a = sum(xg_behind_a),
            tot_draw_a = sum(xg_draw_a),
            tot_ahead_a = sum(xg_ahead_a),
            tot_one_ahead_a = sum(xg_one_ahead_a),
            tot_ahead_a_wt = sum(xg_ahead_a_wt),
            wt_xg_diff_a = ifelse(tot_ahead_a != 0, round((tot_ahead_a_wt - tot_ahead_a) * 100/tot_ahead_a,3),0),
            #away - xGA
            tot_a_behind_a = sum(xga_behind_a),
            tot_a_ahead_a = sum(xga_ahead_a),
            tot_a_ahead_a_wt = sum(xga_ahead_a_wt),
            tot_a_one_ahead_a = sum(xga_one_ahead_a),
            wt_xga_diff_a = ifelse(tot_ahead_a != 0, round((- tot_a_ahead_a_wt + tot_a_ahead_a) * 100/tot_a_ahead_a,3),0))

lei_xg_h <- lei_xg_total %>% 
  ungroup() %>% 
  select(1:4,6,8:19)

lei_xg_a <- lei_xg_total %>% 
  ungroup() %>% 
  select(1:3,5,7,20:31)

colnames(lei_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind","total_xG_level", "total_xG_ahead","total_xG_one_ahead", "weighted_xG_ahead","percentage_diff_ahead", "total_xGA_behind", "total_xGA_ahead", "weighted_xGA_ahead", "total_xGA_one_ahead", "percentage_diff_xGA_ahead")

colnames(lei_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind","total_xG_level", "total_xG_ahead","total_xG_one_ahead", "weighted_xG_ahead","percentage_diff_ahead", "total_xGA_behind", "total_xGA_ahead", "weighted_xGA_ahead", "total_xGA_one_ahead", "percentage_diff_xGA_ahead")

lei_xg_all <- rbind(lei_xg_h, lei_xg_a)%>% 
  mutate(Pos = 1)

lei_min <- lei_xg %>%
  select(1:5,8,10:16) %>% 
  mutate(minute = c(minute[-n()], max(minute) + 1)) %>% 
  distinct() %>% 
  mutate(
    #HOME STATE AND MINUTE COUNTER
    #SET HOME STATE ----
    state_h = case_when(sum_goal_h == sum_goal_a ~ "level",
                        (goal_h == 1 & (lead(sum_goal_h) - lead(sum_goal_a) == 1)) | (sum_goal_h - sum_goal_a == 1) ~ "lead_1",
                        (goal_h == 1 & (lead(sum_goal_h) - lead(sum_goal_a) == 2)) | (sum_goal_h - sum_goal_a == 2) ~ "lead_2",
                        (goal_h == 1 & (lead(sum_goal_h) - lead(sum_goal_a) > 2)) | (sum_goal_h - sum_goal_a > 2) ~ "lead_n",
                        sum_goal_h < sum_goal_a ~ "behind"),
    #TIME LEADING BY 1 ----
    min_ahead1_h = case_when(state_h == "lead_1" & minute == 0 ~ 1,
                             state_h == "lead_1" & lag(state_h) != "lead_1" | state_h == "lead_1" & minute == min(minute) | state_h != "lead_1" & lag(state_h) == "lead_1" ~ minute,
                             (state_h %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_h == "behind" | (state_h == "level" & lag(state_h) == "level") | (state_h == "level" & lag(state_h) == "behind") | state_h == "lead_2" & lag(state_h) == "lead_2"  ~ 999, 
                             TRUE ~ 0),
    min_ahead1_h = na_if(min_ahead1_h,0),
    min_ahead1_h = na.locf(min_ahead1_h, na.rm = FALSE),
    lead_time1_h = case_when(state_h != "lead_1" & lag(state_h) == "lead_1" ~ min_ahead1_h - lag(min_ahead1_h),
                             state_h == "lead_1" & minute == max(minute) ~ max(minute) - min_ahead1_h,
                             TRUE ~ 0),
    #TIME LEADING BY 2 ----
    min_ahead2_h = case_when(state_h == "lead_2" & lag(state_h) != "lead_2" | state_h != "lead_2" & lag(state_h) == "lead_2" ~ minute,
                             (state_h %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_h == "behind" | (state_h == "level" & lag(state_h) == "level") | (state_h == "level" & lag(state_h) == "behind") | state_h == "lead_n" & lag(state_h) == "lead_n"  ~ 999, 
                             TRUE ~ 0),
    min_ahead2_h = na_if(min_ahead2_h,0),
    min_ahead2_h = na.locf(min_ahead2_h, na.rm = FALSE),
    lead_time2_h = case_when(state_h != "lead_2" & lag(state_h) == "lead_2" ~ min_ahead2_h - lag(min_ahead2_h),
                             state_h == "lead_2" & minute == max(minute) ~ max(minute) - min_ahead2_h,
                             TRUE ~ 0),
    #TIME LEADING BY 3 or more ----
    min_aheadn_h = case_when(state_h == "lead_n" & lag(state_h) != "lead_n" | state_h != "lead_n" & lag(state_h) == "lead_n" ~ minute,
                             (state_h %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_h != "lead_n" & lag(state_h) != "lead_n"  ~ 999, 
                             TRUE ~ 0),
    min_aheadn_h = na_if(min_aheadn_h,0),
    min_aheadn_h = na.locf(min_aheadn_h, na.rm = FALSE),
    lead_timen_h = case_when(state_h != "lead_n" & lag(state_h) == "lead_n" ~ min_aheadn_h - lag(min_aheadn_h),
                             state_h == "lead_n" & minute == max(minute) ~ max(minute) - min_aheadn_h,
                             TRUE ~ 0),
    tot_lead_time_h = lead_time1_h + lead_time2_h + lead_timen_h,
    #SET AWAY STATE ----
    state_a = case_when(sum_goal_h == sum_goal_a ~ "level",
                        (goal_a == 1 & (lead(sum_goal_h) - lead(sum_goal_a) == -1)) | (sum_goal_h - sum_goal_a == -1) ~ "lead_1",
                        (goal_a == 1 & (lead(sum_goal_h) - lead(sum_goal_a) == -2)) | (sum_goal_h - sum_goal_a == -2) ~ "lead_2",
                        (goal_a == 1 & (lead(sum_goal_h) - lead(sum_goal_a) < -2)) | (sum_goal_h - sum_goal_a < -2) ~ "lead_n",
                        sum_goal_h > sum_goal_a ~ "behind"),
    #TIME LEADING BY 1 ----
    min_ahead1_a = case_when(state_a == "lead_1" & minute == 0 ~ 1,
                             state_a == "lead_1" & lag(state_a) != "lead_1" | state_a == "lead_1" & minute == min(minute) | state_a != "lead_1" & lag(state_a) == "lead_1" ~ minute,
                             (state_a %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_a == "behind" | (state_a == "level" & lag(state_a) == "level") | (state_a == "level" & lag(state_a) == "behind") | state_a == "lead_2" & lag(state_a) == "lead_2"  ~ 999, 
                             TRUE ~ 0),
    min_ahead1_a = na_if(min_ahead1_a,0),
    min_ahead1_a = na.locf(min_ahead1_a, na.rm = FALSE),
    lead_time1_a = case_when(state_a != "lead_1" & lag(state_a) == "lead_1" ~ min_ahead1_a - lag(min_ahead1_a),
                             state_a == "lead_1" & minute == max(minute) ~ max(minute) - min_ahead1_a,
                             TRUE ~ 0),
    #TIME LEADING BY 2 ----
    min_ahead2_a = case_when(state_a == "lead_2" & lag(state_a) != "lead_2" | state_a != "lead_2" & lag(state_a) == "lead_2" ~ minute,
                             (state_a %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_a == "behind" | (state_a == "level" & lag(state_a) == "level") | (state_a == "level" & lag(state_a) == "behind") | state_a == "lead_n" & lag(state_a) == "lead_n"  ~ 999, 
                             TRUE ~ 0),
    min_ahead2_a = na_if(min_ahead2_a,0),
    min_ahead2_a = na.locf(min_ahead2_a, na.rm = FALSE),
    lead_time2_a = case_when(state_a != "lead_2" & lag(state_a) == "lead_2" ~ min_ahead2_a - lag(min_ahead2_a),
                             state_a == "lead_2" & minute == max(minute) ~ max(minute) - min_ahead2_a,
                             TRUE ~ 0),
    #TIME LEADING BY 3 or more ----
    min_aheadn_a = case_when(state_a == "lead_n" & lag(state_a) != "lead_n" | state_a != "lead_n" & lag(state_a) == "lead_n" ~ minute,
                             (state_a %in% c("behind", "level")) & minute == min(minute) ~ 999,
                             state_a != "lead_n" & lag(state_a) != "lead_n"  ~ 999, 
                             TRUE ~ 0),
    min_aheadn_a = na_if(min_aheadn_a,0),
    min_aheadn_a = na.locf(min_aheadn_a, na.rm = FALSE),
    lead_timen_a = case_when(state_a != "lead_n" & lag(state_a) == "lead_n" ~ min_aheadn_a - lag(min_aheadn_a),
                             state_a == "lead_n" & minute == max(minute) ~ max(minute) - min_aheadn_a,
                             TRUE ~ 0),
    tot_lead_time_a = lead_time1_a + lead_time2_a + lead_timen_a)



#Group and get totals ----
lei_min_totals <- lei_min %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(tot_min_ahead1_h = sum(lead_time1_h),
            tot_min_ahead2_h = sum(lead_time2_h),
            tot_min_aheadn_h = sum(lead_timen_h),
            total_min_ahead_h = sum(tot_lead_time_h),
            tot_min_ahead1_a = sum(lead_time1_a),
            tot_min_ahead2_a = sum(lead_time2_a),
            tot_min_aheadn_a = sum(lead_timen_a),
            total_min_ahead_a = sum(tot_lead_time_a))

lei_min_h <- lei_min_totals %>% 
  ungroup() %>% 
  select(1:4,6:9)

lei_min_a <- lei_min_totals %>% 
  ungroup() %>% 
  select(1:3, 5 ,10:13)

colnames(lei_min_h) <- c("competition","season","match_id", "team", "one_ahead", "two_ahead", "threeplus_ahead", "min_ahead")

colnames(lei_min_a) <- c("competition","season","match_id", "team", "one_ahead", "two_ahead", "threeplus_ahead", "min_ahead")

lei_min_all <- rbind(lei_min_h, lei_min_a)

#join with mins
lei_all <- left_join(lei_xg_all, select(lei_min_all,c(3:8)), by = c("match_id", "team"))

#replace NA values in xGA ahead
lei_all_fix <- lei_all %>% 
  mutate(percentage_diff_xGA_ahead = case_when(is.nan(percentage_diff_xGA_ahead) ~ 0,
                                               TRUE ~ percentage_diff_xGA_ahead)) %>% 
  filter(team == "Leicester")
