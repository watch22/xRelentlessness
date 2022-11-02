#load packages ----
pacman::p_load(tidyverse, StatsBombR, extrafont, ggupset, tibbletime, ggtext, ggrepel, glue, patchwork, cowplot, gtable, grid, magick, here, ggsoccer, janitor, rvest, ggimage, zoo, ggforce)

#import data ----
league_tables <- read_csv(file = here::here("data", "Europe Top 5 - League Tables 2015 - 2022.csv")) %>% 
  rename(competition = Competition,
         season = Season,
         team = Team) %>% 
  mutate(teamseason = paste0(team,season))

teamsout <- filter(league_tables,season == "2015/16" & team != "Leicester") %>% pull(teamseason)

league_tables %>% filter(teamseason %notin% teamsout)

league_tables_summary <- league_tables %>% 
  group_by(team) %>% 
  summarise(M = sum(M))

match_df <- list.files(path = here::here("data","understat_shot_xg"), full.names = T, recursive = T)
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
  select(-c(X, Y, id, player_id, Site))

season_clean <- season_clean %>% filter(!is.na(season))

#Divide xG by teams and assign based on game state -----
xga_wt_rlnt_xg <- season_clean %>% 
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


#group games and get totals ----
xga_wt_rlnt_xg_totals <- xga_wt_rlnt_xg %>% 
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

xga_wt_rlnt_xg_totals

xga_wt_rlnt_xg_h <- xga_wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:4,6,8:19)

xga_wt_rlnt_xg_a <- xga_wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:3,5,7,20:31)

colnames(xga_wt_rlnt_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind","total_xG_level", "total_xG_ahead","total_xG_one_ahead", "weighted_xG_ahead","percentage_diff_ahead", "total_xGA_behind", "total_xGA_ahead", "weighted_xGA_ahead", "total_xGA_one_ahead", "percentage_diff_xGA_ahead")

colnames(xga_wt_rlnt_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind","total_xG_level", "total_xG_ahead","total_xG_one_ahead", "weighted_xG_ahead","percentage_diff_ahead", "total_xGA_behind", "total_xGA_ahead", "weighted_xGA_ahead", "total_xGA_one_ahead", "percentage_diff_xGA_ahead")

#add opponents
xga_wt_rlnt_xg_h <- left_join(xga_wt_rlnt_xg_h,select(xga_wt_rlnt_xg_a,c(match_id,team,goals)),by = "match_id") %>% 
  rename(team = team.x,
         goals_for = goals.x,
         opponent = team.y,
         goals_against = goals.y) 

xga_wt_rlnt_xg_a <- left_join(xga_wt_rlnt_xg_a,select(xga_wt_rlnt_xg_h,c(match_id,team,goals_for)),by = "match_id") %>% 
  rename(team = team.x,
         goals_against = goals_for,
         goals_for = goals,
         opponent = team.y
         ) 

xga_wt_rlnt_xg_all <- rbind(xga_wt_rlnt_xg_h, xga_wt_rlnt_xg_a)%>% 
  left_join(league_tables %>% select(2:4), by = c("team", "season"))

#Get Time/State ----
xga_rlnt_min <- xga_wt_rlnt_xg %>%
  select(1:5,8,10:16) %>% 
  # FIX DUPLICATE MINUTES
  mutate(minute = c(minute[-n()], max(minute) + 1))


xga_rlnt_min <- xga_rlnt_min %>% 
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

xga_rlnt_min

#Group and get totals ----
xga_rlnt_min_totals <- xga_rlnt_min %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(tot_min_ahead1_h = sum(lead_time1_h),
            tot_min_ahead2_h = sum(lead_time2_h),
            tot_min_aheadn_h = sum(lead_timen_h),
            total_min_ahead_h = sum(tot_lead_time_h),
            tot_min_ahead1_a = sum(lead_time1_a),
            tot_min_ahead2_a = sum(lead_time2_a),
            tot_min_aheadn_a = sum(lead_timen_a),
            total_min_ahead_a = sum(tot_lead_time_a))

#CHECK VALUES -----
xga_rlnt_min_totals %>% filter(total_min_ahead_h > 100 |
                             total_min_ahead_a > 100 |
                             total_min_ahead_h < 0 |
                             total_min_ahead_a < 0 |
                             is.na(total_min_ahead_h) |
                             is.na(total_min_ahead_a))

#split home and away ----
xga_rlnt_min_h <- xga_rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:4,6:9)

xga_rlnt_min_a <- xga_rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:3, 5 ,10:13)

colnames(xga_rlnt_min_h) <- c("competition","season","match_id", "team", "one_ahead", "two_ahead", "threeplus_ahead", "min_ahead")
colnames(xga_rlnt_min_a) <- c("competition","season","match_id", "team", "one_ahead", "two_ahead", "threeplus_ahead", "min_ahead")

#add opponents
xga_rlnt_min_h <- left_join(xga_rlnt_min_h,select(xga_rlnt_min_a,c(match_id,team)),by = "match_id") %>% 
  rename(team = team.x,
         opponent = team.y)
xga_rlnt_min_a <- left_join(xga_rlnt_min_a,select(xga_rlnt_min_h,c(match_id,team)),by = "match_id") %>% 
  rename(team = team.x,
         opponent = team.y)

#combine
xga_rlnt_min_all <- rbind(xga_rlnt_min_h, xga_rlnt_min_a)

#join with mins
xga_wt_rlnt_all <- left_join(xga_wt_rlnt_xg_all, select(xga_rlnt_min_all,c(3:8)), by = c("match_id", "team"))

#replace NA values in xGA ahead
xga_wt_rlnt_all_fix <- xga_wt_rlnt_all %>% 
  mutate(percentage_diff_xGA_ahead = case_when(is.nan(percentage_diff_xGA_ahead) ~ 0,
                                               TRUE ~ percentage_diff_xGA_ahead))

#add Leicester City Data
xga_wt_rlnt_all_fix <- rbind(xga_wt_rlnt_all_fix, lei_all_fix)

#get xGpMA - xGApMA -----
pma_by_team_season <- xga_wt_rlnt_all_fix %>% 
  filter(season != "2016/17") %>% 
  group_by(competition, team, season, Pos) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(xGpMA = round(total_xG_ahead/min_ahead,4),
         xGpMA_wt = round(weighted_xG_ahead/min_ahead,4),
         percent_diff_for = (signif((xGpMA_wt-xGpMA)/xGpMA,3)*100),
         xGApMA = round(total_xGA_ahead/min_ahead,4),
         xGApMA_wt = round(weighted_xGA_ahead/min_ahead,4),
         percent_diff_against = (signif((xGApMA_wt-xGApMA)/xGApMA,3)*100),
         xG_per_one_ahead = round(total_xG_one_ahead/one_ahead,4),
         xGA_per_one_ahead = round(total_xGA_one_ahead/one_ahead,4)) %>% 
  select(1:4,10:12,15:17,20,23:31)%>% 
  left_join(league_tables %>% select(competition, season, team, M, W), by = c("competition", "season", "team")) %>% 
  distinct() %>% 
  #normalise values per average minutes ahead per match
  mutate(min_ahead_per_game = round(min_ahead/M,1),
         norm_xGpMA = round(total_xG_ahead/min_ahead_per_game,2),
         norm_xGpMA_wt = round(weighted_xG_ahead/min_ahead_per_game,2),
         norm_percent_diff_for = (signif((norm_xGpMA_wt-norm_xGpMA)/norm_xGpMA,3)*100),
         norm_xGApMA = round(total_xGA_ahead/min_ahead_per_game,2),
         norm_xGApMA_wt = round(weighted_xGA_ahead/min_ahead_per_game,2),
         norm_percent_diff_against = (signif((norm_xGApMA_wt-norm_xGApMA)/norm_xGApMA,3)*100),
         one_ahead_per_game = round(one_ahead/M,1),
         norm_xG_one_ahead = round(total_xG_one_ahead/one_ahead_per_game,2),
         norm_xGA_one_ahead = round(total_xGA_one_ahead/one_ahead_per_game,2)) %>% 
  ungroup()


champions_xGpMA_wt <- pma_by_team_season %>% filter(Pos == 1)

pma_xG_xGA <- pma_by_team_season %>% ungroup() %>% select(competition, 
                                                         season,
                                                         team,
                                                         M,
                                                         W,
                                                         Pos,
                                                         total_xG_ahead,
                                                         total_xGA_ahead,
                                                         weighted_xG_ahead,
                                                         weighted_xGA_ahead,
                                                         xGpMA,
                                                         xGApMA,
                                                         xGpMA_wt,
                                                         xGApMA_wt,
                                                         total_xG_one_ahead,
                                                         total_xGA_one_ahead,
                                                         xG_per_one_ahead,
                                                         xGA_per_one_ahead,
                                                         norm_xGpMA,
                                                         norm_xGApMA,
                                                         norm_xGpMA_wt,
                                                         norm_xGApMA_wt,
                                                         norm_xG_one_ahead,
                                                         norm_xGA_one_ahead) 

pma_xGD <- pma_xG_xGA %>% 
  mutate(total_xGD_ahead = total_xG_ahead - total_xGA_ahead,
         weighted_xGD_ahead = weighted_xG_ahead - weighted_xGA_ahead,
         xGDpMA = xGpMA - xGApMA,
         wt_xGDpMA = xGpMA_wt - xGApMA_wt,
         total_xGD_one_ahead = total_xG_one_ahead - total_xGA_one_ahead,
         xGD_per_one_ahead = xG_per_one_ahead - xGA_per_one_ahead,
         norm_xGDpMA = norm_xGpMA - norm_xGApMA,
         wt_norm_xGDpMA = norm_xGpMA_wt - norm_xGApMA_wt,
         norm_xGD_one_ahead = norm_xG_one_ahead - norm_xGA_one_ahead) %>% 
  select(1:6,contains('xGD'))
