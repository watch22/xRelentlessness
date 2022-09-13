pacman::p_load(tidyverse, zoo, here, extrafont)

#Import Understat shot data for top 5 leagues between 2016/2017 & 2020/2021
match_df <- list.files(path = here::here("data"), full.names = T, recursive = T)
season_df <- tibble(File = match_df) %>%
  extract(File, "Site", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data)

#clean up data ----
season_clean <- season_df %>% 
  rename(competition = File) %>% 
  mutate(competition = str_split(basename(competition),"_")) %>% 
  rowwise() %>% 
  mutate(competition = competition[[1]][1]) %>% 
  arrange(date, match_id, minute) %>% 
  select(competition, season, date, match_id, minute, h_team, a_team, h_a, everything()) %>% 
  select(-c(X, Y, id, player_id))


#split xG by state of game (drawing, losing, ahead by 1,2, or more goals) ----
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

#split home and away ----
rlnt_xg_h <- rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:4,6,8:13)

rlnt_xg_a <- rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:3,5,7,14:19)

colnames(rlnt_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")

colnames(rlnt_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG","total_xG_behind", "total_xG_level", "total_xG_ahead_1", "total_xG_ahead_2", "total_xG_ahead_N")
  
rlnt_xg_all <- rbind(rlnt_xg_h, rlnt_xg_a)

rlnt_xg_all %>% 
  group_by(competition, season, team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(total_xG_ahead = total_xG_ahead_1 + total_xG_ahead_2 + total_xG_ahead_N) %>% 
  select(-c(match_id)) %>% 
  arrange(desc(total_xG_ahead))

#get minutes per game state ----
rlnt_min <- rlnt_xg %>%
  select(1:5,8,10:16) %>% 
  # FIX DUPLICATE MINUTES
  mutate(minute = c(minute[-n()], max(minute) + 1))

rlnt_min <- rlnt_min %>% 
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

#get totals
rlnt_min_totals <- rlnt_min %>% 
  group_by(competition, season, match_id, h_team, a_team) %>% 
  summarise(tot_min_ahead_h = sum(lead_time_h),
            tot_min_ahead_a = sum(lead_time_a))

rlnt_min_totals %>% filter(tot_min_ahead_h > 100 |
                           tot_min_ahead_a > 100 |
                             tot_min_ahead_h < 0 |
                             tot_min_ahead_a < 0 |
                             is.na(tot_min_ahead_h) |
                             is.na(tot_min_ahead_a))

#split home and away ----
rlnt_min_h <- rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:4,6)

rlnt_min_a <- rlnt_min_totals %>% 
  ungroup() %>% 
  select(1:3, 5 ,7)

colnames(rlnt_min_h) <- c("competition","season","match_id", "team", "min_ahead")

colnames(rlnt_min_a) <- c("competition","season","match_id", "team", "min_ahead")

rlnt_min_all <- rbind(rlnt_min_h, rlnt_min_a)

#2. get xG per minute ahead ----
rlnt_all <- left_join(rlnt_xg_all, select(rlnt_min_all, c(3:5)), by = c("match_id", "team")) %>% 
  ungroup() %>% 
  mutate(competition = case_when(competition == "bundesliga" ~ "Bundesliga",
                                 competition == "epl" ~ "Premier League",
                                 competition == "seriea" ~ "Serie A",
                                 competition == "laliga" ~ "La Liga",
                                 competition == "ligue1" ~ "Ligue 1"),
         season = case_when(season == 2016 ~ "2016/17",
                            season == 2017 ~ "2017/18",
                            season == 2018 ~ "2018/19",
                            season == 2019 ~ "2019/20",
                            season == 2020 ~ "2020/21",
                            season == 2021 ~ "2021/22")) %>% 
  filter(season != "2021/2022")


#Plot 1: 
rlnt_all_by_team_season <- rlnt_all %>% 
  group_by(competition, team, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(total_xG_ahead = (total_xG_ahead_1 + total_xG_ahead_2 + total_xG_ahead_N),
          xGpMA = total_xG_ahead/min_ahead) %>% 
  arrange(desc(xGpMA)) %>% 
  select(-c(match_id)) %>% 
  filter(min_ahead > 180)

rlnt_all_by_team <- rlnt_all %>% 
  group_by(team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(total_xG_ahead = (total_xG_ahead_1 + total_xG_ahead_2 + total_xG_ahead_N),
         xGpMA = total_xG_ahead/min_ahead) %>% 
  arrange(desc(xGpMA)) %>% 
  select(-c(match_id))

rlnt_all_by_competition <-  rlnt_all %>% 
  group_by(competition, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(total_xG_ahead = (total_xG_ahead_1 + total_xG_ahead_2 + total_xG_ahead_N),
         xGpMA = total_xG_ahead/min_ahead) %>% 
  arrange(desc(xGpMA)) %>% 
  select(-c(match_id))
  

#3. Plot -----
#set colours & aesthetics----
season_shapes <- c("2016/17"= "triangle filled",
                   "2017/18" = "triangle down filled",
                   "2018/19" = "circle filled",
                   "2019/20" = "square filled",
                   "2020/21" = "diamond filled")
competition_colours<- c("Bundesliga" = "#D03028",
                         "Premier League"= "#3561B0",
                         "Serie A" = "#F9AD41",
                         "Ligue 1" = "#1C0368",
                         "La Liga" = "#3CA23A")
season_colours <- c("2016/17"= "#3CA23A",
                    "2017/18" = "#D03028",
                    "2018/19" = "#3561B0",
                    "2019/20" = "#F9AD41",
                    "2020/21" = "#1C0368")
competition_shapes <- c("Bundesliga" = "triangle down filled",
                        "Premier League"= "circle filled",
                        "Serie A" = "square filled",
                        "Ligue 1" = "diamond filled",
                        "La Liga" = "triangle filled")
plotfont <- "Roboto Condensed"
titlefont <- "Roboto Slab"
bgcol <-  "#171516"
panelcol <- "#f7f7ed"#F1F1EF
textcol <- "#ECECEC"


#season by season
rlnt_all_by_team_season %>% 
  filter(min_ahead > 1500 & total_xG_ahead > 40) %>% 
  ggplot(aes(x = min_ahead, y = total_xG_ahead, label = paste(team,"-",season), fill = competition, colour = competition, label = team)) + 
  geom_point(size = 4) + 
  scale_fill_manual(values = competition_colours) +
  scale_colour_manual(values = competition_colours) +
  #scale_shape_manual(values = competition_shapes) + 
  geom_text_repel(colour = bgcol,
                  force = 10,
                  family = plotfont)+
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = titlefont, size = 25, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = panelcol, colour = bgcol),
        legend.key = element_rect(fill = panelcol, colour = panelcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = bgcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 15, hjust = 0)) + 
  labs(title = "Relentlessness",
       subtitle = "Expected Goals (xG) per Minutes Spent in the Lead (or Minutes Ahead)",
       caption = "1. All data from UnderStat \n2. Only considered teams that have spent at least 180 minutes in the lead \nover the course of a season",
       x = "Minutes Ahead" ,
       y = "xG Ahead")

#xG vs xGpMA
rlnt_all_by_team_season %>% 
  filter(min_ahead > 1500 & total_xG_ahead > 40) %>% 
  ggplot(aes(x = total_xG, y = xGpMA, label = paste(team,"-",season), fill = competition, colour = competition, label = team)) + 
  geom_point(size = 4) + 
  scale_fill_manual(values = competition_colours) +
  scale_colour_manual(values = competition_colours) +
  #scale_shape_manual(values = competition_shapes) + 
  geom_text_repel(colour = bgcol,
                  force = 10,
                  family = plotfont)+
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = titlefont, size = 25, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = panelcol, colour = bgcol),
        legend.key = element_rect(fill = panelcol, colour = panelcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = bgcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 15, hjust = 0)) + 
  labs(title = "Relentlessness",
       subtitle = "Expected Goals (xG) per Minutes Spent in the Lead (or Minutes Ahead)",
       caption = "1. All data from UnderStat",
       x = "xG Ahead" ,
       y = "xGpMA")


#xG Ahead Breakdown ----
rlnt_bar <- rlnt_all_by_team %>% select(1, 3, 4, 5, 10) %>% arrange(desc(total_xG_ahead)) %>% 
  filter(total_xG > median(total_xG)) %>% 
  mutate(ahead_percentage = total_xG_ahead * 100/total_xG,
         level_percentage = total_xG_level * 100/total_xG,
         behind_percentage = total_xG_behind * 100/total_xG) %>% 
  select(1,6:8) %>% 
  arrange(desc(ahead_percentage)) %>% 
  pivot_longer(-team, names_to = "State", values_to = "xG_percentage") %>%
  mutate(State = factor(State, levels = c("ahead_percentage", "level_percentage", "behind_percentage")))

team_order <- rlnt_all_by_team %>% select(1, 3, 4, 5, 10) %>% arrange(desc(total_xG_ahead)) %>% 
  filter(total_xG > median(total_xG)) %>% 
  mutate(ahead_percentage = total_xG_ahead * 100/total_xG,
         level_percentage = total_xG_level * 100/total_xG,
         behind_percentage = total_xG_behind * 100/total_xG) %>% 
  select(1,6:8) %>% 
  arrange(desc(ahead_percentage), desc(level_percentage)) %>% pull(team)

rlnt_bar_plot <- rlnt_bar %>% 
  head(n = 60) %>% 
  ggplot(aes(x = team , y = xG_percentage, fill = State)) +
  geom_bar(colour =  bgcol, stat = "identity") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("#D03028","#3561B0", "#3CA23A"),labels = c("Ahead", "Level", "Behind")) + 
  scale_x_discrete(limits = rev(team_order[1:20])) +
  coord_flip() +
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = titlefont, size = 25, face = "italic"),
        legend.position = "right",
        legend.background = element_rect(fill = panelcol, colour = bgcol),
        legend.key = element_rect(fill = panelcol, colour = panelcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = bgcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = textcol),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 15, hjust = 0)) + 
  labs(title = "Relentlessness",
       subtitle = "Percentage xG per state in Europe's top 5 leagues (2017 - 2022)",
       caption = "1. All data from UnderStat \n2. Only considered the first half of the 2021/2022 season",
       y = "Percentage xG per State")

rlnt_bar_plot

