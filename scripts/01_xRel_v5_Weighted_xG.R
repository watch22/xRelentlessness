pacman::p_load(tidyverse, zoo, here, extrafont, knitr, kableExtra, ggrepel, gridExtra)

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

#create table with top 4 teams----
top4 <- tibble(competition = c(rep("Bundesliga",20), rep("La Liga",20), rep("Ligue 1",20), rep("Premier League",20),rep("Serie A", 20)), season = rep(sort(rep(names(season_colours),4)),5), team = c("Bayern Munich", "RasenBallsport Leipzig", "Borussia Dortmund","Hoffenheim","Bayern Munich","Schalke 04", "Hoffenheim","Borussia Dortmund","Bayern Munich", "RasenBallsport Leipzig","Borussia Dortmund","Bayer Leverkusen", "Bayern Munich", "RasenBallsport Leipzig","Borussia Dortmund","Borussia M.Gladbach", "Bayern Munich", "RasenBallsport Leipzig", "Borussia Dortmund", "Wolfsburg", "Real Madrid",  "Barcelona", "Atletico Madrid","Sevilla","Real Madrid","Barcelona","Atletico Madrid", "Valencia", "Real Madrid", "Barcelona","Atletico Madrid","Valencia","Real Madrid","Barcelona", "Atletico Madrid","Sevilla",  "Real Madrid","Barcelona","Atletico Madrid", "Sevilla","Monaco", "Paris Saint Germain","Nice", "Lyon","Monaco","Paris Saint Germain","Marseille","Lyon",
                                                                                                                                                                                                      "Paris Saint Germain", "Lyon", "Lille","Saint-Etienne",
                                                                                                                                                                                                      "Paris Saint Germain", "Lille", "Marseille", "Rennes", 
                                                                                                                                                                                                      "Paris Saint Germain", "Lille", "Monaco", "Lyon",
                                                                                                                                                                                                      "Chelsea", "Tottenham", "Manchester City", "Liverpool",
                                                                                                                                                                                                      "Tottenham", "Manchester City", "Liverpool","Manchester United",
                                                                                                                                                                                                      "Chelsea", "Tottenham", "Manchester City", "Liverpool",
                                                                                                                                                                                                      "Chelsea", "Manchester City", "Liverpool","Manchester United",
                                                                                                                                                                                                      "Chelsea", "Manchester City", "Liverpool","Manchester United",
                                                                                                                                                                                                      "Juventus", "Napoli", "Roma", "Atalanta",
                                                                                                                                                                                                      "Juventus", "Napoli", "Roma", "Inter",
                                                                                                                                                                                                      "Juventus", "Napoli", "Atalanta", "Inter",
                                                                                                                                                                                                      "Juventus", "Lazio", "Atalanta", "Inter",
                                                                                                                                                                                                      "Juventus", "AC Milan", "Atalanta", "Inter"), top4 = rep(1,100))


#GET xG PER GAME STATE ----
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

#split home and away ----
wt_rlnt_xg_h <- wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:4,6,8:13)

wt_rlnt_xg_a <- wt_rlnt_xg_totals %>% 
  ungroup() %>% 
  select(1:3,5,7,14:19)

colnames(wt_rlnt_xg_h) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead", "weighted_xG_ahead", "percentage_diff_ahead")

colnames(wt_rlnt_xg_a) <- c("competition", "season", "match_id", "team", "goals", "total_xG", "total_xG_behind", "total_xG_level","total_xG_ahead", "weighted_xG_ahead", "percentage_diff_ahead")
  
wt_rlnt_xg_all <- rbind(wt_rlnt_xg_h, wt_rlnt_xg_a)

wt_rlnt_xg_all %>% 
  group_by(competition, season, team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x)))%>% 
  mutate(percentage_diff_ahead = 100 * (weighted_xG_ahead - total_xG_ahead)/total_xG_ahead) %>% 
  select(-c(match_id)) %>% 
  arrange(desc(percentage_diff_ahead))

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

#3. xG PER GAME STATE (WHILE AHEAD) ------
wt_rlnt_all <- left_join(wt_rlnt_xg_all, select(wt_rlnt_min_all,c(3:5)), by = c("match_id", "team")) %>% 
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


#4. ANALYSIS - BY TEAM/SEASON/COMPETITION ----
wt_rlnt_all_by_team_season <- wt_rlnt_all %>% 
  group_by(competition, team, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(xGpMA = round(total_xG_ahead/min_ahead,4),
         XGpMA_wt = round(weighted_xG_ahead/min_ahead,4),
         percent_diff = (signif((XGpMA_wt-xGpMA)/xGpMA,3)*100)) %>% 
  arrange(desc(percent_diff), desc(XGpMA_wt)) %>% 
  select(1:3,9,10,12:15) %>% 
  left_join(top4, by = c("competition", "team", "season")) %>% distinct()

  wt_rlnt_all_by_team_season %>% 
  ungroup() %>% 
  filter(!is.na(top4)) %>% 
  rename(Team = team,
         Season = season,
         `xGpMA (Weighted)` = XGpMA_wt,
         `% diff` = percent_diff) %>% 
  select(-competition) %>% 
  arrange(`% diff`) %>% 
  head(10) %>% 
  select(1,2,6:8) %>% 
  kable() %>% 
  kable_styling(full_width = T, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:5, background = bgcol, color = textcol) %>% 
  row_spec(row = 0, background = bgcol, color = textcol) %>% 
  row_spec(row = 1, background = "#165d8f", color = textcol, italic = T, bold = T) #8F1D16

wt_rlnt_all_by_team_season %>% 
  ungroup() %>% 
  # filter(competition == "Premier League") %>%
  filter(min_ahead >= quantile(min_ahead, 0.75)) %>% 
  rename(Team = team,
         Season = season,
         `xGpMA (Weighted)` = XGpMA_wt,
         `% diff` = percent_diff) %>% 
  select(-competition) %>% 
  arrange(`% diff`) %>% 
  head(10) %>% 
  select(1,2,6:8) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:5, background = bgcol, color = textcol) %>% 
  row_spec(row = 0, background = "#2B2326", color = textcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = textcol, italic = T, bold = T)


wt_rlnt_all_by_team <- wt_rlnt_all %>% 
  group_by(team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(xGpMA = round(total_xG_ahead/min_ahead,4),
         XGpMA_wt = round(weighted_xG_ahead/min_ahead,4),
         percent_diff = (signif((XGpMA_wt-xGpMA)/xGpMA,3)*100))

wt_rlnt_all_by_team %>% 
  arrange(desc(min_ahead)) %>% 
  filter(min_ahead >= quantile(min_ahead, 0.9)) %>% 
  arrange(desc(percent_diff), desc(XGpMA_wt)) %>% 
  select(1,11:13) %>% 
  rename(Team = team,
         `xGpMA (Weighted)` = XGpMA_wt,
         `% diff` = percent_diff) %>% 
  slice(1:10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:4, background = bgcol, color = textcol) %>% 
  row_spec(row = 0, background = "#2B2326", color = textcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = textcol, italic = T, bold = T)
  

wt_rlnt_all_by_comp_season <-  wt_rlnt_all %>% 
  group_by(competition, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(xGpMA = round(total_xG_ahead/min_ahead,4),
         XGpMA_wt = round(weighted_xG_ahead/min_ahead,4),
         percent_diff = (signif((XGpMA_wt-xGpMA)/xGpMA,3)*100)) %>% 
  arrange(desc(percent_diff), desc(XGpMA_wt)) %>% 
  select(1:2,12:14)


#5. PLOTS -----
#set colours & aesthetics----
season_shapes <- c("2016/17"= "triangle filled",
                   "2017/18" = "triangle down filled",
                   "2018/19" = "circle filled",
                   "2019/20" = "square filled",
                   "2020/21" = "diamond filled")
competition_colours<- c("Bundesliga" = "black",
                         "Premier League"= "white",
                         "Serie A" = "#002d71",
                         "Ligue 1" = "#081C3D",#081C3D#D03028
                         "La Liga" = "#fbec21")
competition_fills <- c("Bundesliga" = "white",
                       "Premier League"= "#37003D", #D03028
                       "Serie A" = "#008fd7",#008fd7 #3561B0
                       "Ligue 1" = "#CEFB0B",#CEFB0B#3CA23A
                       "La Liga" = "#D03028")#fbec21#F9AD41#e00c1a
season_colours <- c("2016/17"= "#3CA23A",
                    "2017/18" = "#D03028",
                    "2018/19" = "#3561B0",
                    "2019/20" = "#F9AD41",
                    "2020/21" = "#1C0368")
# competition_shapes <- c("Bundesliga" = "triangle down filled",
#                         "Premier League"= "circle filled",
#                         "Serie A" = "square filled",
#                         "Ligue 1" = "diamond filled",
#                         "La Liga" = "triangle filled")
plotfont <- "Roboto Condensed"
titlefont <- "Roboto Slab"
textcol <-  "#171516"
panelcol <- "#f7f7ed"#F1F1EF
bgcol <- "#f7f7ed"#ECECEC


#season by season
wt_rlnt_all_by_team_season %>% 
  arrange(desc(min_ahead)) %>% 
  filter(min_ahead >= quantile(min_ahead,0.75)) %>% 
  ggplot(aes(x = min_ahead, y = weighted_xG_ahead, fill = competition, colour = competition)) + 
  geom_point(size = 5, shape = 21) + 
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) +
  #scale_shape_manual(values = competition_shapes) + 
  geom_text_repel(data = wt_rlnt_all_by_team_season %>% 
                    arrange(desc(min_ahead)) %>% 
                    filter(min_ahead >= quantile(min_ahead,0.75)) %>% head(10),
                  aes(label = paste(team,"-",season)),
                  colour = bgcol,
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
       caption = "1. All data from UnderStat \n2. Only considered teams that have spent at least 180 minutes in the lead \nover the course of a season")

#xG vs xGpMA
wt_rlnt_all_by_team_season %>% 
  #filter(min_ahead > 1500 & total_xG_ahead > 40) %>% 
  ggplot(aes(x = total_xG_ahead, y = xGpMA, label = paste(team,"-",season), fill = competition, colour = competition)) + 
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
        legend.justification = "left",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 15),
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






