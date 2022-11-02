#Prep Leicester_1516 -------
lei1516_gd <- lei1516_df %>% 
  select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>% 
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% distinct(match_id, .keep_all = T) %>% 
  mutate(GD = h_goals - a_goals,team = ifelse(GD > 0, h_team, ifelse(GD < 0,a_team,"draw"))) %>% 
  filter(team != "draw") %>% 
  filter(team == "Leicester")



#Win_by_1 --------
win_by_1 <- season_clean %>% 
  filter(season != "2016/17") %>% 
  select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>%
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% 
  distinct(match_id, .keep_all = T) %>% 
  mutate(GD = h_goals - a_goals,
         team = ifelse(GD > 0, h_team, ifelse(GD < 0, a_team, "draw"))) %>% 
  rbind(lei1516_gd) %>% 
  mutate(by_1 = ifelse(abs(GD) == 1, 1, 0)) %>% 
  filter(GD != 0) %>%
  group_by(team, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>%
  arrange(desc(by_1)) %>% 
  select(team, season, by_1) %>% 
  mutate(joiner = sub(" ","_",paste0(team,season))) %>% 
  ungroup()

win_by_1

#Get teams who have won with the smallest win margins -----
win_margins <- season_clean %>% 
  filter(season != "2016/17") %>% 
  select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>% 
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% 
  distinct(match_id, .keep_all = T) %>% 
  mutate(GD = h_goals - a_goals,
         team = ifelse(GD > 0, h_team, ifelse(GD < 0,a_team,"draw"))) %>% 
  filter(team != "draw") %>% 
  rbind(lei1516_gd) %>%
  mutate(GD = abs(GD)) %>% 
  ungroup() %>% 
  select(competition, match_id,season, team, GD) %>% 
  distinct(match_id, .keep_all = T) %>%
  group_by(team, season) %>% 
  summarise(GD = sum(GD)) %>% 
  arrange(desc(GD)) %>% 
  ungroup() %>% 
  left_join(league_tables,by = c("team", "season")) %>% 
  left_join(win_by_1, by = c("team", "season")) %>% 
  select(competition, team, season, GD, M, W, by_1, Pos) %>% 
  mutate(Avg_Win_Margin = round(GD/W,2),
         by_one_percent = signif(by_1 * 100/ W,3)) %>% 
  arrange(Avg_Win_Margin)

win_margins %>% 
  arrange(desc(by_one_percent))

win_margins %>% 
  mutate(win_percentage = W/M) %>% 
  select(team, season, win_percentage, Avg_Win_Margin, by_one_percent, Pos)

#teams who won their leagues with the highest number of wins by 1 ----
#without Leicester 15/16
win_margins %>% 
  filter(Pos == 1 & season != "2015/16") %>% 
  select(team, season, W, by_1, by_one_percent) %>% 
  arrange(desc(by_one_percent))

#with Leicester 15/16
win_margins %>% 
  filter(Pos == 1) %>% 
  select(team, season, W, by_1, by_one_percent) %>% 
  arrange(desc(by_one_percent))

#teams with highest xGA while ahead ----
#one ahead
pma_by_team_season %>% 
  filter(W/M > 0.5) %>% 
  select(1:4, total_xGA_one_ahead, xGA_per_one_ahead,norm_xGA_one_ahead) %>% 
  arrange(desc(xGA_per_one_ahead))

#ahead any margin
pma_by_team_season %>% 
  filter(W/M > 0.5) %>% 
  select(1:4, total_xGA_ahead, norm_xGApMA, norm_xGApMA_wt, norm_percent_diff_against) %>% 
  arrange(desc(norm_percent_diff_against))

#tightest GD/Win
pma_xGD %>% mutate(xGD_per_win = total_xGD_ahead/W,
                   wt_xGD_per_win = weighted_xGD_ahead/W) %>% 
  select(3,2,4:6,xGD_per_win,wt_xGD_per_win) %>% 
  arrange(wt_xGD_per_win) %>% 
  filter(W/M > 0.5)

#tightest xGDpMA (one ahead)
pma_xGD %>% select(3,2,4:6,xGD_per_one_ahead) %>% 
  filter(W/M > 0.5) %>% 
  arrange(xGD_per_one_ahead)

#Suffering - weighted xGApMA - xGApMA
pma_by_team_season %>% 
  # select(1:4, contains("xGA"), contains("against"), Pos) %>% 
  left_join(select(league_tables,c("W","team","season")), by = c("team","season")) %>% 
  mutate(for_vs_against = norm_xGpMA_wt/norm_xGApMA_wt,
         Win_Percent = W/M) %>% 
  filter(Win_Percent > 0.5) %>% 
  arrange(for_vs_against) %>% 
  View()

#Weighted xGA ahead vs Minutes ahead per game -----
pma_by_team_season %>% 
  left_join(select(league_tables,c("W","team","season")), by = c("team","season")) %>% 
  filter(W >= 10) %>% 
  ggplot(aes(x = min_ahead_per_game, y = weighted_xGA_ahead, fill = competition, colour = competition)) + 
  geom_smooth(aes(x = min_ahead_per_game, y = weighted_xGA_ahead, group = 1),
              colour = textcol,
              se = T,
              method = "loess", 
              formula = y ~ x,
              show.legend = F,
              alpha = 0.25) + 
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) + 
  # scale_size_manual(values = c(7,6,5,4), guide = "none")
  # scale_x_continuous(limits = c(25,55),
  #                    expand = c(0,0))+
  # scale_y_continuous(limits = c(18,90),expand = c(0,0)) +
  geom_label_repel(data = pma_by_team_season %>% 
                     left_join(select(league_tables,c("W","team","season")), by = c("team","season")) %>% 
                     filter(W >= 20) %>% arrange(desc(norm_xGApMA_wt)) %>% head(10),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 20,
                   family = plotfont,
                   show.legend = F)+
  geom_label_repel(data = champions_xGpMA_wt %>% 
                     arrange(norm_xGApMA_wt) %>% head(5),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 10,
                   family = plotfont,
                   show.legend = F)+
  geom_mark_hull(data = champions_xGpMA_wt %>% 
                   arrange(norm_xGApMA_wt) %>% head(5),
                 aes(group = 1),
                 colour = "#D03028",
                 fill = "#fa9d98",
                 alpha = 0.25,
                 expand = unit(2.5, "mm"),
                 con.cap = unit(0.5, "mm"),
                 con.colour = "#D03028",
                 con.type = "elbow",
                 # description = paste("Least", dQuote("relentless"), "league champions in terms of weighted xGpMA"),
                 # label.width = unit(75, 'mm'),
                 # label.buffer = unit(-.75, "mm"),
                 # label.family = plotfont,
                 # label.fontsize = 12,
                 # label.colour = textcol,
                 # label.fill = "#fa9d98",
                 show.legend = FALSE) +
  guides(size = "none", fill = guide_legend(override.aes = list(size = 5))) +
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = "Roboto Slab Light", size = 20),
        legend.position = "top",
        legend.justification = "left",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 20),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 18),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.margin = margin(10,20,10,10),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0))

#Leading by 1: Weighted xGA ahead vs Minutes ahead per game -----
pma_by_team_season %>% 
  left_join(select(league_tables,c("W","team","season")), by = c("team","season")) %>% 
  filter(W >= 10) %>% 
  ggplot(aes(x = one_ahead_per_game, y = total_xG_one_ahead, fill = competition, colour = competition)) + 
  geom_smooth(aes(x = one_ahead_per_game, y = xGA_per_one_ahead, group = 1),
              colour = textcol,
              se = T,
              method = "loess", 
              formula = y ~ x,
              show.legend = F,
              alpha = 0.25) + 
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) + 
  # scale_size_manual(values = c(7,6,5,4), guide = "none")
  # scale_x_continuous(limits = c(25,55),
  #                    expand = c(0,0))+
  # scale_y_continuous(limits = c(18,90),expand = c(0,0)) +
  geom_label_repel(data = pma_by_team_season %>%
                     left_join(select(league_tables,c("W","team","season")), by = c("team","season")) %>%
                     filter(W >= 20) %>% arrange(desc(norm_xGA_one_ahead)) %>% head(10),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 20,
                   family = plotfont,
                   show.legend = F)+
  # geom_label_repel(data = champions_xGpMA_wt %>% 
  #                    arrange(norm_xGApMA_wt) %>% head(5),
  #                  aes(label = paste(team,"-",season)),
  #                  size = 4,
  #                  seed = 15,
  #                  label.padding = 0.5,
  #                  force = 10,
  #                  family = plotfont,
  #                  show.legend = F)+
  # geom_mark_hull(data = champions_xGpMA_wt %>% 
  #                  arrange(norm_xGApMA_wt) %>% head(5),
  #                aes(group = 1),
  #                colour = "#D03028",
  #                fill = "#fa9d98",
  #                alpha = 0.25,
  #                expand = unit(2.5, "mm"),
  #                con.cap = unit(0.5, "mm"),
  #                con.colour = "#D03028",
  #                con.type = "elbow",
                 # description = paste("Least", dQuote("relentless"), "league champions in terms of weighted xGpMA"),
                 # label.width = unit(75, 'mm'),
                 # label.buffer = unit(-.75, "mm"),
                 # label.family = plotfont,
                 # label.fontsize = 12,
                 # label.colour = textcol,
                 # label.fill = "#fa9d98",
                 # show.legend = FALSE) +
  guides(size = "none", fill = guide_legend(override.aes = list(size = 5))) +
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = "Roboto Slab Light", size = 20),
        legend.position = "top",
        legend.justification = "left",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 20),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 18),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.margin = margin(10,20,10,10),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0))  



