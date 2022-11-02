#Updated Part 1 Plots and Tables -------
#Winning margins team/season ------
team_gd <- season_clean %>% select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>% 
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% 
  distinct() %>% 
  mutate(GD = h_goals - a_goals,
         team = ifelse(GD > 0, h_team, ifelse(GD < 0,a_team,"draw")),
         `Win by 2` = ifelse(abs(GD) == 2, 1, 0),
         `Win by 3` = ifelse(abs(GD) == 3, 1, 0),
         `Win by 4` = ifelse(abs(GD) == 4, 1, 0),
         `Win by 5+` = ifelse(abs(GD) >= 5, 1, 0)) %>% 
  filter(GD != 0) %>% 
  group_by(team, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  arrange(desc(`Win by 5+`), desc(`Win by 4`),desc(`Win by 3`),desc(`Win by 2`)) %>% 
  select(team, season, `Win by 2`, `Win by 3`, `Win by 4`, `Win by 5+`) %>% 
  rename(Team = team,
         Season = season) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol,
                font_size = 16) %>% 
  column_spec(column = 1:6, background = bgcol, color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

team_gd

#xG ahead tables ----
pma_by_team_season %>% ungroup() %>% 
  # select(2,3,10,4,5,6,9) %>% 
  mutate(weighted_xG_ahead = round(weighted_xG_ahead,2),
         total_xG_ahead = round(total_xG_ahead,2)) %>% 
  arrange(desc(total_xG_ahead)) %>% 
  rename(Team = team,
         Season = season,
         Weighted_xG_Ahead = weighted_xG_ahead,
         `Total xG_Ahead` = total_xG_ahead,
         `League Position` = Pos,
         `% diff` = percent_diff_for) %>% 
  select(2,3,4,5) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "left",
                html_font = plotfont,
                stripe_color = bgcol,
                font_size = 16) %>% 
  column_spec(column = 1:3, background = bgcol, color = textcol) %>% 
  column_spec(column = 4, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Export table with xG 3+ ----
rlnt_xg_all %>% 
  group_by(competition, season, team, Pos) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>% 
  mutate(total_xG_ahead_1 = round(total_xG_ahead_1,2),
         total_xG_ahead_2 = round(total_xG_ahead_2,2),
         total_xG_ahead_N = round(total_xG_ahead_N,2),
         total_xG_ahead = total_xG_ahead_1 + total_xG_ahead_2 + total_xG_ahead_N) %>% 
  rename(Team = team,
         Season = season,
         `Total xG Ahead by 3+` = total_xG_ahead_N,
         League_Position = Pos) %>% 
  arrange(desc(`Total xG Ahead by 3+`)) %>% 
  ungroup() %>% 
  select(-c(match_id)) %>%
  select(3,2,4,11) %>%
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol,
                font_size = 16) %>% 
  column_spec(column = 1:3, background = bgcol, color = textcol) %>% 
  column_spec(column = 4, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Percentage difference - weighted and actual xG ahead ----
pma_by_team_season %>% ungroup() %>% 
  mutate(weighted_xG_ahead = round(weighted_xG_ahead,2),
         total_xG_ahead = round(total_xG_ahead,2)) %>% 
  rename(Team = team,
         Season = season,
         `Weighted xG Ahead` = weighted_xG_ahead,
         `Total xG Ahead` = total_xG_ahead,
         `League Position` = Pos,
         `% diff` = percent_diff_for) %>% 
  arrange(desc(`% diff`)) %>% 
  ungroup() %>% 
  select(2:4,`Total xG Ahead`,`Weighted xG Ahead`,`% diff`) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol,
                font_size = 16) %>% 
  column_spec(column = 1:5, background = bgcol, color = textcol) %>% 
  column_spec(column = 6, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Total minutes ahead & min_ahead_per_game -----
pma_by_team_season %>% 
  ungroup() %>% 
  arrange(desc(min_ahead_per_game)) %>% 
  rename(`Total Time Ahead (mins)` = min_ahead,
         `Time Ahead per Game (mins)` = min_ahead_per_game,
         Team = team,
         Season = season,
         `League Position` = Pos) %>% 
  #select 12 for minutes ahead - 22 for min ahead per game
  select(2,3,4,12) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol,
                font_size = 16) %>% 
  column_spec(column = 1:3, background = bgcol, color = textcol) %>% 
  column_spec(column = 4, background = "#eba6a2", color = textcol) %>%
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Weighted xGpMA ----
pma_by_team_season %>% 
  ungroup() %>% 
  arrange(desc(norm_xGpMA_wt)) %>% 
  mutate(weighted_xG_ahead = round(weighted_xG_ahead,2)) %>% 
  rename(Team = team,
         Season = season,
         `League Position` = Pos,
         `Weighted xG Ahead` = weighted_xG_ahead,
         `Total Time Ahead (mins)` = min_ahead,
         `Time Ahead per Game (mins)` = min_ahead_per_game,
         `% diff` = percent_diff_for,
         `Weighted xGpMA` = norm_xGpMA_wt) %>% 
  select(-competition) %>% 
  # filter(`League Position` %in% c(1:4)) %>% 
  head(15) %>% 
  select(1:3,`Weighted xG Ahead`,`Time Ahead per Game (mins)`,`Weighted xGpMA`)%>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:5, background = bgcol, color = textcol) %>% 
  column_spec(column = 6, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Percentage difference
pma_by_team_season %>% 
  ungroup() %>% 
  arrange(desc(norm_percent_diff_for)) %>% 
  mutate(weighted_xG_ahead = round(weighted_xG_ahead,2)) %>% 
  rename(Team = team,
         Season = season,
         `League Position` = Pos,
         `Weighted xG Ahead` = weighted_xG_ahead,
         `Total Time Ahead (mins)` = min_ahead,
         `Time Ahead per Game (mins)` = min_ahead_per_game,
         `% diff` = norm_percent_diff_for,
         `Weighted xGpMA` = norm_xGpMA_wt,
         `Actual xGpMA` = norm_xGpMA) %>% 
  select(-competition) %>% 
  filter(`League Position` %in% c(1:4)) %>% 
  head(10) %>% 
  select(1:3,`Actual xGpMA`, `Weighted xGpMA`,`% diff`)%>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:5, background = bgcol, color = textcol) %>% 
  column_spec(column = 6, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)

#Percentage difference (Over 5 seasons)
pma_by_team_season %>% 
  group_by(team) %>% 
  summarise(`Avg xGpMA` = mean(norm_xGpMA),
            `Avg Weighted xGpMA` = mean(norm_xGpMA_wt),
            `Time Ahead per Game (mins)` = mean(min_ahead_per_game)) %>% 
  mutate(`% diff` = 100 * signif((`Avg Weighted xGpMA` - `Avg xGpMA`)/`Avg xGpMA`,3)) %>% 
  # filter(`Time Ahead per Game (mins)` >= 30) %>% 
  rename(Team = team) %>% 
  arrange(desc(`% diff`)) %>% 
  head(10) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:4, background = bgcol, color = textcol) %>% 
  column_spec(column = 5, background = "#eba6a2", color = textcol) %>% 
  row_spec(row = 0, background = textcol, color = bgcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = bgcol, italic = T, bold = T)


#PLOTS ----
#xG percentage breakdown - bar plots ----
p2_rlnt_bar <- xga_wt_rlnt_xg_all %>% 
  group_by(team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>%  
  select(1, 4:7) %>% arrange(desc(total_xG_ahead)) %>% 
  filter(total_xG > median(total_xG)) %>% 
  mutate(ahead_percentage = total_xG_ahead * 100/total_xG,
         level_percentage = total_xG_level * 100/total_xG,
         behind_percentage = total_xG_behind * 100/total_xG) %>% 
  select(1,6:8) %>% 
  arrange(desc(ahead_percentage)) %>% 
  pivot_longer(-team, names_to = "State", values_to = "xG_percentage") %>%
  mutate(State = factor(State, levels = rev(c("ahead_percentage", "level_percentage", "behind_percentage"))))

p2_bar_team_order <- xga_wt_rlnt_xg_all %>% 
  group_by(team) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>%  
  select(1, 4:7) %>% arrange(desc(total_xG_ahead)) %>% 
  filter(total_xG > median(total_xG)) %>% 
  mutate(ahead_percentage = total_xG_ahead * 100/total_xG,
         level_percentage = total_xG_level * 100/total_xG,
         behind_percentage = total_xG_behind * 100/total_xG) %>% 
  select(1,6:8) %>% 
  arrange(desc(ahead_percentage), desc(level_percentage)) %>% pull(team)

p2_rlnt_bar_plot <- p2_rlnt_bar %>% 
  head(n = 60) %>% 
  ggplot(aes(x = team , y = xG_percentage, fill = State)) +
  geom_bar(colour =  bgcol, stat = "identity") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("#D03028","#3561B0", "#3CA23A"), labels = c("Behind", "Level", "Ahead")) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(team_order[1:20])) +
  coord_flip() +
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = "Roboto Slab Light", size = 20),
        plot.title.position = "plot",
        legend.position = "top",
        legend.justification = 'left',
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 18),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        plot.margin = margin(20,20,20,10),
        axis.text.y = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0),
        axis.text.x = element_text(family = plotfont, colour = textcol, size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = textcol),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 1)) + 
  labs(title = "Relentlessness",
       subtitle = "Percentage xG per game state in Europe's top 5 leagues, 2017/18 - 2021/22",
       caption = "Data courtesy of UnderStat")

p2_rlnt_bar_plot

ggsave(plot = p2_rlnt_bar_plot,
       filename = here::here("outputs","plots","xG breakdown_state_18-22.png"),
       scale = 3, width = 15, height = 9, units = "cm")

#add W22 logo
p2_rlnt_bar_plot_logo <- add_logo(
  plot_path = here("outputs","plots","xG breakdown_state_18-22.png"),
  logo_path = here("resources/WATCH22_CIRCLE.png"),
  logo_position = "top right",
  logo_scale = 19)

p2_rlnt_bar_plot_logo

magick::image_write(
  image = p2_rlnt_bar_plot_logo, 
  path = here("outputs","plots","xG_breakdown_state_logo_18-22.png"))


#Weighted xG ahead vs Minutes ahead per game - top 4 -----
tot_xG_vs_mins_top4_normalised <- pma_by_team_season %>% 
  filter(Pos %in% c(1:4)) %>%
  ungroup() %>% 
  ggplot(aes(x = min_ahead_per_game, y = weighted_xG_ahead, fill = competition, colour = competition)) + 
  geom_smooth(aes(x = min_ahead_per_game, y = weighted_xG_ahead, group = 1),
              colour = textcol,
              se = T,
              method = "loess", 
              formula = y ~ x,
              show.legend = F,
              alpha = 0.25) + 
  geom_label(label = stringr::str_wrap("Teams with the highest weighted xGpMA in Europe's top 4 leagues",50),
             x = 31, y = 70,
             colour = textcol,
             fill = "#a2fc9f",
             family = plotfont,
             hjust = 0,
             alpha = 0.75,
             size = 12 / .pt,
             label.r = unit(0.5,"mm"),
             label.size = NA) +
  geom_point(shape = 21, aes(size = factor(Pos))) +
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) +
  scale_size_manual(values = c(7,6,5,4), guide = "none") +
  scale_x_continuous(limits = c(25,55),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(18,90),expand = c(0,0))+
  geom_mark_hull(data = pma_by_team_season %>% filter(Pos %in% c(1:4)) %>% 
                   arrange(desc(norm_xGpMA_wt)) %>% head(10),
                 aes(group = 1),
                 colour = "#3CA23A",
                 fill = "#a2fc9f",
                 alpha = 0.25,
                 expand = unit(2.5, "mm"), 
                 show.legend = FALSE) + 
  geom_label_repel(data = pma_by_team_season %>% filter(Pos %in% c(1:4)) %>% 
                     arrange(desc(norm_xGpMA_wt)) %>% head(10),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 20,
                   family = plotfont,
                   show.legend = F)+
  geom_mark_hull(data = champions_xGpMA_wt %>% 
                   arrange(norm_xGpMA_wt) %>% head(5),
                 aes(group = 1),
                 colour = "#D03028",
                 fill = "#fa9d98",
                 alpha = 0.25,
                 expand = unit(2.5, "mm"),
                 con.cap = unit(0.5, "mm"),
                 con.colour = "#D03028",
                 con.type = "elbow",
                 description = paste("Least", dQuote("relentless"), "league champions in terms of weighted xGpMA"),
                 label.width = unit(75, 'mm'),
                 label.buffer = unit(-.75, "mm"),
                 label.family = plotfont,
                 label.fontsize = 12,
                 label.colour = textcol,
                 label.fill = "#fa9d98",
                 show.legend = FALSE) +
  geom_label_repel(data = champions_xGpMA_wt %>% 
                     arrange(norm_xGpMA_wt) %>% head(5),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 10,
                   family = plotfont,
                   show.legend = F)+
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
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0)) + 
  labs(title = "Relentlessness",
       subtitle = paste(stringr::str_wrap("Weighted Expected Goals (xG) per Minutes Spent in the Lead (or Minutes Ahead)
                       Top 4 Teams in Europe's Top 5 Leagues, 2017/18 - 2021/22",80), collapse="\n"),
       caption = "1. Data courtesy of UnderStat\n2. Weighted xG Ahead = (xG accumulated while leading) x (goal diff)/10 \n3. Larger points indicate higher league positions",
       x = "Average Minutes Ahead per Game",
       y = "Weighted xG Ahead")

tot_xG_vs_mins_top4_normalised

ggsave(plot = tot_xG_vs_mins_top4_normalised,
       filename = here::here("outputs","plots","tot_xG_vs_mins_top4_normalised_18-22.png"),
       scale = 3, width = 15, height = 9, units = "cm")

#add W22 logo
tot_xG_vs_mins_top4_normalised_logo_new <- add_logo(
  plot_path = here("outputs","plots","tot_xG_vs_mins_top4_normalised_18-22.png"),
  logo_path = here("resources/WATCH22_CIRCLE.png"),
  logo_position = "top right",
  logo_scale = 19)

tot_xG_vs_mins_top4_normalised_logo_new

magick::image_write(
  image = tot_xG_vs_mins_top4_normalised_logo_new, 
  path = here("outputs","plots","tot_xG_vs_mins_top4_normalised_logo_18-22.png"))

#percentage difference plot - top 4 ----
percdiff_top4_norm <- pma_by_team_season %>% 
  filter(Pos %in% c(1:4)) %>%
  ungroup() %>% 
  ggplot(aes(x = norm_xGpMA, y = norm_xGpMA_wt - norm_xGpMA, fill = competition, colour = competition)) + 
  geom_smooth(aes(x = norm_xGpMA, y = norm_xGpMA_wt - norm_xGpMA, group = 1),
              colour = textcol,
              se = T,
              method = "loess", 
              formula = y ~ x,
              show.legend = F,
              alpha = 0.25) + 
  geom_jitter(shape = 21, aes(size = factor(Pos))) + 
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) +
  scale_size_manual(values = c(7,6,5,4), guide = "none") +
  geom_mark_hull(data = pma_by_team_season %>% filter(Pos %in% c(1:4)) %>% 
                   arrange(desc(percent_diff_for)) %>% head(10),
                 aes(group = 1),
                 colour = "#3CA23A",
                 fill = "#a2fc9f",
                 alpha = 0.25,
                 con.cap = unit(0.5, "mm"),
                 con.colour = "#3CA23A",
                 con.type = "elbow",
                 description = paste("Europe's most relentless teams"),
                 label.width = unit(50, 'mm'),
                 label.buffer = unit(20, "mm"),
                 label.family = plotfont,
                 label.fontsize = 12,
                 label.colour = textcol,
                 label.fill = "#a2fc9f",
                 expand = unit(2.5, "mm"),
                 show.legend = FALSE) +
  geom_label_repel(data = pma_by_team_season %>% filter(Pos %in% c(1:4)) %>% 
                     arrange(desc(percent_diff_for)) %>% head(10),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 20,
                   family = plotfont,
                   show.legend = F)+
  geom_label_repel(data = pma_by_team_season %>% filter(Pos %in% c(1:4)) %>% 
                     arrange(desc(percent_diff_for)) %>% tail(10),
                   aes(label = paste(team,"-",season)),
                   size = 4,
                   seed = 15,
                   label.padding = 0.5,
                   force = 10,
                   family = plotfont,
                   show.legend = F) +
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
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0)) + 
  labs(title = "Relentlessness",
       subtitle = paste(stringr::str_wrap("% Difference between Weighted and Actual xG per Minute Ahead
                       Top 4 Teams in Europe's Top 5 Leagues, 2017/18 - 2021/22",80), collapse="\n"),
       caption = "1. Data courtesy of UnderStat\n2.Relentlessness = (Weighted xGpMA - xGpMA)/xGpMA\n3. Larger points indicate higher league positions",
       x = "xGpMA",
       y = "Weighted xGpMA - xGpMA")


percdiff_top4_norm

ggsave(plot = percdiff_top4_norm,
       filename = here::here("outputs","plots","percdiff_top4_normalised_18-22.png"),
       scale = 3, width = 15, height = 9, units = "cm")

#add W22 logo
percdiff_top4_normalised_logo_new <- add_logo(
  plot_path = here("outputs","plots","percdiff_top4_normalised_18-22.png"),
  logo_path = here("resources/WATCH22_CIRCLE.png"),
  logo_position = "top right",
  logo_scale = 19)

percdiff_top4_normalised_logo

magick::image_write(
  image = percdiff_top4_normalised_logo_new, 
  path = here("outputs","plots","percdiff_top4_normalised_logo_18-22.png"))
