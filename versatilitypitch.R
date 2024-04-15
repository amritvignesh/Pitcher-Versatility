library(baseballr)
library(dplyr)
library(rvest)
library(mlbplotR)
library(gt)
library(gtExtras)
library(tidyverse)
library(ggtext)

qualified <- fg_pitch_leaders(qual = "y", startseason = "2023", endseason = "2023") %>%
  select(id = xMLBAMID, pitcher = PlayerName, team = team_name)

qualified$team[which(qualified$id == 656756)] <- "TEX"
qualified$team[which(qualified$id == 434378)] <- "HOU"
qualified$team[which(qualified$id == 608337)] <- "CLE"
qualified$team[which(qualified$id == 458681)] <- "LAD"

pitch_type_stats <- data.frame()
for (id in qualified$id) {
  node <- read_html(paste0("https://baseballsavant.mlb.com/player-services/statcast-pitches-breakdown?playerId=", id, "&position=1&hand=&pitchBreakdown=pitches&timeFrame=yearly&season=2023&pitchType=&count=&updatePitches=true")) %>% html_node("#detailedPitches")
  table <- node %>% html_table()
  table <- table %>%
    select(-Year) %>%
    mutate(id = id) %>%
    select(id, everything())
  cat(paste0(id, " done\n"))
  Sys.sleep(3)
  pitch_type_stats <- rbind(pitch_type_stats, table)
}

pitch_type_stats <- left_join(pitch_type_stats, qualified, by = "id") %>% select(id, pitcher, team, everything())

pitch_types <- unique(pitch_type_stats$`Pitch Type`)

nums <- expand.grid(id = unique(pitch_type_stats$id), pitch_type = pitch_types) %>% arrange(id)

pcts <- pitch_type_stats %>% select(id, pitch_type = `Pitch Type`, pct = `%`)

nums <- left_join(nums, pcts, by = c("id", "pitch_type"))

nums$pct[which(is.na(nums$pct))] <- 0

nums <- nums %>%
  group_by(id) %>%
  summarize(stdev = sd(pct)) %>%
  ungroup() %>%
  mutate(versatility = round(100 * min(stdev)/stdev, 1)) %>%
  left_join(qualified, by = "id") %>%
  select(id, pitcher, team, versatility) %>%
  arrange(-versatility)

headshots <- load_headshots() %>% select(id = savant_id, headshot = espn_headshot)

nums <- left_join(nums, headshots, by = "id") %>% select(-id) %>% select(headshot, everything())

mlb_logo <- "https://upload.wikimedia.org/wikipedia/commons/a/a6/Major_League_Baseball_logo.svg"

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>Baseball Savant</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

table <- nums %>% gt() %>%
  gt_img_rows(columns = headshot, height = 50) %>%
  gt_fmt_mlb_logo(columns = "team") %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot, pitcher, team, versatility)
  ) %>%
  gt_hulk_col_numeric(versatility) %>%
  cols_label(
    headshot = "",
    pitcher = md("**Pitcher**"),
    team = md("**Team**"),
    versatility = md("**Versatility**")
  ) %>%
  tab_header(
    title = add_text_img(url = mlb_logo, " Qualified Pitcher Versatility", 30, left = TRUE),
    subtitle = md("*Based on Pitch Arsenal Usage in **2023***")
  ) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(pitcher, versatility)
    )
  ) %>%
  tab_source_note(html(caption))

stuff_stats <- read_csv("stuff.csv")

Encoding(stuff_stats$pitcher)<-'latin1'

nums <- left_join(nums, stuff_stats, by = "pitcher") 

plot <- nums %>%
  ggplot(aes(x = versatility, y = stuff_plus)) +
  geom_hline(yintercept = mean(nums$stuff_plus), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(nums$versatility), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_from_path(aes(x = versatility, y = stuff_plus, path = headshot), width = 0.1, height = 0.1) +
  labs(x = "Versatility",
       y = "Stuff+",
       title = "Comparing Pitch Arsenal Versatility To Pitching Proficiency",
       caption = "Data from **FanGraphs** & **Baseball Savant** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.caption = element_markdown(hjust = 0.5))