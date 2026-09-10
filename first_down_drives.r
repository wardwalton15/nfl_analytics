# Run: Rscript first_down_drives.r
# Optional cached nflreadr data: Rscript first_down_drives.r /path/to/pbp.rds
# Field definitions: https://nflreadr.nflverse.com/articles/dictionary_pbp.html
library(nflreadr)
library(dplyr)
library(tidyr)
library(gt)
# PNG export also requires webshot2 and Chrome/Chromium.
# Install once: install.packages(c("nflreadr", "dplyr", "tidyr", "gt", "webshot2"))

args <- commandArgs(trailingOnly = TRUE)
fd_pbp <- if (length(args)) readRDS(args[1]) else load_pbp(2025)
stopifnot(nrow(fd_pbp) > 0)
fd_pbp <- fd_pbp |> filter(season == 2025, season_type == "REG")
stopifnot(n_distinct(fd_pbp$game_id) == 272)

# Use corrected drive IDs. Keep penalty first downs, but remove kickoffs,
# conversion attempts and administrative records from the drive calculation.
# A drive must contain an offensive down (including kneels/spikes), or a
# credited first down. End-of-half and kneel-only possessions are included.
# nflverse first_down includes first downs on touchdown plays; the initial
# first-and-10 at the start of a possession is not itself a first down gained.
fd_drives <- fd_pbp |>
  filter(!is.na(posteam), !is.na(fixed_drive),
         !coalesce(play_deleted == 1, FALSE),
         !coalesce(play_type %in% c("kickoff", "extra_point"), FALSE),
         !coalesce(two_point_attempt == 1, FALSE),
         !coalesce(extra_point_attempt == 1, FALSE)) |>
  group_by(game_id, posteam, fixed_drive) |>
  summarise(
    offensive_drive = any(!is.na(down) | first_down == 1, na.rm = TRUE),
    first_downs = sum(first_down == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(offensive_drive) |>
  mutate(bucket = factor(if_else(first_downs >= 4, "4+", as.character(first_downs)),
                         levels = c("0", "1", "2", "3", "4+")))

fd_team_long <- fd_drives |>
  count(posteam, bucket, .drop = FALSE, name = "drives") |>
  group_by(posteam) |>
  mutate(total_drives = sum(drives), pct = drives / total_drives) |>
  ungroup()
fd_league <- fd_drives |>
  count(bucket, .drop = FALSE, name = "drives") |>
  mutate(total_drives = sum(drives), pct = drives / total_drives)
fd_teams <- fd_team_long |>
  select(posteam, total_drives, bucket, pct) |>
  pivot_wider(names_from = bucket, values_from = pct, names_prefix = "fd_") |>
  arrange(desc(`fd_4+`), fd_0, posteam)
stopifnot(nrow(fd_teams) == 32, abs(sum(fd_league$pct) - 1) < 1e-10,
          all(abs(rowSums(as.data.frame(fd_teams[, 3:7])) - 1) < 1e-10),
          sum(fd_league$drives) == nrow(fd_drives))

out_dir <- "outputs/first_down_drives_2025"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(fd_teams, file.path(out_dir, "team_percentages.csv"), row.names = FALSE)
write.csv(fd_league, file.path(out_dir, "league_percentages.csv"), row.names = FALSE)
write.csv(fd_drives, file.path(out_dir, "drive_audit.csv"), row.names = FALSE)

# gt produces both editable HTML tables and high-resolution PNGs.
bg <- "#F5F0E6"
ink <- "#000000"
muted <- "#000000"
accent <- "#B44A16"
style_table <- function(tbl, subtitle, large = FALSE) {
  tbl |>
    tab_header(title = md("**MOVING THE CHAINS**"), subtitle = subtitle) |>
    tab_source_note("All offensive drives, including kneels and end-of-half drives.") |>
    tab_source_note("First downs include TD plays and penalties. Percentages rounded.") |>
    tab_source_note("Data: nflreadr / nflverse | Ward Walton") |>
    cols_align("center", everything()) |>
    tab_options(
      table.width = px(900), table.font.names = "Arial",
      table.background.color = bg, table.font.color = ink,
      table.font.size = px(if (large) 46 else 26),
      heading.background.color = bg, heading.align = "left",
      heading.title.font.size = px(44), heading.subtitle.font.size = px(23),
      heading.padding = px(24), column_labels.background.color = "#EAE1D2",
      column_labels.font.size = px(22), column_labels.font.weight = "bold",
      data_row.padding = px(if (large) 34 else 13),
      source_notes.font.size = px(17), source_notes.padding = px(7),
      table.border.top.style = "none", table.border.bottom.style = "none",
      table_body.hlines.color = "#D6C9B5",
      column_labels.border.top.color = bg,
      column_labels.border.bottom.color = "#D6C9B5"
    ) |>
    tab_style(cell_text(color = accent), cells_title("subtitle")) |>
    tab_style(cell_text(color = muted), cells_source_notes())
}
save_table <- function(tbl, name) {
  gtsave(tbl, paste0(name, ".html"), path = out_dir)
  gtsave(tbl, paste0(name, ".png"), path = out_dir,
         vwidth = 940, vheight = 1600, zoom = 2, expand = 20)
}

league_table <- fd_league |>
  select(bucket, pct) |>
  gt() |>
  cols_label(bucket = "FIRST DOWNS", pct = "% OF DRIVES") |>
  fmt_percent(pct, decimals = 1) |>
  style_table(sprintf("2025 REGULAR SEASON | %s drives | 272 games",
                      format(nrow(fd_drives), big.mark = ",")), large = TRUE) |>
  tab_style(cell_text(color = accent, weight = "bold"), cells_body(pct))
save_table(league_table, "league_gt")

# Split teams across two slides for phone readability; rank by 4+ share.
team_tables <- lapply(1:2, function(page) {
  teams <- fd_teams |>
    mutate(rank = row_number()) |>
    slice(((page - 1) * 16 + 1):(page * 16)) |>
    select(rank, posteam, starts_with("fd_"))
  tbl <- teams |>
    gt() |>
    cols_label(rank = "#", posteam = "TEAM", fd_0 = "0", fd_1 = "1",
               fd_2 = "2", fd_3 = "3", `fd_4+` = "4+") |>
    tab_spanner("FIRST DOWNS PER DRIVE", starts_with("fd_")) |>
    fmt_percent(starts_with("fd_"), decimals = 1) |>
    style_table(sprintf("2025 REGULAR SEASON | Teams %s–%s | %% of drives",
                        min(teams$rank), max(teams$rank))) |>
    tab_style(cell_text(color = accent, weight = "bold"), cells_body(`fd_4+`)) |>
    tab_source_note("Ranked by share of drives with 4+ first downs (highest first).")
  save_table(tbl, sprintf("teams_gt_%s", page))
  tbl
})
print(fd_league)
if (interactive()) print(league_table)
message("Saved gt PNG and HTML tables plus CSV data to ", out_dir)

# All 32 teams ranked by the share of drives gaining at least two first downs.
two_plus <- fd_drives |>
  group_by(posteam) |>
  summarise(drives = n(), qualifying_drives = sum(first_downs >= 2),
            pct = qualifying_drives / drives, .groups = "drop") |>
  arrange(desc(pct), posteam) |>
  mutate(rank = min_rank(desc(pct)))
stopifnot(nrow(two_plus) == 32,
          all(two_plus$qualifying_drives <= two_plus$drives),
          sum(two_plus$qualifying_drives) == sum(fd_drives$first_downs >= 2))
write.csv(two_plus, file.path(out_dir, "two_plus_percentages.csv"), row.names = FALSE)

team_metadata <- nflreadr::load_teams() |>
  select(team_abbr, team_name, team_logo_espn)
two_plus <- two_plus |>
  left_join(team_metadata, by = c("posteam" = "team_abbr"))
stopifnot(nrow(two_plus) == 32, !anyNA(two_plus$team_logo_espn))
# Cache and embed logos so the saved HTML also works offline.
logo_dir <- file.path(out_dir, "logos")
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)
two_plus$logo <- file.path(logo_dir, paste0(two_plus$posteam, ".png"))
for (i in seq_len(nrow(two_plus))) {
  if (!file.exists(two_plus$logo[i])) {
    download.file(two_plus$team_logo_espn[i], two_plus$logo[i], mode = "wb", quiet = TRUE)
  }
}
two_plus_table <- two_plus |>
  select(rank, logo, team_name, pct) |>
  gt() |>
  cols_label(rank = "#", logo = "", team_name = "TEAM", pct = "2+ FIRST DOWNS") |>
  fmt_percent(pct, decimals = 1) |>
  text_transform(cells_body(logo), fn = function(x) {
    vapply(x, function(path) local_image(path, height = 28), character(1))
  }) |>
  style_table("2025 REGULAR SEASON | Share of offensive drives") |>
  tab_header(title = md("**WHO KEEPS DRIVES ALIVE?**"),
             subtitle = "2025 REGULAR SEASON | Share of offensive drives") |>
  cols_align("left", team_name) |>
  cols_width(rank ~ px(65), logo ~ px(70), team_name ~ px(480), pct ~ px(285)) |>
  tab_style(cell_text(color = accent, weight = "bold"), cells_body(pct)) |>
  tab_options(data_row.padding = px(3), table.font.size = px(24),
              heading.title.font.size = px(40))
save_table(two_plus_table, "two_plus_gt")
if (interactive()) print(two_plus_table)
message("Saved ranked 2+ first-down table with team logos.")
