######################### Overview of "Abiturtexte" ############################

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(tidyverse)
library(ggridges)
library(ggthemes)
library(igraph)
library(geodata)
library(sf)

#------------------------------------------------------------------------------#
## Settings / paths -----------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Projects\\CANON-EX")
input_file <- "C:\\Users\\Brottrager\\Documents\\Projects\\CANON-EX\\metadata_abitur.csv"

# state columns (16 German states)
state_cols <- c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV",
                "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")

#------------------------------------------------------------------------------#
## Map ------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

data <- read.csv(".\\Abiturkanon\\overview.csv", sep = ";", 
                 stringsAsFactors = FALSE, check.names = FALSE)

data_summary <- data %>%
  rowwise() %>%
  mutate(total = sum(c_across(-Bundesland), na.rm = TRUE)) %>%
  ungroup()

germany_spat <- geodata::gadm("Germany", level = 1, path = tempdir())
germany_sf <- st_as_sf(germany_spat)

germany_merged <- germany_sf %>%
  left_join(data_summary, by = c("NAME_1" = "Bundesland"))

ggplot(germany_merged) +
  geom_sf(aes(fill = total), colour = "black", size = 0.25) +
  scale_fill_gradient(low = "white", high = "black", guide = "none") +  # <- no legend
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("img//map_states.png", dpi = 360, width = 16, height = 12, 
       units = "cm")

summary_stats <- data_summary %>%
  summarise(
    bundeslaender_mit_daten = sum(total > 0),
    gesamt_bundeslaender = n(),
    anteil_bundeslaender_mit_daten = mean(total > 0),
    durchschnitt_jahre_pro_bundesland = mean(total),
    max_jahre = max(total),
    min_jahre = min(total)
  )

summary_stats

data_summary %>%
  arrange(desc(total)) %>%
  select(Bundesland, total)

#------------------------------------------------------------------------------#
## Utility functions ----------------------------------------------------------#
#------------------------------------------------------------------------------#

pairs_from_list <- function(titles) {
  titles <- unique(titles)             # drop duplicates within one reading list
  if (length(titles) < 2) return(NULL)
  cmb <- t(combn(titles, 2))
  tibble(book1 = cmb[,1], book2 = cmb[,2])
}

make_cooccurrences <- function(df, group_cols = character()) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(books = list(title), .groups = "drop") %>%
    filter(lengths(books) > 1) %>%
    mutate(pairs = map(books, pairs_from_list)) %>%
    select(all_of(group_cols), pairs) %>%
    unnest(cols = c(pairs)) %>%
    group_by(book1, book2) %>%
    summarise(
      count  = n(),
      years  = if ("year" %in% colnames(df)) 
        paste(sort(unique(df$year[df$title %in% c(book1, book2)])), 
              collapse = ", ") else NA_character_,
      states = if ("state" %in% colnames(df)) 
        paste(sort(unique(df$state[df$title %in% c(book1, book2)])), 
              collapse = ", ") else NA_character_,
      .groups = "drop"
    )
}

#------------------------------------------------------------------------------#
## Read data ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

df <- read.csv(input_file, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

#------------------------------------------------------------------------------#
## Convert to long format for co-occurrence / per-state analyses --------------#
#------------------------------------------------------------------------------#

df_long <- df %>%
  pivot_longer(cols = all_of(state_cols), names_to = "state", values_to = "included") %>%
  filter(!is.na(title) & title != "", included == 1) %>%
  select(year, state, title, author_firstname, author_lastname)

# list of all titles (for isolates)
all_books <- sort(unique(df_long$title))

#------------------------------------------------------------------------------#
## Co-occurrence edge lists (various aggregation levels) ----------------------#
#------------------------------------------------------------------------------#

edges_y_s <- make_cooccurrences(df_long, c("year", "state"))
edges_y   <- make_cooccurrences(df_long, c("year"))
edges_s   <- make_cooccurrences(df_long, c("state"))
edges_all <- make_cooccurrences(df_long, c())

write.csv(edges_y_s, "edges_y_s.csv")
write.csv(edges_y,   "edges_y.csv")
write.csv(edges_s,   "edges_s.csv")
write.csv(edges_all, "edges_all.csv")

tibble(book = all_books) %>% write.csv("all_titles.csv")

books_list <- df %>%
  filter(!is.na(title) & title != "") %>%
  distinct(title, author_firstname, author_lastname) %>%
  arrange(title)

write.csv(books_list, "meta_titles.csv")

#------------------------------------------------------------------------------#
## Bipartite network: years <-> texts -----------------------------------------#
#------------------------------------------------------------------------------#

# one row per year-text
edges_bipartite <- df_long %>%
  select(year, title) %>%
  distinct() %>%
  filter(!is.na(year) & !is.na(title))

# define vertices
vertices <- tibble(
  name = unique(c(edges_bipartite$year, edges_bipartite$title)),
  type = if_else(name %in% edges_bipartite$year, TRUE, FALSE)  # TRUE = year, FALSE = text
)

# create bipartite graph
g_bipartite <- graph_from_data_frame(edges_bipartite, vertices = vertices, directed = FALSE)

edges_bipartite_export <- edges_bipartite %>%
  rename(source = year, target = title)

write.csv(edges_bipartite_export, "bipartite_year_text_edgelist.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
## Bipartite edge list: states <-> texts --------------------------------------#
#------------------------------------------------------------------------------#

# Filter only included texts per state
df_bipartite <- df_long %>%
  select(state, title) %>%
  distinct()  # remove duplicates

# Optional: rename columns for clarity
edges_bipartite_states <- df_bipartite %>%
  rename(source = state, target = title)

write.csv(edges_bipartite_states, "bipartite_state_text_edgelist.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
## Per-row states count + basic summaries (text-level & author-level) ---------#
#------------------------------------------------------------------------------#

df <- df %>%
  mutate(states = rowSums(select(., all_of(state_cols)) == 1, na.rm = TRUE))

# text-level summary (one row per text)
text_summary <- df %>%
  group_by(title, author_lastname, author_firstname, pub_year_clean, gender, epoch) %>%
  summarise(
    total_states = sum(states, na.rm = TRUE),
    n_years = n_distinct(year),
    first_year_listed = min(year, na.rm = TRUE),
    last_year_listed  = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_states))

write.csv(text_summary, "text_summary.csv")

# author-level summary (aggregate across texts)
author_summary <- df %>%
  group_by(author_lastname, author_firstname) %>%
  summarise(
    total_states = sum(states, na.rm = TRUE),
    n_titles = n_distinct(title),
    n_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(desc(total_states))

write.csv(author_summary, "author_summary.csv")

#------------------------------------------------------------------------------#
## Author heatmap (authors appearing in >4 years), ordered by peak year -------#
#------------------------------------------------------------------------------#

author_yearly <- df %>%
  group_by(year, author_lastname, author_firstname) %>%
  summarise(states = sum(states, na.rm = TRUE), .groups = "drop")

authors_multi <- author_yearly %>%
  group_by(author_lastname, author_firstname) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  filter(n_years > 2)

author_heatmap <- author_yearly %>%
  semi_join(authors_multi, by = c("author_lastname", "author_firstname")) %>%
  group_by(year, author_lastname, author_firstname) %>%
  summarise(total_states = sum(states, na.rm = TRUE), .groups = "drop")

peak_years_author <- author_heatmap %>%
  group_by(author_lastname, author_firstname) %>%
  slice_max(order_by = total_states, n = 1, with_ties = FALSE) %>%
  select(author_lastname, author_firstname, peak_year = year)

author_heatmap <- author_heatmap %>%
  left_join(peak_years_author, by = c("author_lastname", "author_firstname")) %>%
  mutate(author_label = paste(author_firstname, author_lastname)) %>%
  mutate(author_label = gsub("^nan ", "", author_label))

author_order <- author_heatmap %>%
  distinct(author_label, peak_year) %>%
  arrange(peak_year) %>%
  pull(author_label)

author_heatmap$author_label <- factor(author_heatmap$author_label, 
                                      levels = author_order)

ggplot(author_heatmap, aes(x = year, y = author_label, fill = total_states)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "lightgrey", high = "black", guide = "none") +
  labs(x = "Jahr", y = "Autor:in") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 7),
        plot.margin = margin(t = 4, r = 20, b = 4, l = 4))

ggsave("img/author_heatmap.png", dpi = 360, width = 16, height = 12, units = "cm")

#------------------------------------------------------------------------------#
## Text heatmap (texts appearing in >4 years), ordered by peak year -----------#
#------------------------------------------------------------------------------#

text_yearly <- df %>%
  group_by(year, title, author_lastname, author_firstname) %>%
  summarise(states = sum(states, na.rm = TRUE), .groups = "drop")

texts_multi <- text_yearly %>%
  group_by(title, author_lastname, author_firstname) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  filter(n_years > 4)

text_multi <- text_yearly %>%
  semi_join(texts_multi, by = c("title", "author_lastname", "author_firstname"))

title_order <- text_multi %>%
  group_by(title) %>%
  summarise(total_states = sum(states, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_states)) %>%
  pull(title)

text_multi <- text_multi %>%
  mutate(title = factor(title, levels = title_order))

peak_years_text <- text_multi %>%
  group_by(title) %>%
  slice_max(states, n = 1, with_ties = FALSE) %>%
  select(title, peak_year = year)

text_multi <- text_multi %>%
  left_join(peak_years_text, by = "title") %>%
  mutate(title = fct_reorder(title, peak_year))

ggplot(text_multi, aes(x = year, y = title, fill = states)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "lightgrey", high = "black", guide = "none") +
  labs(x = "Jahr", y = "Titel") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 7))

ggsave("img//text_heatmap.png", dpi = 360, width = 16, height = 12, 
       units = "cm")

#------------------------------------------------------------------------------#
## Gender proportion (percentage of women authors) ----------------------------#
#------------------------------------------------------------------------------#

gender_share <- df %>%
  group_by(year, gender) %>%
  summarise(n_authors = n_distinct(paste(author_firstname, author_lastname)), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = n_authors / sum(n_authors)) %>%
  filter(gender == "f") %>%
  ungroup() %>%
  complete(year = full_seq(min(df$year, na.rm = TRUE):max(df$year, na.rm = TRUE), 1), fill = list(share = 0))

ggplot(gender_share, aes(x = year, y = share)) +
  geom_line(colour = "black", size = 1) +
  geom_point(colour = "black", size = 2) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Jahr", y = "Anteil von Autorinnen") +
  theme_minimal(base_size = 12)

ggsave("img//gender_proportion.png", dpi = 360, width = 16, height = 6, 
       units = "cm")

#------------------------------------------------------------------------------#
## Biggest winners / losers (start vs end periods) ----------------------------#
#------------------------------------------------------------------------------#

start_years <- 2006:2010
end_years   <- 2021:2026

author_trends <- df %>%
  group_by(author_lastname, author_firstname, year) %>%
  summarise(n_titles = n_distinct(title), .groups = "drop") %>%
  mutate(period = case_when(
    year %in% start_years ~ "start",
    year %in% end_years   ~ "end",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(author_lastname, author_firstname, period) %>%
  summarise(years_active = n_distinct(year), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = period, values_from = years_active, values_fill = 0) %>%
  mutate(change = end - start) %>%
  arrange(desc(change))

author_trends$author_firstname <- gsub("nan", "", author_trends$author_firstname)

top_winners <- author_trends %>% slice_max(change, n = 5)
top_losers  <- author_trends %>% slice_min(change, n = 5)

ggplot(author_trends, aes(x = reorder(paste(author_firstname, author_lastname), 
                                      change), y = change)) +
  geom_col(aes(fill = change > 0), colour = "black", alpha = .6) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "black"), guide = "none") +
  labs(x = "Autor:in", y = "Veränderung der Anzahl aktiver Jahre") +
  theme_minimal(base_size = 12)

ggsave("img//winner_loser.png", dpi = 360, width = 20, height = 24, 
       units = "cm")

#------------------------------------------------------------------------------#
## Save key outputs -----------------------------------------------------------#
#------------------------------------------------------------------------------#

write_csv(text_summary, "text_summary.csv")
write_csv(author_summary, "author_summary.csv")
write_csv(author_consistency, "author_consistency.csv")
write_csv(author_volatility, "author_volatility.csv")
write_csv(author_trends, "author_trends_start_end.csv")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
