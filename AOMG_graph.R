library(dplyr) 
library(igraph)
library(ggraph)
library(tidygraph)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)
library(visNetwork)
library(htmlwidgets)

dat <- read_xlsx("data/Mini_Project_Artist_Disco.xlsx")

current_artist <- c("YUGYEOM", "Hoody", "punchnello", "ELO", "Jvcki Wai", "Ugly Duck", "SIKKOO")
all_aomg <- unique(dat$Artist)
prev_aomg <- setdiff(all_aomg,current_artist)


all_artists <- unique(c(dat$Artist, dat$Feat)) %>%
  strsplit(",\\s*") %>% unlist() %>% unique()
all_artists1 <- all_artists[!is.na(all_artists)]

get_groups <- function(name) {
  groups <- c()
  if (name %in% current_artist) groups <- c(groups, "Current AOMG Artist")
  if (name %in% prev_aomg) groups <- c(groups, "Prev. AOMG Artist")
  if (length(groups) == 0) return("Non AOMG Artist")
  return(paste(groups, collapse = " / "))
}

 
nodes <- data.frame(id = all_artists1, label = all_artists1, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    group = get_groups(id),  # for color
  ) %>%
  ungroup()

edges <- dat %>%
  filter(!is.na(Artist) & !is.na(Feat)) %>%
  filter(!is.na(Feat)) %>%
  separate_rows(Artist, Feat, sep = ",\\s*") %>%
  select(from = Artist, to = Feat) %>%
  count(from, to, name = "weight") 

edges <- edges %>%
  mutate(
    color = ifelse(from %in% all_aomg & to %in% all_aomg, "purple", "gray")
  )


# interactive plot
graph <- visNetwork(nodes, edges, width = "100%", height = "900px") %>%
  visGroups(groupname = "Current AOMG Artist", color = "hotpink") %>%
  visGroups(groupname = "Prev. AOMG Artist", color = "lightblue") %>%
  visGroups(groupname = "Non AOMG Artist", color = "lightgrey") %>%
  visNodes(
    font = list(size = 50, valign = "top"),
    color = list(
      highlight = list(background = "inherit", border = "inherit"),
      hover = list(background = "inherit", border = "inherit")
    )
  ) %>%
  visOptions(
    selectedBy = list(
      variable = "group",
      values = unique(nodes$group)
    ),
    highlightNearest = list(
      enabled = TRUE,
      degree = 0,
      hover = FALSE,
      algorithm = "all"  # ensures both dropdowns trigger highlights
    ),
    nodesIdSelection = list(
      enabled = TRUE,
      values = nodes$id[nodes$id %in% all_aomg]
    )
  ) %>%
  visEdges(
    smooth = TRUE,
    scaling = list(min = 1, max = 5),
    width = "weight"
  ) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 400,  # increase to push nodes farther apart
      springLength = 200,
      springConstant = 0.01,
      damping = 0.1
    ),
    stabilization = list(enabled = FALSE, iterations = 500)
  )%>%
  visInteraction(navigationButtons = TRUE) %>%
  visLegend() 




#saveWidget(graph, file = "docs/index.html", selfcontained = TRUE)

# stagnate graph highlighting the AOMG network with a focus on the current ones
songs_per_artist_main <- dat %>%
  mutate(.song_id = row_number()) %>%        
  filter(!is.na(Artist)) %>%
  separate_rows(Artist, sep = ",\\s*") %>%
  mutate(Artist = trimws(Artist)) %>%
  filter(Artist != "", Artist %in% all_aomg) %>%   
  distinct(Artist, .song_id) %>%           
  count(Artist, name = "songs_main")

nodes_aomg <- data.frame(
  id = all_aomg,
  label = all_aomg,
  stringsAsFactors = FALSE
) %>%
  mutate(
    group = if_else(id %in% current_artist,
                    "Current AOMG Artist", "Prev. AOMG Artist")
  ) %>%
  left_join(songs_per_artist_main, by = c("id" = "Artist")) %>%
  mutate(songs_main = tidyr::replace_na(songs_main, 0L))  


edges_aomg <- dat %>%
  filter(!is.na(Artist), !is.na(Feat)) %>%
  separate_rows(Artist, Feat, sep = ",\\s*") %>%
  mutate(Artist = trimws(Artist), Feat = trimws(Feat)) %>%
  filter(Artist %in% all_aomg, Feat %in% all_aomg) %>%
  count(from = Artist, to = Feat, name = "weight") %>%
  mutate(arrows = "to")

nodes_ig <- nodes_aomg %>% rename(name = id)

g <- graph_from_data_frame(
  d = edges_aomg,           
  vertices = nodes_ig,      
  directed = TRUE
)

# keep only vertices with degree > 0 (in OR out)
g <- igraph::induced_subgraph(g, vids = igraph::V(g)[igraph::degree(g, mode = "all") > 0])

set.seed(428)  
p <- ggraph(g, layout = "fr") +   
  geom_edge_link(
    aes(edge_width = weight),
    arrow   = arrow(length = unit(3, "mm")),
    end_cap = circle(2, "mm"),
    edge_alpha = 0.5
  ) +
  geom_node_point(aes(color = group, size = songs_main)) +
  geom_node_text(aes(label = label), repel = FALSE, size = 3.5, fontface = "bold") +
  scale_edge_width(range = c(0.3, 1.8), guide = "none") +
  scale_color_manual(
    values = c(
      "Current AOMG Artist" = "purple",
      "Prev. AOMG Artist"   = "grey"
    )
  ) +
  guides(
    color = guide_legend(title = NULL, override.aes = list(size = 6)), 
    size = "none", 
    edge_alpha = "none"
  )+
  labs(
    title = "AOMG Music Artist Network",
    subtitle = "Directed graph of song collaborations within AOMG: arrows point to featured artists.\nNode size represents total songs released; color indicates genre.",
    caption = "Graph by Hortencia Josefina Hernandez·\nData: AOMG artist discography"
  ) +
  scale_size_area(max_size = 18, limits = c(0, max(igraph::V(g)$songs_main, na.rm = TRUE)),
                  guide = "none") +
  theme_void() +
  theme(
    legend.position  = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.key.size = unit(1.2, "cm") 
    #panel.background = element_rect(fill = "transparent", colour = NA),
    #plot.background  = element_rect(fill = "transparent",  colour = NA)
  )


#ggsave("images/AOMG_graph.png", p, width = 12, height = 8, dpi = 300)


#----Collab Proportion----

collab_summary <- dat %>%
  mutate(across(c(Artist, Feat), as.character)) %>%
  filter(!is.na(Artist), !is.na(Feat)) %>%
  separate_rows(Artist, Feat, sep = ",\\s*") %>%
  mutate(
    Artist = str_trim(Artist),
    Feat   = str_trim(Feat)
  ) %>%
  filter(Artist != "", Feat != "") %>%
  # keep only AOMG main artists pointing to their featured artists
  filter(Artist %in% all_aomg) %>%
  distinct(Artist, Feat) %>%                 # <-- unique collaborators per artist
  group_by(Artist) %>%
  summarise(
    n_collab_total = n_distinct(Feat),
    n_collab_aomg  = n_distinct(Feat[Feat %in% all_aomg]),
    n_collab_non   = n_collab_total - n_collab_aomg,
    prop_aomg      = if_else(n_collab_total > 0, n_collab_aomg / n_collab_total, NA_real_),
    prop_non       = 1 - prop_aomg,
    .groups = "drop"
  ) %>%
  arrange(desc(n_collab_total))

#collab_summary

plot_props <- collab_summary %>%
  tidyr::pivot_longer(c(n_collab_aomg, n_collab_non),
                      names_to = "type", values_to = "n") %>%
  mutate(type = dplyr::recode(type,
                              n_collab_aomg = "AOMG collaborators",
                              n_collab_non  = "Non-AOMG collaborators"))

p2 <- ggplot(plot_props,
       aes(x = reorder(Artist, n_collab_total), 
           y = n, fill = type)) +
    geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c("AOMG collaborators" = "purple",
               "Non-AOMG collaborators" = "lightgray"),
    breaks = c("AOMG collaborators", "Non-AOMG collaborators"),
    guide = guide_legend(title = NULL)
  ) +
  labs(x = "",y = "Percentage of collaborators") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold"))

#ggsave("images/AOMG_collab_percent.png", p2, width = 9, height = 8, dpi = 300)

#----IN/OUT-DEG----

g_all <- graph_from_data_frame(
  d = edges_aomg,                                    
  vertices = nodes_aomg %>% dplyr::rename(name = id),
  directed = TRUE
)

# Degrees (counts of unique edges), per artist
in_deg  <- igraph::degree(g_all, mode = "in")
out_deg <- igraph::degree(g_all, mode = "out")


songs_tbl <- songs_per_artist_main %>%
  rename(Artist = Artist, discography_total = songs_main)

degree_table <- tibble(
  Artist     = names(out_deg),
  in_degree  = as.integer(in_deg[Artist]),
  out_degree = as.integer(out_deg[Artist])
) %>%
  inner_join(tibble(Artist = current_artist), by = "Artist") %>%
  left_join(nodes_aomg %>% select(Artist = id, group), by = "Artist") %>%
  left_join(songs_tbl, by = "Artist") %>%
  mutate(
    discography_total = replace_na(discography_total, 0L)
  ) %>%
  arrange(desc(discography_total),desc(out_degree), desc(in_degree))

#degree_table


# --- Centralities (AOMG-only subgraph) ---
# stronger tie = shorter distance
btw  <- igraph::betweenness(g_all, directed = TRUE, weights = E(g_all)$dist, normalized = TRUE)

# Closeness has direction choices:
clo_out <- igraph::closeness(g_all, mode = "out", weights = E(g_all)$dist, normalized = TRUE) 
clo_in  <- igraph::closeness(g_all, mode = "in",  weights = E(g_all)$dist, normalized = TRUE)  




cent_tbl <- tibble::tibble(
  Artist        = igraph::V(g_all)$name,
  betweenness   = as.numeric(btw),
  closeness_out = as.numeric(clo_out), # how easily a node reaches others
  closeness_in  = as.numeric(clo_in) # how easily others reach a node
)

cent_tbl %>%
  arrange(desc(betweenness),desc(closeness_out), desc(closeness_in)) %>%
  filter(Artist %in% current_artist)


# --- FULL graph: use ALL artists and ALL collabs ---
edges_full <- dat %>%
  filter(!is.na(Artist), !is.na(Feat)) %>%
  separate_rows(Artist, Feat, sep = ",\\s*") %>%
  mutate(Artist = trimws(Artist), Feat = trimws(Feat)) %>%
  filter(Artist != "", Feat != "") %>%
  count(from = Artist, to = Feat, name = "weight") %>%
  mutate(arrows = "to")

nodes_full <- tibble::tibble(
  id = all_artists1,
  label = all_artists1
) %>%
  mutate(
    group = dplyr::case_when(
      id %in% current_artist ~ "Current AOMG Artist",
      id %in% prev_aomg      ~ "Prev. AOMG Artist",
      TRUE                   ~ "Non AOMG Artist"
    )
  )

g_full <- graph_from_data_frame(
  d = edges_full,
  vertices = nodes_full %>% dplyr::rename(name = id),
  directed = TRUE
)

btw_full  <- igraph::betweenness(g_full, directed = TRUE, weights = E(g_full)$dist, normalized = TRUE)
clo_out_f <- igraph::closeness(g_full, mode = "out", weights = E(g_full)$dist, normalized = TRUE)
clo_in_f  <- igraph::closeness(g_full, mode = "in",  weights = E(g_full)$dist, normalized = TRUE)

cent_tbl_full <- tibble::tibble(
  Artist        = igraph::V(g_full)$name,
  betweenness   = as.numeric(btw_full),
  closeness_out = as.numeric(clo_out_f),
  closeness_in  = as.numeric(clo_in_f)
)

cent_tbl_full %>%
  arrange(desc(betweenness),desc(closeness_out), desc(closeness_in))


