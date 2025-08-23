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




saveWidget(graph, file = "docs/index.html", selfcontained = TRUE)

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
      "Current AOMG Artist" = "hotpink",
      "Prev. AOMG Artist"   = "lightblue"
    )
  ) +
  guides(size = "none", color = guide_legend(title = NULL)) +
  labs(
    title = "AOMG Music Artist Network",
    subtitle = "Directed graph of song collaborations within AOMG: arrows point to featured artists.\nNode size represents total songs released; color indicates genre.",
    caption = "Graph by Hortencia Josefina Hernandez·\nData: AOMG artist discography"
  ) +
  theme_void() +
  theme(
    legend.position  = "right",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent",  colour = NA)
  )

kkk

ggsave("images/AOMG_graph.png", p, width = 12, height = 8, dpi = 300, bg = "transparent")
