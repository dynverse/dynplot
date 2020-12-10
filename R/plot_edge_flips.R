#' Plotting edge flips
#'
#' Utility plotting function for analysing the edge-flip metric from dyneval.
#'
#' @param oldadj Old adjacency matrix
#' @param newadj New adjancency matrix
#'
#' @keywords compare_trajectory
#'
#' @export
plot_edge_flips <- function(oldadj, newadj) {
  # names are used for generating the network, make sure they are present and unique
  names <- seq_len(nrow(oldadj))
  dimnames(oldadj) <- list(names, names)
  dimnames(newadj) <- list(names, names)

  oldnet <- oldadj %>%
    reshape2::melt(varnames = c("from", "to"), value.name = "old") %>%
    mutate(old = old == 1) %>%
    filter(from >= to)

  newnet <- newadj %>%
    reshape2::melt(varnames = c("from", "to"), value.name = "new") %>%
    mutate(new = new == 1) %>%
    filter(from >= to)

  types <- tibble(old = c(F, F, T, T), new = c(F, T, F, T), type = c("irrelevant", "gained", "lost", "stayed"))
  net <- left_join(oldnet, newnet, by = c("from", "to")) %>%
    left_join(types, by = c("old", "new"))

  graph <- net %>% tidygraph::as_tbl_graph(directed = FALSE)

  graph <- graph %>%
    tidygraph::activate(edges) %>%
    mutate(weight = as.numeric(type != "irrelevant")) %>%
    arrange(weight)

  ggraph::ggraph(graph, layout = "fr") +
    ggraph::geom_edge_fan(aes(color = type)) +
    ggraph::geom_edge_loop(aes(colour = type)) +
    ggraph::geom_node_label(aes(label = name)) +
    ggraph::scale_edge_colour_manual(values = c(irrelevant = "#DDDDDD", lost = "#FF4136", stayed = "#0074D9", gained = "#2ECC40")) +
    theme_nothing()
}

# copied from cowplot to reduce dynplot dependencies
#' @importFrom ggplot2 %+replace%
theme_nothing <- function(font_size = 14, font_family = "", rel_small = 12/14){
  theme_void(base_size = font_size, base_family = font_family) %+replace%
    theme(
      # Elements in this first block aren't used directly, but are inherited
      line = element_blank(),
      rect = element_blank(),
      text = element_text(
        family = font_family, face = "plain",
        color = "black", size = font_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
        margin = margin(), debug = FALSE
      ),

      axis.line = element_blank(),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.text = element_blank(),
      axis.text.x = NULL,
      axis.text.x.top = NULL,
      axis.text.y = NULL,
      axis.text.y.right = NULL,
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),
      axis.title.x = NULL,
      axis.title.x.top = NULL,
      axis.title.y = NULL,
      axis.title.y.right = NULL,

      legend.background = element_blank(),
      legend.spacing = unit(font_size, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = margin(0, 0, 0, 0),
      legend.key = element_blank(),
      legend.key.size = unit(1.1*font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position = "none",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL,
      legend.box.margin = margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = NULL,
      panel.grid.minor = NULL,
      panel.spacing = unit(font_size / 2, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,

      strip.background = element_blank(),
      strip.text = element_blank(),
      strip.text.x = NULL,
      strip.text.y = NULL,
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(0., "cm"),
      strip.switch.pad.wrap = unit(0., "cm"),

      plot.background = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.tag = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),

      plot.margin = margin(0, 0, 0, 0),

      complete = TRUE
    )
}
