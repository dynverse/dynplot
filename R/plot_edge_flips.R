#' Plotting edge flips
#'
#' Utility plotting function for analysing the edge-flip metric from dyneval.
#'
#' @param oldadj Old adjacency matrix
#' @param newadj New adjancency matrix
#'
#' @importFrom cowplot theme_nothing
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
    cowplot::theme_nothing()
}
