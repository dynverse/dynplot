# Disabled for now because it's probably not being used

# Plotting edge flips
#
# Utility plotting function for analysing the edge-flip metric from dyneval.
#
# @param oldadj Old adjacency matrix
# @param newadj New adjacency matrix
#
# @keywords compare_trajectory
#
# @returns An edge flip ggplot comparison between two trajectories.
#
# @export
# plot_edge_flips <- function(oldadj, newadj) {
#   # names are used for generating the network, make sure they are present and unique
#   names <- seq_len(nrow(oldadj))
#   dimnames(oldadj) <- list(names, names)
#   dimnames(newadj) <- list(names, names)
#
#   oldnet <- oldadj %>%
#     reshape2::melt(varnames = c("from", "to"), value.name = "old") %>%
#     mutate(old = .data$old == 1) %>%
#     filter(.data$from >= .data$to)
#
#   newnet <- newadj %>%
#     reshape2::melt(varnames = c("from", "to"), value.name = "new") %>%
#     mutate(new = .data$new == 1) %>%
#     filter(.data$from >= .data$to)
#
#   types <- tibble(
#     old = c(FALSE, FALSE, TRUE, TRUE),
#     new = c(FALSE, TRUE, FALSE, TRUE),
#     type = c("irrelevant", "gained", "lost", "stayed")
#   )
#   net <- left_join(oldnet, newnet, by = c("from", "to")) %>%
#     left_join(types, by = c("old", "new"))
#
#   graph <- net %>% tidygraph::as_tbl_graph(directed = FALSE)
#
#   graph <- graph %>%
#     tidygraph::activate(.data$edges) %>%
#     mutate(weight = as.numeric(.data$type != "irrelevant")) %>%
#     arrange(.data$weight)
#
#   ggraph::ggraph(graph, layout = "fr") +
#     ggraph::geom_edge_fan(aes(color = .data$type)) +
#     ggraph::geom_edge_loop(aes(colour = .data$type)) +
#     ggraph::geom_node_label(aes(label = .data$name)) +
#     ggraph::scale_edge_colour_manual(values = c(irrelevant = "#DDDDDD", lost = "#FF4136", stayed = "#0074D9", gained = "#2ECC40")) +
#     theme_graph()
# }
#
