add_cell_coloring <- dynutils::inherit_default_params(add_milestone_coloring, add_cell_coloring)
plot_dendro <- dynutils::inherit_default_params(add_cell_coloring, plot_dendro)
plot_onedim <- dynutils::inherit_default_params(add_cell_coloring, plot_onedim)
plot_graph <- dynutils::inherit_default_params(list(add_cell_coloring,add_milestone_coloring), plot_graph)
