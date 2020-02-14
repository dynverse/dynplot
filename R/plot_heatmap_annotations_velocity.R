calculate_cell_trajectory_velocity <- function(
  dataset,
  trajectory
) {
  assert_that(!is.null(dataset$velocity))

  message("Calculating RNA velocity along the trajectory")

  dimred_traj <- calculate_trajectory_dimred(trajectory)

  # get milestone network and MAX progressions
  milestone_network <- trajectory$milestone_network
  progressions <- trajectory$progressions %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    ungroup()

  # get actual vectors of milestone network
  milestone_network_vectors <- milestone_network %>%
    left_join(dimred_traj$milestone_positions %>% select(from = milestone_id, comp_1_from = comp_1, comp_2_from = comp_2), "from") %>%
    left_join(dimred_traj$milestone_positions %>% select(to = milestone_id, comp_1_to = comp_1, comp_2_to = comp_2), "to") %>%
    mutate(
      comp_1 = comp_1_to - comp_1_from,
      comp_2 = comp_2_to - comp_2_from
    ) %>%
    select(from, to, comp_1, comp_2)

  # get vectors of cells based on RNA velocity in embedding
  dimred <- dimred_traj$cell_positions %>%
    select(-cell_id) %>%
    as.matrix()
  rownames(dimred) <- dimred_traj$cell_positions$cell_id
  dimred <- dimred[dataset$cell_ids,]

  dimred_velocity <- embed_velocity(dataset, dimred = dimred)

  cell_vectors <- (dimred_velocity - dimred) %>%
    as.data.frame() %>%
    rownames_to_column("cell_id")

  cell_vectors_mapped <- left_join(
    progressions %>%
      ungroup() %>%
      select(cell_id, from, to),
    cell_vectors,
    "cell_id"
  )

  # calculate for each cell its trajectory velocity vector
  # first calculate the angle of the trajectory and velocity vectors
  # then subtract these to get the angle between the two vectors
  # then project the cell vector on the trajectory vector using the cos(angle) and its magnitude
  cell_trajectory_velocities <- left_join(
    milestone_network_vectors %>% rename(comp_1_trajectory = comp_1, comp_2_trajectory = comp_2),
    cell_vectors_mapped %>% rename(comp_1_cell = comp_1, comp_2_cell = comp_2),
    c("from", "to")
  ) %>%
    mutate(
      angle_trajectory = atan2(comp_2_trajectory, comp_1_trajectory),
      angle_velocity = atan2(comp_2_cell, comp_1_cell),
      angle = angle_velocity - angle_trajectory,
      velocity = cos(angle) * sqrt(comp_1_cell**2 + comp_2_cell**2),

      # alternative implementations to calculate it in one line, but less understandable
      velocity2 = rowSums(cbind(comp_1_trajectory, comp_2_trajectory) * cbind(comp_1_cell, comp_2_cell)) / sqrt(rowSums(cbind(comp_1_trajectory, comp_2_trajectory)^2))
    )

  cell_trajectory_velocities %>%
    select(cell_id, velocity)
}







annotate_velocity <- function(
  dataset,
  trajectory = dataset,
  linearised,
  plot_velocity = case_when(
    !is.null(dataset$velocity) ~ "top",
    TRUE ~ "none"
  ),
  velocity_each = as.integer(length(trajectory$cell_ids) / 100)
) {
  assert_that(plot_velocity %in% c("top", "bottom", "none"))
  assert_that(length(plot_velocity) == 1)

  cell_trajectory_velocities <- calculate_cell_trajectory_velocity(dataset, trajectory)

  # select waypoint cells
  waypoint_cells <- linearised$progressions %>%
    dplyr::filter((row_number() %% velocity_each) == 0) %>%
    pull(cell_id)

  # calculate for each waypoint cell the average velocity using moving window
  cell_waypoint_trajectory_velocities <- linearised$progressions %>%
    group_by(from, to) %>%
    mutate(group = floor(row_number() / velocity_each)) %>%
    group_by(from, to, group) %>%
    summarise(
      cell_ids = list(cell_id),
      cell_id = first(cell_id)
    ) %>%
    mutate(velocity = map_dbl(cell_ids, function(cell_ids) {
      cell_trajectory_velocities %>%
        filter(cell_id %in% cell_ids) %>%
        pull(velocity) %>%
        mean()
    })) %>%
    ungroup() %>%
    mutate(normalized_velocity = velocity / max(velocity)) %>%
    select(cell_id, velocity, normalized_velocity)

  # add index, so we can easily extract the correct cells within the annotation using the index
  cell_waypoint_trajectory_velocities$index <- match(cell_waypoint_trajectory_velocities$cell_id, linearised$progressions$cell_id)


  annotation_velocity = ComplexHeatmap::AnnotationFunction(
    fun = function(index, k, n) {
      n = length(index)
      pushViewport(viewport(xscale = c(0, n), yscale = c(0, 1)))

      cell_ids <- linearised$progressions[index, ]$cell_id
      cell_waypoint_trajectory_velocities <- cell_waypoint_trajectory_velocities %>%
        dplyr::filter(cell_id %in% cell_ids) %>%
        mutate(
          width = velocity_each * normalized_velocity,
          x_min = match(index, !!index), # the start of the "box" containing the vectors
          x_mid = x_min + velocity_each / 2,
          x_from = x_mid - width / 2,
          x_to = x_mid + width / 2,
        )

      y <- unit(0.5, "native")

      base_arrow_length <- arrow()$length

      for (i in seq_len(nrow(cell_waypoint_trajectory_velocities))) {
        row <- extract_row_to_list(cell_waypoint_trajectory_velocities, i)
        grid.lines(
          x = unit.c(unit(row$x_from, "native"), unit(row$x_to, "native")),
          y = unit.c(y, y),
          gp = gpar(fill = "#333333"),
          arrow = ggplot2::arrow(length = base_arrow_length * abs(row$normalized_velocity), type = "closed")
        )
      }

      popViewport()
    },
    var_import = list(
      linearised = linearised,
      cell_waypoint_trajectory_velocities = cell_waypoint_trajectory_velocities,

      # parameters for positioning
      velocity_each = velocity_each
    ),
    n = nrow(linearised$progressions),
    subsetable = FALSE,
    height = unit(1, "cm")
  )

  legend_velocity <- ComplexHeatmap::Legend(
    labels = c("Strong", "Medium", "Weak"),
    title = "RNA velocity",
    type = "points",
    pch = "\U25BA",
    legend_gp = gpar(fill = "#333333", fontsize = c(20, 10, 5)),
    background = NULL,
    direction = "horizontal",
    title_position = "topcenter"
  )

  lst(
    annotation_velocity,
    legend_velocity
  )
}
