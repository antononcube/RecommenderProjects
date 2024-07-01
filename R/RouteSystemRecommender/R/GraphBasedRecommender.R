# Graph-based recommenders
#

#' @import igraph
#' @import geohashTools
#' @import SparseMatrixRecommemder
#' @import SMRMon
NULL

#===========================================================
#' Make route graph
#' @description Makes a route graph from package's generic set of edges.
#' @param precision Geohash precision to use.
#' @param directed Should the graph be directed or not?
#' @details Package's edge data is with geohash precision 9.
#' Hence, `precision` should be 9 or less.
#' @export
make_route_graph <- function(precision = 5, directed = FALSE) {
  dsEdges <-
    dfUSARouteSystemGraphEdges %>%
    dplyr::mutate(from = substr(from, 1, precision), to = substr(to, 1, precision)) %>%
    dplyr::filter(from != to) %>%
    unique

  return(igraph::graph_from_data_frame(dsEdges, directed = directed))
}

#===========================================================
#' Closest Geohash vertex from a graph
#' @description Finds the nearest neighbor Geohash tile for a given Geo-point.
#'
closest_geohash_vertex <- function(graph, gh, matCoords = NULL) {

  if ( gh %in% igraph::V(graph) ) {
    return(gh)
  }

  res <- geohashTools::gh_decode(gh)

  latitude <- res$latitude
  longitude <- res$longitude

  # Make the matrix of all vertex points
  if ( !is.matrix(matCoords) ) {
    matCoords <- purrr::map_df(igraph::V(graph)$name, function(x) { geohashTools::gh_decode(x) })
    matCoords <- as.matrix(matCoords)
    rownames(matCoords) <- igraph::V(graph)$name
  }

  # Make the matrix for the focus point
  focusPoint <- c(latitude, longitude)
  matFocusPoint <- matrix(rep_len(x = focusPoint, length.out = 2*nrow(matCoords)), byrow = T, ncol = 2)

  # Squared Euclidean distance
  lsDists <- rowSums((matFocusPoint - matCoords) * (matFocusPoint - matCoords))

  # Closest vertex
  pos <- which.min(lsDists)

  names(pos)
}

#===========================================================
#' Find route graph paths
#' @description Find shortest paths for a given data frame with start and end locations
#' over a given route graph.
#' @param data Data frame with columns "id", "lat1", "lon1", "lat2", 'lon2".
#' @param graph Route system graph.
#' If NULL a geohashes route graph with precision 4 is used.
#' @details Generally speaking any graph based on geohash can be used,
#' but the main use case of this function is to use graphs based on a route system.
#' @export
find_route_graph_paths <- function(data, graph = NULL) {

  if (is.null(graph)) {
    graph <- make_route_graph(precision = 4, directed = FALSE)
  }

  if (!igraph::is_igraph(graph)) {
    stop("The argument graph is expected to be an iGraph graph object or NULL", call. = TRUE)
  }

  expectedColnames <- c("id", "lat1", "lon1", "lat2", "lon2")
  if ( !(is.data.frame(data) && sum(colnames(data) %in% expectedColnames) >= 5) ) {
    stop(paste0("The argument data is expected to be a data frame with the columns \"", paste0(expectedColnames,'", "'), "."))
  }

  precision <- nchar(igraph::V(graph)$name[[1]])

  dfDataGeohashes <-
    purrr::map_df(1:nrow(data), function (i) {
      c(
        id = data[i, "id"],
        start  = geohashTools::gh_encode( longitude = data[i, "lon1"], latitude = data[i, "lat1"], precision = precision),
        end = geohashTools::gh_encode( longitude = data[i, "lon2"], latitude = data[i, "lat2"], precision = precision)
      )
    })

  # Matrix of vertex coordinates
  matCoords <- purrr::map_df(igraph::V(graph)$name, function(x) { geohashTools::gh_decode(x) })
  matCoords <- as.matrix(matCoords)
  rownames(matCoords) <- igraph::V(graph)$name

  # Paths
  aPaths <-
    purrr::map(1:nrow(dfDataGeohashes), function (i) {
      dfX <- dfDataGeohashes[i,]

      path <- igraph::shortest_paths(graph = graph,
                                     from = closest_geohash_vertex(graph = graph, gh = dfX$start, matCoords = matCoords),
                                     to = closest_geohash_vertex(graph = graph, gh = dfX$end, matCoords = matCoords),
                                     output = "vpath"
                                     )

      path <- path$vpath[[1]]
    })

  aPaths <- setNames(aPaths, dfDataGeohashes$id)

  # Result
  aPaths
}

#===========================================================
#' Make graph paths recommender
#' @description Creates are a reommender for a given data frame with start and end locations
#' over a given route graph.
#' @param data Data frame with columns "id", "lat1", "lon1", "lat2", 'lon2".
#' @param graph Route system graph.
#' If NULL a geohashes route graph with precision 4 is used.
#' @details Generally speaking any graph based on geohash can be used,
#' but the main use case of this function is to use graphs based on a route system.
#' @export
make_graph_paths_recommender <- function(data, graph = NULL) {

  aPaths <- find_route_graph_paths(data, graph);

  dfPathsLong <-
    purrr::map_df( 1:length(aPaths), function(i) {
      data.frame( Item = names(aPaths)[[i]], TagType = 'PathTile', Value = aPaths[[i]], Weight = 1)
    })

  dfPathsLong <- dfPathsLong %>% dplyr::filter(!is.na(Value))

  smrPaths <- SparseMatrixRecommender::SMRCreateFromLongForm(data = dfPathsLong)

  return(smrPaths)
}
