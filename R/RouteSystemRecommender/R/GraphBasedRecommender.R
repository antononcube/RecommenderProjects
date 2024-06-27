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

  precision <- nchar(V(graph)$name[[1]])

  dfDataGeohashes <-
    purrr::map_df(1:nrow(data), function (i) {
      c(
        id = data[i, "id"],
        start  = geohashTools::gh_encode( longitude = data[i, "lon1"], latitude = data[i, "lat1"], precision = precision),
        end = geohashTools::gh_encode( longitude = data[i, "lon2"], latitude = data[i, "lat2"], precision = precision)
      )
    })

  aPaths <-
    purrr::map(1:nrow(dfDataGeohashes), function (i) {
      dfX <- dfDataGeohashes[i,]
      if ( (dfX$start %in% V(graph)$name) && dfX$end %in% V(graph)$name ) {
        path <- igraph::shortest_paths(graph, from = dfX$start, to = dfX$end, output = "vpath")$vpath[[1]]
        if ( length(path) > 0) {
          path$name
        } else {
          NA
        }
      } else {
        NA
      }
    })

  aPaths <- setNames(aPaths, dfDataGeohashes$id)


  dfPathsLong <-
    purrr::map_df( 1:length(aPaths), function(i) {
      data.frame( Item = names(aPaths)[[i]], TagType = 'PathTile', Value = aPaths[[i]], Weight = 1)
    })

  dfPathsLong <- dfPathsLong %>% dplyr::filter(!is.na(Value))

  smrPaths <- SparseMatrixRecommender::SMRCreateFromLongForm(data = dfPathsLong)

  return(smrPaths)
}
