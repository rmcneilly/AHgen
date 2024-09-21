vis_AH_layout <- function(edgelist, vInfo, key, spacing) {
  
  require(ggraph)
  
  vInfo <- vInfo %>% arrange(level, Node)
  edgelist <- edgelist %>% arrange(layer)
  
  
  # DUMMY VERTEX ------------------------------------------------------------
  VINFO <- vInfo %>% filter(level == min(level))
  
  EDGELIST <- edgelist %>% add_row(.before = 1, layer = "dummyLayer", from = "dummyVertex", to = VINFO$Node)
  
  VINFO <-
    vInfo %>%
    mutate(level = level + 1) %>%
    add_row(.before = 1, level = 1, levelName = "dummyLevel", Node = "dummyVertex")
  
  IGRAPH <- EDGELIST %>% select(from, to, weight) %>% graph_from_data_frame(directed = FALSE)
  
  
  # SUGIYAMA LAYOUT TEMPLATE ------------------------------------------------
  LAYOUT <-
    (IGRAPH %>%
       layout_with_sugiyama(layers = VINFO$level))$layout %>%
    as.data.frame %>%
    setNames(c("x", "y")) %>%
    slice(-1) %>%
    cbind(vInfo, .) %>%
    as_tibble() %>%
    group_by(level, levelName) %>%
    mutate(x = scales::rescale(x, to = c(-1,1)),
           y = -level,
           pos = 1:n()) %>%
    ungroup %>%
    arrange(level, x, y) %>%
    split(.$level)
  
  
  # VERTEX SPACING ----------------------------------------------------------
  LAYOUT <- lapply(LAYOUT, function(i) {
    
    if(nrow(i) == 1) { dt <- i } else {
      
      dt <- i
      
      for(j in 1:(nrow(dt)-1)) {
        
        diff = abs(dt$x[[j]] - dt$x[[j + 1]])
        
        if(diff < spacing[[1]]) { dt$x[-(1:j)] <- dt$x[-(1:j)] + (spacing[[1]] - diff) }
        if(diff > spacing[[2]]) { dt$x[-(1:j)] <- dt$x[-(1:j)] - (diff - spacing[[2]]) }
        
      }
      
    }
    
    return(dt)
    
  })
  
  
  # RADIAL LAYOUT -----------------------------------------------------------
  makeRadial <- function(dataInput, dataKey) {
    
    midPoint <- 270
    
    # Convert angles to radian
    min <- midPoint - dataKey$angle
    min <- min * pi / 180
    max <- dataKey$angle + midPoint
    max <- max * pi / 180
    
    # S = R0, arc length is equal to radius multiplied by theta
    minS <- min * dataInput$y[[1]]
    maxS <- max * dataInput$y[[1]]
    
    # (Optional) Adjustments to radius
    R <- dataKey$addR + dataInput$level[[1]]
    
    # Convert to polar coordinates
    dataInput %>%
      mutate(x = scales::rescale(x, to = c(minS, maxS))) %>%
      mutate(thetaRad2 = x/y,
             x2 = R * cos(thetaRad2),
             y2 = R * sin(thetaRad2))
    
  }
  
  LAYOUT_GG <- lapply(1:length(LAYOUT), function(x) { makeRadial(LAYOUT[[x]], key[x,]) }) %>% bind_rows()
  
  LAYOUT_GG <-
    LAYOUT_GG %>%
    select(-x, -y) %>%
    rename(x = x2, y = y2, theta = thetaRad2) %>%
    create_layout(edgelist, layout = .)
  
  # EXTRACT -----------------------------------------------------------------
  findEdges <- get_edges("short", collapse = "none")
  
  edges <- findEdges(LAYOUT_GG) %>%
    as_tibble()
  
  edges <- findEdges(LAYOUT_GG) %>%
    as_tibble() %>%
    select(fromLevel = node1.level, toLevel = node2.level,
           from = node1.name, to = node2.name,
           x = node1.x, y = node1.y, xend = node2.x, yend = node2.y) %>%
    left_join(edgelist, by = c("from", "to")) %>%
    mutate(layer = ifelse(is.na(layer), "dummyLayer", layer)) %>%
    mutate(layer = factor(layer, levels = edgelist %>% pull(layer) %>% unique()))
  
  
  findNodes <- get_nodes()
  
  vertices <-
    findNodes(LAYOUT_GG) %>%
    as_tibble %>%
    select(level, levelName, Node = name, x, y, theta) 
  
  
  # OUTPUT ------------------------------------------------------------------
  vertices$Node <- gsub("'", '', vertices$Node)
  edges$to <- gsub("'", '', edges$to)
  
  output <- list(edges = edges, vertices = vertices)
  return(output)
  
}
