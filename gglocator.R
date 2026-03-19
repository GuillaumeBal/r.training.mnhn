gglocator <- 
  function(p, n = Inf) {
    
    if(!test.grid){
      install.packages('grid')
    }
    library(grid)
    
    # Build plot to access scales
    gb <- ggplot_build(p)
    gt <- ggplot_gtable(gb)
    
    # Extract panel ranges (data scale)
    panel_params <- gb$layout$panel_params[[1]]
    x_range <- panel_params$x.range
    y_range <- panel_params$y.range
    
    # Draw plot
    grid.newpage()
    grid.draw(gt)
    
    coords <- data.frame(x = numeric(), y = numeric())
    
    i <- 1
    while (i <= n) {
      message("Click inside the panel (ESC to stop)")
      
      loc <- grid.locator(unit = "npc")
      if (is.null(loc)) break
      
      # Convert npc -> data scale
      x <- x_range[1] + as.numeric(loc$x) * diff(x_range)
      y <- y_range[1] + as.numeric(loc$y) * diff(y_range)
      
      coords <- rbind(coords, data.frame(x = x, y = y))
      i <- i + 1
    }
    
    return(coords)
  }

