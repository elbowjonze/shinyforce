shinyServer(function(input, output, session) {
    
    ## polygons
    gpoly <- NULL
    for(x in 0:7)
    {
        for(y in 0:7)
        {
            cell <- paste0(x+1, '.', y+1)
            
            gpoly <- rbind(gpoly, c(x, y, cell))        ## bottom left
            gpoly <- rbind(gpoly, c(x+1, y, cell))      ## bottom right
            gpoly <- rbind(gpoly, c(x+1, y+1, cell))    ## top right
            gpoly <- rbind(gpoly, c(x, y+1, cell))      ## top left
        }
    }
    gpoly <- as.data.frame(gpoly)
    names(gpoly) <- c('x', 'y', 'cell')
    
    
    output$playgrid <- renderPlot({
        ggplot() +
            geom_polygon(data=gpoly, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA) +
            annotation_raster(alex, xmin=1, xmax=2, ymin=1, ymax=2) +
            annotation_raster(ivan, xmin=5, xmax=6, ymin=5, ymax=6)
    })
    
    output$cell_click <- renderPrint({
        xval <- floor(input$grid_click$x)
        yval <- floor(input$grid_click$y)
        
        paste0(xval, ',', yval)
    })
})