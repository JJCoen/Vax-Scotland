plot_pop_change <- function(scot_pop11, scot_pop22){
    #' Plot the population numbers by 5-year age group 
    #' 
    #' 
    #' @description This function plots the number of persons in
    #' distinct 5-year age group from Scotland's Census 2022.
    #' It compares this with the population numbers in Census 2011.
    #' 
    #' @param scot_11 data table. A data table obtained from Census 2011.
    #' The first column, named "age", identifies the 5-year age category.
    #' The second column, "pop", gives the number of persons in each age group.
    #' @param scot_22 data table.  Population numbers from Census 2022 with
    #' columns as above  
    #' 
    #' @usage plot_pop_change(previous_year, current_year)
    #' @return No return value.  Side effect is to create plot.
    #' @note There are separate age categories for newborns ("0") and
    #' for 90 years and over ("90+")
    #'  

    # add column for 2011 population counts
    pop_11_vec <- scot_pop11[, pop]
    scot_pop11_22 <- scot_pop22[, pop_11 := pop_11_vec] 
    
    # Population sub-group wave moving to older age categories.
    
    ggplot(scot_pop11_22, aes(age, pop)) + 
        geom_col(aes(fill = pop)) +
        scale_fill_gradient(low = "white", high = "navy") +
        labs(title = "More older people in 2022 compared to 2011",
             subtitle = "Population wave moving into older age categories",
             x = "Age 5yr", y = "Population",
             caption = "Source: Scotland's Census") +
        geom_line(aes(x = age, y = pop_11,
                      group = 1), colour = "#FFCB05", linewidth = 1.2) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme(legend.position = "none") +    
        annotate(geom = "text", x = 7, y = 400000, 
                 label = "2011", colour = "#FFCB05", size = 5,) +
        coord_flip()

}
