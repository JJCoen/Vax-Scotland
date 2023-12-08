
get_census_22 <- function(is_plot){
    # Import census data for 2022.  
    # Records population numbers by 5-year age categories
    # Table 3 contains Population by age category
    scot_pop22 <- read_excel("./data/scotland-s-census-2022-first-results-rounded-population-estimates-data.xlsx", 
                             sheet = "Table 3",
                             range = "E4:X5") |> 
        clean_names() |> 
        setDT()
    # extract population total
    pop22_total <- scot_pop22[, all_ages]
    scot_pop22[, all_ages := NULL]
    
    # This data has two variables:
    # "age_category" spread across the column names, and
    # population numbers are stored in the cell values.
    
    # Pivot table from wide to long format
    
    scot_pop22 <- scot_pop22 %>%    
        pivot_longer(     
            cols = "aged_0_4_years":"aged_90_years",
            names_to = "age",      
            values_to = "pop",
            names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()

    # Check that pop total for 2022 equals 
    # sum of pop in age categories
    # pop22_total == scot_pop22[, sum(pop)] 
    
    # There are 19 age categories in 5 year intervals.
    # The last entry is for 90 years and older.
    # Newborns (age 0) are combined with in the [0-4] group
    
    if(is_plot){
        # Prepare pop data table for plotting 
        age_5yr_cat <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", 
                         "25 - 29", "30 - 34", "35 - 39", "40 - 44", 
                         "45 - 49", "50 - 54", "55 - 59", "60 - 64", 
                         "65 - 69", "70 - 74", "75 - 79",
                         "80 - 84", "85 - 89", "90 plus")
        scot_pop22 <- scot_pop22[, age := age_5yr_cat]
        return(scot_pop22)
    } else {
        # from census 2011, 20% of 0-4 age group are newborns
        num_newborns <- scot_pop22[age == 0, pop] * 0.2
        return(list(scot_pop22, num_newborns) )
    }
}
    