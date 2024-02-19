
get_pop_by_year <- function(file_year, sheet_yr, range_yr) {
    # Import population numbers for each year of age.  
    # file_year <- file_21
    # sheet_yr <- sheet_21
    # range_yr <- range_21
    scot_pop <- read_excel(file_year, 
                             sheet = sheet_yr,
                             range = range_yr) |> 
        setDT()
    # Store the total count recorded in source data
    # Data for 2021 has "All ages" field
    if("All ages" %in% colnames(scot_pop)) {
        total <- scot_pop[, "All ages"] |> 
            as.numeric()
        scot_pop[, "All ages":=NULL]
    } else if ("Total" %in% colnames(scot_pop)){
        # Data for 2010 has "Total" field
        total <- scot_pop[, "Total"] |> 
            as.numeric()
        scot_pop[, "Total":=NULL]
    }

    # This data has two variables:
    # "age" spread across the column names, and
    # population numbers are stored in the cell values.
    # Need to pivot_longer to create age column and 
    # to transpose age values from row to column format.
    
    scot_pop <- scot_pop %>%    
        pivot_longer(   
            cols = "0":"90+",
            names_to = "age",      
            values_to = "count",
            names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()
    verify(scot_pop, total == sum(count))
    return( list(scot_pop, total) )
    
}