
get_pop_by_year <- function(file_year, sheet_yr, range_yr) {
    # Import population numbers for each year of age.  
    # file_year <- file_21
    # sheet_yr <- sheet_21
    # range_yr <- range_21
    scot_pop <- read_excel(file_year, 
                             sheet = sheet_yr,
                             range = range_yr) |> 
        setDT()
    
    # This data has two variables:
    # "age" spread across the column names, and
    # population numbers are stored in the cell values.
    # Need to pivot_longer to create age column and 
    # to transpose age values from row to column format.
    
    scot_pop <- scot_pop %>%    
        pivot_longer(   
            cols = "0":"90+",
            names_to = "age",      
            values_to = "pop",
            names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()

    return(scot_pop )
    
}