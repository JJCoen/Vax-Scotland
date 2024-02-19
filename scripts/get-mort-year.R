
get_mort_year <- function(file_year){
    #' Extract mortality numbers from excel workbook
    #' 
    #' @description This function inputs the number of persons who died
    #' in a given year and within a 5-year subgroup.  
    #' The relevant sheet in the excel workbook is "5.02"
    #'
    #' There are separate groups for newborns (recorded as "0")
    #' and for 90 years and over (recorded as "90+")
    #' 
    #' @param file_year character. The name of the excel file.
    #' Can include the path to the file.
    #' Sheet "5.02" contains deaths by age
    #' @usage get_mort_year(file_name)
    #' @return A data table with columns "age" and "count"
    #' giving the number of deaths in each age category
    
    scot_mort <- read_excel(file_year, sheet = 4,
                              range = "C4:W6") |> 
        setDT()
    
    # First row is blank / NAs
    scot_mort <- scot_mort[2,]
    
    # Extract the total count contained in the first column
    total <- scot_mort[, 1] |> 
        as.numeric()
    # Remove first column
    scot_mort[, 1 := NULL]
    
    # age category variable is spread across columns
    # mortality values are stored in cell values as numbers
    scot_mort <- scot_mort %>%    
        pivot_longer(
            cols = 1:20,
            names_to = "age-cat",      
            values_to = "count") |> 
        # names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()

    return(list(scot_mort, total))
}