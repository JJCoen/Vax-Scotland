
get_mort_5_2 <- function(file_year, age_categories, yr){
    #' Extract mortality numbers from a single excel worksheet
    #' 
    #' @description This function inputs the number of persons who died
    #' in a given year and within a 5-year subgroup.  
    #' The excel sheet is "5.2"
    #'
    #' There are separate groups for newborns (recorded as "0")
    #' and for 90 years and over (recorded as "90+")
    #' 
    #' @param file_year character. The name of the excel file.
    #' Can include the path to the file.
    #' Sheet "5.2" contains deaths by age
    #' @param age_categories numeric vector. Each integer identifies 
    #' a specific 5-year age category
    #' @param yr integer. Takes values from 10 to 16 indicating years
    #' 2010 to 2016.  Necessary to deal with placement of data in differing
    #' excel cells
    #' @usage get_mort_5_2(file_name, age_vector)
    #' @return A data table with columns "age" and "mort"
    #' giving the number of deaths in each age category
    #' 

    # Check yr is within correct range
    if(!(yr %in% 10:16)) {stop()}
    
    # layout of excel file differs slightly for years 2010-2016
    if(yr == 16){
        age_range <- "D4:W6"
        col_range <- 1:20
    } else if(yr %in% 12:15){
        age_range <- "D4:V6"
        # age groups stop at "85+" and do not include "90+"
        age_categories <- age_categories[1:19]
        col_range <- 1:19
    } else if(yr %in% 10:11){
        age_range <- "D3:V5"
        age_categories <- age_categories[1:19]
        col_range <- 1:19
    }

    scot_mort <- read_excel(file_year, 
                            range = age_range) |> 
        setDT()
    
    
    # First row is blank / NAs
    scot_mort <- scot_mort[2,]
    # age category variable is spread across columns
    # mortality values are stored in cell values as numbers
    scot_mort <- scot_mort %>%    
        pivot_longer(
            cols = col_range,
            names_to = "age-cat",      
            values_to = "mort") |> 
        # names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()
    
    # change age categories for compatibility with other tables
    scot_mort[, age := age_categories][, age_cat := NULL]
    setcolorder(scot_mort, c("age", "mort"))
    return(scot_mort)
}