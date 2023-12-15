
get_mort_year <- function(file_year, age_categories){
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
    #' @param age_categories numeric vector. Each integer identifies 
    #' a specific 5-year age category
    #' @usage get_mort_year(file_name, age_vector)
    #' @return A data table with columns "age" and "mort"
    #' giving the number of deaths in each age category
    
    scot_mort <- read_excel(file_year, sheet = 4,
                              range = "D4:W6") |> 
        setDT()
    
    # First row is blank / NAs
    scot_mort <- scot_mort[2,]
    # age category variable is spread across columns
    # mortality values are stored in cell values as numbers
    scot_mort <- scot_mort %>%    
        pivot_longer(
            cols = 1:20,
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