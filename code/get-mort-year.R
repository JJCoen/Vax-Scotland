# Extract mortality numbers by 5-year age group 
# from source data

get_mort_year <- function(file_year){
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
    # Combine under 1 years group into 1 to 4 years 
    # to give 0-4 age category.
    zero_to_four <- scot_mort[1:2, sum(mort)]
    # remove first row
    scot_mort <- scot_mort[2:20, ]
    # replace value for 0-4 age category
    scot_mort[1, mort := zero_to_four]
    # change age categories for compatibility with other tables
    # 'age_categories' variable is in the parent environment
    scot_mort[, age_cat := age_categories]
    return(scot_mort)
}