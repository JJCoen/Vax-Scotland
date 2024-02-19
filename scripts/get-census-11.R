
get_census_11 <- function() {
    # Import census data for 2011.  
    # Records population numbers by single year of age"
    
    # File contains a single sheet
    scot_pop11 <- read_excel("./raw-data/scotland-census-2011.xlsx", 
                             sheet = "Data Sheet 0",
                             range = "C12:CZ14") |> 
        setDT()
    
    # The first row is empty
    scot_pop11 <- scot_pop11[2, ]
    # record total population
    pop11_total <- scot_pop11[, `All people`]
    # Remove entry for 'All people'
    scot_pop11[, `All people` := NULL]
    
    # Rename "Under 1" column to "0" 
    # to represent first year of life
    setnames(scot_pop11, "Under 1", "0")
    
    # This data has two variables:
    # "age" spread across the column names, and
    # population numbers are stored in the cell values.
    # Need to pivot_longer to create age column and 
    # to transpose age values from row to column format.
    
    scot_pop11 <- scot_pop11 %>%    
        pivot_longer(   
            cols = "0":"100 and over",
            names_to = "age",      
            values_to = "count",
            names_transform = readr::parse_number) |> 
        clean_names() |> 
        setDT()
    
    # sum pop numbers for 90 years and older
    over90 <- scot_pop11[91:101, sum(count)]
    # remove rows for age 90 and above
    scot_pop11 <- scot_pop11[1:90, ]
    
    # record number of newborns (age 0)
    # num_newborns <- scot_pop11[age == 0, pop]
    
    # combine rows into 5-year age categories
    # Create a grouping variable
    scot_pop11[, age_cat := cut2(age, g = 18)]
    
    # sum the pop numbers in each age category
    scot_pop11_cats <- scot_pop11[, sum(count), by = age_cat]
    
    scot_pop11_cats[, count := V1][, V1 := NULL]
    
    # append an entry for the 90 plus group
    oldest <- data.table(age_cat = "90 plus", 
                         count = over90)
    scot_pop11 <- rbind(scot_pop11_cats, oldest)
    
    # update "age_cat" for compatibility with other years
    age_5yr_cat <- c("0-4", "5-9", "10-14", "15-19", "20-24", 
                     "25-29", "30-34", "35-39", "40-44", "45-49", 
                     "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
                     "80-84", "85-89", "90 plus")
    scot_pop11[, age_cat := age_5yr_cat]
    
    # Check that pop total equals 
    # sum of pop in age categories
    verify(scot_pop11, pop11_total == scot_pop11[, sum(count)] )
    
    return(scot_pop11 )
    
}