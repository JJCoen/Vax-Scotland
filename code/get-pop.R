# Extract population numbers by 5-year age category 
# for given year

get_pop_year <- function(file) {
    # Table 1 contains Population by each year of age
    scot_pop <- read_excel(file_year, 
                           sheet = "Table 1",
                           skip = 2) |> 
        clean_names() |> 
        setDT()
    
    # Store total population for validation
    total <- scot_pop[1, persons_2]
    
    # Remove colums for "males", "females", and NA
    scot_pop[, c("males_3", "females_4", "x5", "males_8", "females_9",
                 "x10", "males_13", "females_14") := NULL]
    
    # remove first row since it contains total number
    scot_pop <- scot_pop[2:39, ]
    
    # filter "90 plus" entry
    over90 <- scot_pop[38, .(persons_12)][,                                             pop :=as.integer(persons_12)][,persons_12 := NULL]
    over90[, age_cat := as.factor("90 plus")]
    setcolorder(over90, c("age_cat", "pop"))
    
    # filter rows for 5-year age categories only
    scot_pop <- scot_pop[c(6, 12, 18, 24, 30, 36), ]
    # extract young ages
    scot_popyoung <- scot_pop[, .(age_1, persons_2 )]
    # extract middle ages
    scot_popmid <- scot_pop[, .(age_6, persons_7)]
    # extract old ages
    scot_popold <- scot_pop[, .(age_11, persons_12)]
    
    scot_pop <- list(scot_popyoung, scot_popmid, 
                     scot_popold, over90) |> 
        rbindlist(use.names = FALSE) 
    setnames(scot_pop, c("age_1", "persons_2"),
             c("age_cat", "pop"))
    
    # convert pop column to integer
    scot_pop[, pop := as.integer(pop)]
    # Check that total population number equals 
    # the sum of numbers in each age category:
    cat("\n total: ", total, "\n",
        "sum of pop categories: ", scot_pop[, sum(pop)] )
    
    return(scot_pop)
}