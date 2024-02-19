# Extract population numbers for each year of age 
# the final entry is for the "90 & over" group

get_pop <- function(file_year, is_16) {
    # Table 1 contains Population by each year of age
    scot_pop <- read_excel(file_year, 
                           sheet = "Table 1",
                           skip = 2,
                           n_max = 39) |> 
        clean_names() |> 
        setDT()
    
    # # Store pop number of newborns
    # num_newborns <- read_excel(file_year, 
    #                            sheet = "Table 1",
    #                            range = "B5", 
    #                            col_names = FALSE)[[1]]
    
    # Store total population for validation
    total <- scot_pop[1, persons_2] |> 
        as.numeric()
    
    # Remove columns for "males", "females", and NA
    scot_pop[, c("males_3", "females_4", "x5", "males_8", "females_9",
                 "x10", "males_13", "females_14") := NULL]
    
    if(is_16) {
        # remove first row since it contains total number
        scot_pop <- scot_pop[2:38, ]
        # create a data table with a single obs. for "90 & over"
        over90 <- scot_pop[37, .(persons_12) ]        
    } else {
        # remove first row since it contains total number
        scot_pop <- scot_pop[2:39, ]
        # create a data table with a single obs. for "90 & over"
        over90 <- scot_pop[38, .(persons_12) ]
    }
    # Create an entry for the 90+ group
    over90[, count :=as.integer(persons_12)][, persons_12 := NULL]
    over90[, age_cat := "90 plus"]
    setcolorder(over90, c("age_cat", "count"))
    
    # filter rows with counts for each 5-year age category
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
             c("age_cat", "count"))
    
    # # age = "0-4" represents 0-4 year old group
    # # separate out a new group for newborns
    # # age = "0": newborns
    # # age = "1-4" for 1 <= age <= 4
    # # Create new entry for age = 0
    # pop0 <- data.table(age_cat = "0", count = num_newborns)
    # # Substract number of newborns from the 0 category
    # # and convert this to the "1-4" category
    # scot_pop[age_cat == "0-4", `:=` (count = count - num_newborns,
    #                           age_cat = "1-4")]
    # scot_pop <- rbind(pop0, scot_pop)
    
    # Check that total population number equals 
    # the sum of numbers in each age category:
    scot_pop |> 
        verify(total == scot_pop[, sum(count)])
    
    return(list(scot_pop, total))
}