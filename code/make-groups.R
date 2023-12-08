
make_groups <- function(persons_dt, is_pop, cat_19){
    #' Section the "age" variable into the 7 groups 
    #' 
    #' 
    #' @description This function combines the number of persons 
    #' into the 7 groupings used by Humanity Project
    #' 
    #' @param persons_dt data table. A data table where one column 
    #' identifies the 5-year age category and named "age".  In most cases,
    #' there are 20 age categories.  However, the mortality files for 
    #' years 2010 - 2015 have only 19 categories.  Consequently, there is 
    #' a different "cut" onto groups.
    #' The second column gives the number of persons in each age group.
    #' Can have either the name "pop" for population counts or
    #' "mort" for mortality numbers  
    #' @param is_pop integer Value = 1 indicates that the second column
    #' contains population numbers.  
    #' Value = 0 indicates mortality numbers.
    #' @param cat_19 logical.  When TRUE, indicates that the mortality data
    #' is for years 2010-2015.
    #' @usage make_groups(persons_dt, is_pop)
    #' @return A data table containing the 7 Humanity Projects age 
    #' categories and the number of persons in each group.
    
    if(is_pop) {
        # sum population values by age_cat
        persons_dt[, age_cat := cut2(age, 
                                     c(1, 15, 45, 65, 75, 85))]
        persons_dt[, pop := sum(pop), by=age_cat][, age := NULL]
    } else if (is_pop == 0) {
        if(cat_19) {
            persons_dt[, age_cat := cut2(age, 
                                         c(1, 15, 45, 65, 75, 85, 90))]
        } else {
            persons_dt[, age_cat := cut2(age, 
                                         c(1, 15, 45, 65, 75, 85))]
        }
        
        persons_dt[, mort := sum(mort), by = age_cat][, age := NULL]
    }
    
    persons_dt <- unique(persons_dt)
    
    # HP age categories
    age_categories <- c( "0", "1-14", "15-44", "45-64", "65-74",
                         "75-84", "85+")
    persons_dt <- persons_dt[, age_cat := age_categories]
    return(persons_dt)
}