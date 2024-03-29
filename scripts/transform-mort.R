
transform_mort <- function(persons_dt){
    #' Transform to HP age categories and death counts
    #' 
    #' 
    #' @description This function converts age categories 
    #' to the 14 groups used by Humanity Projects:
    #' 0-4 5-9 10-14 15-19 20-24 25-49 50-59 60-69 70-79 80+
    #' 0-14 15-24 <20 45-64
    #' and sums the number of persons according to HP age categories
    #' 
    #' @param persons_dt data table. First column: "age_cat" 
    #' identifies the age categories in source data   
    #' In most cases, there are 20 age categories.  
    #' However, the mortality files for years 2010 - 2015 have only 19 categories.  Consequently, there is 
    #' a different "cut" onto 19 groups.
    #' Second column: "count" 
    #' gives the number of persons in each age group.
    #' @usage make_groups(persons_dt)
    #' @return A data table containing the 7 Humanity Projects age 
    #' categories and the number of persons in each group.
    
    # Record HP age categories starting with
    # 0-4 5-9 10-14 15-19 20-24
    age_tx <- c("0-4", "5-9", "10-14", "15-19", "20-24")
    # Age categories 25-49 50-59 60-69 70-79 80+
    age_tx <- c(age_tx, "25-49", "50-59", "60-69", "70-79", 
                "80+", "Total")
    # Age categories 0-14 15-24 <20 45-64
    age_tx <- c(age_tx, "0-14", "15-24", "<20", "45-64")

    # Count of persons in HP age categories starting with
    # 0-4 
    counts_tx <- persons_dt[1:2, sum(count)]
    # 5-9 10-14 15-19 20-24
    counts_tx <- c(counts_tx, persons_dt[3:6, count])
    # 25-49
    counts_tx <- c(counts_tx, persons_dt[7:11, sum(count)])
    # 50-59
    counts_tx <- c(counts_tx, persons_dt[12:13, sum(count)])
    # 60-69
    counts_tx <- c(counts_tx, persons_dt[14:15, sum(count)])
    # 70-79
    counts_tx <- c(counts_tx, persons_dt[16:17, sum(count)])
    if(nrow(persons_dt) == 19) {  # years 2010-2015
        # Age categories stop at 85+
        # 80+ 
        counts_tx <- c(counts_tx, persons_dt[18:19, sum(count)])
        # Total
        counts_tx <- c(counts_tx, persons_dt[1:19, sum(count)])
    } else if(nrow(persons_dt) == 20) {
        # age categories go up to 90+
        # 80+ 
        counts_tx <- c(counts_tx, persons_dt[18:20, sum(count)])
        # Total
        counts_tx <- c(counts_tx, persons_dt[1:20, sum(count)])
    }
    # 0-14
    counts_tx <- c(counts_tx, persons_dt[1:4, sum(count)])
    # 15-24
    counts_tx <- c(counts_tx, persons_dt[5:6, sum(count)])
    # <20
    counts_tx <- c(counts_tx, persons_dt[1:5, sum(count)])
    # 45-64
    counts_tx <- c(counts_tx, persons_dt[11:14, sum(count)])
        
    # Construct new data table for HP categories
    persons_tx <- data.table(age_cat = age_tx,
                             count = counts_tx)
    return(persons_tx)
}