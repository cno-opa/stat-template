#
# 311.R
#
# =================
#
# Calculates 311 service request stats from 311 source data
#
# =================
#

#clean
cleanSource <- function(data) {
  names(data) <- slugify(names(data))
  data$x_1 <- NULL
  data$x_2 <- NULL
  data$x_3 <- NULL
  data$x_4 <- NULL
  data$x_5 <- NULL
  data$x_6 <- NULL
  data$x_7 <- NULL
  data$x_8 <- NULL
  data$open_dt <- mdy(data$open_dt)
  data$closed_dt <- mdy(data$closed_dt)
  data$month_start <- as.factor(as.yearmon(data$open_dt))
  data$month_end <- as.factor(as.yearmon(data$closed_dt))
  data$age__calendar <- as.numeric(as.character(data$age__calendar))
  return(data)
}

# calculate summary tables
makeSummary <- function(data, filter_input) {

  countOpen <- function(data, month) {
    u <- dateFromYearMon(month)
    x <- filter(data, open_dt <= u & closed_dt > u | is.na(closed_dt))
    return(nrow(x))
  }

  countNet <- function(data, month) {
    o <- nrow(filter(data, month_start == month))
    c <- nrow(filter(data, month_end == month))
    return( o - c )
  }

  d <- filter(data, type == filter_input)
  month_range <- unique(d$month_start)

  output <- data.frame(type = filter_input, date = month_range)
  output$open <- sapply(output$date, countOpen, data = d)
  output$net <- sapply(output$date, countNet, data = d)
  output <- melt(output, id.vars = c("type", "date"))

  return(output)
}

#plot

#load and run
data <- read.csv("./data/311-source.csv", header = TRUE)
data <- cleanSource(data)
data <- getTwoYears(data, open_dt, r_period)

summary_table <- rbind(
                 makeSummary(data, "Pothole/Roadway Surface Repair"),
                 makeSummary(data, "Street Light"),
                 makeSummary(data, "Traffic Sign"),
                 makeSummary(data, "Street Name Sign"),
                 makeSummary(data, "Abandoned Vehicle Reporting/Removal"),
                 makeSummary(data, "Street Flooding/Drainage"),
                 makeSummary(data, "Tree Service Emergency"),
                 makeSummary(data, "Tree Service"),
                 makeSummary(data, "Illegal Dumping Reporting"),
                 makeSummary(data, "Residential Recycling Programs"),
                 makeSummary(data, "Mosquito Control"),
                 makeSummary(data, "Rodent Complaint")
                 )
