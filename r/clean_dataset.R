# Load necessary package
library(data.table)

# Read the data from the RDS file
data <- readRDS("for_upwork.rds")

# Convert data to data.table
setDT(data)

# Step 1: Creating 'Race' variable
# Categorizing 'race' into Black, White, Latino, Asian, and Other
data[, Race := fifelse(race %chin% c("Black", "African American"), "Black",
                       fifelse(race %chin% c("White", "Caucasian"), "White",
                               fifelse(race %chin% c("Latino", "Hispanic"), "Latino",
                                       fifelse(race %chin% c("Asian", "Asian American"), "Asian", "Other"))))]

# Step 2: Identifying the school where the bachelor’s degree was obtained
# Function to find the most recent bachelor's degree
find_bachelor_school <- function(row) {
  schools <- list(school1 = list(name = row[["school1schoolname"]], degree = row[["school1degree"]], end_date = row[["school1enddate"]]),
                  school2 = list(name = row[["school2schoolname"]], degree = row[["school2degree"]], end_date = row[["school2enddate"]]),
                  school3 = list(name = row[["school3schoolname"]], degree = row[["school3degree"]], end_date = row[["school3enddate"]]))
  bachelors <- Filter(function(s) !is.na(s$degree) && grepl("bachelor", tolower(s$degree)), schools)
  if (length(bachelors) == 0) return(NA)
  recent_bachelor <- bachelors[[which.max(sapply(bachelors, function(s) s$end_date))]]
  return(recent_bachelor$name)
}

data[, SchoolBachelors := apply(.SD, 1, find_bachelor_school), .SDcols = patterns("^school[1-3](schoolname|degree|enddate)")]

# Step 3: Classifying the course of study for the bachelor's degree
# Function to classify the course of study
classify_course <- function(row) {
  course <- row[["school1field"]]
  if (is.na(course)) return("Other")
  course <- tolower(course)
  if (course %chin% c("math", "chemistry", "engineering", "computer science", "physics", "biology", "biochemistry")) {
    return("STEM")
  } else if (course %chin% c("finance", "marketing", "economics", "supply chain", "business administration")) {
    return("Business")
  } else {
    return("Other")
  }
}

data[, CourseOfStudy := apply(.SD, 1, classify_course), .SDcols = "school1field"]

# Step 4: Categorizing job titles into defined job roles
# Function to categorize job titles
categorize_job <- function(title) {
  title <- tolower(title)
  if (grepl("analyst", title)) {
    return("Business Analyst")
  } else if (grepl("marketing", title)) {
    return("Marketing")
  } else if (grepl("sales", title)) {
    return("Sales")
  } else if (grepl("finance|accounting", title)) {
    return("Finance/Accounting")
  } else if (grepl("manager|management", title)) {
    return("Management")
  } else if (grepl("healthcare|nurse|doctor|medical", title)) {
    return("Healthcare")
  } else if (grepl("student|intern", title)) {
    return("Student")
  } else {
    return("Other")
  }
}

job_cols <- patterns("^job[1-3]title")
data[, paste0("job", 1:3, "roletype") := lapply(.SD, categorize_job), .SDcols = job_cols]

# Step 5: Identifying startup joiners
# Function to classify startup joiners
classify_startup <- function(company_size, company_age) {
  company_size <- as.numeric(company_size)
  company_age <- as.numeric(company_age)
  joiner_50_5 <- !is.na(company_size) && company_size <= 50 && !is.na(company_age) && company_age <= 5
  joiner_200_5 <- !is.na(company_size) && company_size <= 200 && !is.na(company_age) && company_age <= 5
  joiner_500_10 <- !is.na(company_size) && company_size <= 500 && !is.na(company_age) && company_age <= 10
  return(list(joiner_50_5, joiner_200_5, joiner_500_10))
}

for (i in 1:3) {
  data[, paste0("Joiner_50_5_", i) := apply(.SD, 1, function(row) classify_startup(row[paste0("job", i, "companysize")], row[paste0("job", i, "companyfounded")])[[1]]), .SDcols = c(paste0("job", i, "companysize"), paste0("job", i, "companyfounded"))]
  data[, paste0("Joiner_200_5_", i) := apply(.SD, 1, function(row) classify_startup(row[paste0("job", i, "companysize")], row[paste0("job", i, "companyfounded")])[[2]]), .SDcols = c(paste0("job", i, "companysize"), paste0("job", i, "companyfounded"))]
  data[, paste0("Joiner_500_10_", i) := apply(.SD, 1, function(row) classify_startup(row[paste0("job", i, "companysize")], row[paste0("job", i, "companyfounded")])[[3]]), .SDcols = c(paste0("job", i, "companysize"), paste0("job", i, "companyfounded"))]
}

# Step 6: Identifying founders
# Function to identify side hustles, local founders, and growth founders
identify_founders <- function(row, job_title, job_startdate, job_enddate, company_description, is_full_time) {
  is_founder <- grepl("owner|founder|cofounder", tolower(job_title))
  if (!is_founder) return(list(NA, NA, NA))
  
  side_hustle <- is_founder && !is_full_time
  local_founder <- is_founder && is_full_time && grepl("restaurant|salon|cleaning|landscaping|consulting|photography|local", tolower(company_description))
  growth_founder <- is_founder && is_full_time && !grepl("restaurant|salon|cleaning|landscaping|consulting|photography|local", tolower(company_description))
  
  return(list(side_hustle, local_founder, growth_founder))
}

founders <- data.table()
for (i in 1:3) {
  data[, c(paste0("SideHustle_", i), paste0("LocalFounder_", i), paste0("GrowthFounder_", i)) := apply(.SD, 1, function(row) identify_founders(row, row[paste0("job", i, "title")], row[paste0("job", i, "startdate")], row[paste0("job", i, "enddate")], row[paste0("job", i, "companydescription")], row[paste0("job", i, "startdate")]), .SDcols = c(paste0("job", i, "title"), paste0("job", i, "startdate"), paste0("job", i, "enddate"), paste0("job", i, "companydescription"), paste0("job", i, "startdate"))]
  
  side_hustles <- data[get(paste0("SideHustle_", i)) == TRUE, .(full_name, jobTitle = get(paste0("job", i, "title")), startDate = get(paste0("job", i, "startdate")), endDate = get(paste0("job", i, "enddate")), fullTime = "N", companyStartDate = get(paste0("job", i, "companyfounded")), companySize = get(paste0("job", i, "companysize")), companyIndustry = get(paste0("job", i, "companydescription")))]
  local_founders <- data[get(paste0("LocalFounder_", i)) == TRUE, .(full_name, jobTitle = get(paste0("job", i, "title")), startDate = get(paste0("job", i, "startdate")), endDate = get(paste0("job", i, "enddate")), fullTime = "Y", companyStartDate = get(paste0("job", i, "companyfounded")), companySize = get(paste0("job", i, "companysize")), companyIndustry = get(paste0("job", i, "companydescription")))]
  growth_founders <- data[get(paste0("GrowthFounder_", i)) == TRUE, .(full_name, jobTitle = get(paste0("job", i, "title")), startDate = get(paste0("job", i, "startdate")), endDate = get(paste0("job", i, "enddate")), fullTime = "Y", companyStartDate = get(paste0("job", i, "companyfounded")), companySize = get(paste0("job", i, "companysize")), companyIndustry = get(paste0("job", i, "companydescription")))]
  
  founders <- rbind(founders, side_hustles, local_founders, growth_founders, fill = TRUE)
}

# Save the cleaned data
saveRDS(data, "cleaned_for_upwork.rds")

# Save the founders data
saveRDS(founders, "founders_for_upwork.rds")
