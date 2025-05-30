# write_to_municipality_info.R
library(DBI)
library(RPostgres)
library(config)
library(here)
library(dplyr)
library(cp3r)

mun_info <- cp3r::CP3_municipal_info()

# Load configuration
app_config <- config::get(
  file = here::here("config.yml"),
  config = Sys.getenv("R_CONFIG_ACTIVE", "QA")
)


# Database connection called MUNICIPALITY_INFO
MUNICIPALITY_INFO <- dbConnect(
  RPostgres::Postgres(),
  dbname = app_config$MUNdbName,
  host = app_config$MUNdbHost,
  port = app_config$MUNdbPort,
  user = app_config$dbUser,
  password = app_config$dbPassword
)

# Check if table exists and drop it if it does
if(dbExistsTable(MUNICIPALITY_INFO, "mun_info")) {
  cat("Table 'mun_info' already exists. Dropping it...\n")
  dbRemoveTable(MUNICIPALITY_INFO, "mun_info")
}

# Write the data frame to the database
cat("Writing mun_info data to MUNICIPALITY_INFO connection...\n")
dbWriteTable(
  conn = MUNICIPALITY_INFO,
  name = "mun_info",
  value = mun_info,           # Your dataframe name here
  row.names = FALSE,          # Don't include R row names
  overwrite = FALSE,          # Don't overwrite (we already dropped if needed)
  append = FALSE              # Don't append to existing data
)

# Verify the write was successful
row_count <- dbGetQuery(MUNICIPALITY_INFO, "SELECT COUNT(*) as count FROM mun_info")$count
cat("Successfully wrote", row_count, "rows to mun_info table.\n")

# Show a sample of the data
sample_data <- dbGetQuery(MUNICIPALITY_INFO, "SELECT * FROM mun_info LIMIT 5")
cat("\nSample of written data:\n")
print(sample_data)

# Show table structure
cat("\nTable structure:\n")
table_info <- dbGetQuery(MUNICIPALITY_INFO, "
  SELECT column_name, data_type, is_nullable
  FROM information_schema.columns
  WHERE table_name = 'mun_info'
  ORDER BY ordinal_position
")
print(table_info)

# Add primary key constraint on No. column
cat("\nAdding primary key constraint...\n")
dbExecute(MUNICIPALITY_INFO, 'ALTER TABLE mun_info ADD PRIMARY KEY ("No.")')

# Add indexes for commonly filtered columns
cat("Adding indexes...\n")
dbExecute(MUNICIPALITY_INFO, 'CREATE INDEX idx_mun_info_province ON mun_info ("Province")')
dbExecute(MUNICIPALITY_INFO, 'CREATE INDEX idx_mun_info_type ON mun_info ("Type")')
dbExecute(MUNICIPALITY_INFO, 'CREATE INDEX idx_mun_info_demarcation ON mun_info ("DemarcationCode")')

cat("Constraints and indexes added successfully.\n")

# Close the connection
dbDisconnect(MUNICIPALITY_INFO)
cat("Database connection closed.\n")

cat("\n✅ mun_info table has been successfully created using MUNICIPALITY_INFO connection!")
