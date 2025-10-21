# Seasonality Analysis: Complete Step-by-Step Guide

## Overview

Seasonality refers to predictable, yearly changes driven by environmental factors such as rainfall, temperature, and humidity. In malaria, these changes matter because mosquito breeding and transmission depend heavily on rainfall. During rainy months, mosquito numbers and malaria cases rise; in dry months, breeding diminishes and cases drop.

Understanding these patterns helps programs plan **when** and **where** to act. Interventions such as **Seasonal Malaria Chemoprevention (SMC)**, **ITN distribution**, and **IRS campaigns** are most effective when scheduled before or during the high-transmission season.

This analysis determines whether a location has **predictable, consistent** malaria transmission seasons suitable for targeted interventions like Seasonal Malaria Chemoprevention (SMC). It answers three key questions: Are there seasonal peaks (do 60%+ of rainfall/cases occur in a 4-month period)? Are peaks consistent (do these peaks happen every year)? Where should we intervene (which locations qualify for seasonal interventions)?

## WHO 4-Month Rule

According to **WHO guidance**, a location is considered **seasonal** if **60% or more of total annual rainfall (or malaria cases)** occurs within **any 4 consecutive months**. This identifies likely periods of peak transmission and guides intervention timing. The 4-month window and 60% threshold come from WHO recommendations, but rainfall and transmission patterns vary by country. Before applying or adjusting this rule, consult the **SNT** to confirm whether 60% is appropriate for the local context, adjust if rainfall or malaria data show different timing patterns, and document and justify any modification to ensure national consistency.

## Rolling Window Concept

Rather than fixing specific months (e.g., always June–September), test **all possible 4-month windows** in each year. For every starting month, compute the **4-month total** (candidate rainy period) and the **12-month total** (comparison period). Calculate the share of annual rainfall captured by each 4 month window. If the percentage is **≥ 60%**, that window is a **seasonal block**.

## Classification Logic

The analysis uses a three-level classification system. At the **block level**, a 4-month period is a "seasonal block" if it captures ≥60% of the corresponding 12-month period's rainfall. At the **year level**, a year is "seasonal" if it contains at least one seasonal block among its 12 tested blocks. At the **location level**, a location is "Seasonal" if ALL analyzed years are seasonal, or "Not Seasonal" if one or more years are not seasonal. This strict approach ensures SMC is only implemented where transmission peaks occur predictably every year.

## Data Requirements

Both rainfall and malaria case data can be used for seasonality analysis. This example uses rainfall because it closely reflects environmental drivers of transmission. To ensure reliable classification, provide **at least six consecutive years** of complete monthly data and ensure **all twelve months** are present for each year. Handle missingness carefully since incomplete data can misclassify areas. The six-year minimum follows WHO analytical guidance. If the dataset is shorter (e.g., 4 years) the **SNT** should review and approve the adjustment. Before proceeding, the SNT should confirm the time span reflects consistent climatic conditions, ensure comparable data coverage across administrative areas, and document and justify any change for transparency and reproducibility.

---

## Step 1: Import packages and data

This step loads all the software tools (packages) needed for the analysis and reads your rainfall data file. The code first checks if the package manager 'pacman' is installed - this tool makes it easier to load multiple packages at once. Then it loads all required packages: readxl for reading Excel files, dplyr for data manipulation, openxlsx for writing Excel files, lubridate for handling dates, ggplot2 for creating plots, readr for reading data files, stringr for text manipulation, here for managing file paths, tidyr for data tidying, gridExtra for arranging plots, knitr for creating tables, writexl for writing Excel files, and sf for handling spatial data. Finally, it sets the path to your data file and loads the rainfall data into memory.

After running this code, you should see no error messages if packages loaded successfully. Your data will appear in the "Environment" panel in RStudio, showing the number of rows and columns in your dataset. Check that the file path is correct - you'll need to update the file_path line to match where your file is stored on your computer. If you have CSV data instead of Excel, change `read_excel()` to `read_csv()`. If you see an error saying "file does not exist", check your file path spelling and location. If you get "package not found" errors, run `install.packages("package_name")` for any missing packages. Warnings about missing values are usually okay since we handle those later in the analysis.

::: {.panel-tabset}
## R
```r
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  readxl, dplyr, openxlsx, lubridate, ggplot2, readr, stringr, 
  here, tidyr, gridExtra, knitr, writexl, sf
)

file_path <- here::here("english/data_r/modeled", "chirps_data_2015_2023_lastest.xls")
data <- readxl::read_excel(file_path)
```
## Python
:::

---

## Step 2: Configure analysis parameters

This step sets up the "rules" for your analysis by telling R which columns in your data contain which information, what time period to analyze, and what threshold defines "seasonal." The year_column variable should be set to the name of the column containing years (like 2015, 2016, etc.), month_column to the column with months (1-12), value_column to the column with rainfall amounts, and admin_columns to the geographic unit names (like district and chiefdom). The analysis_start_year sets the first year to include in the analysis, analysis_start_month defines which month to start from (usually January = 1), and seasonality_threshold sets the percentage of annual rainfall that must occur in a 4-month period to be considered seasonal (60 is the WHO standard, meaning 60% of annual rainfall concentrated in 4 months). The commented-out lines at the end are file names for saving results - you can uncomment these later if you want to automatically save your results to Excel files.

You'll need to customize the column names to match your actual data. Open your data file and check the exact column names, then update these variables accordingly. For example, if your district column is called "District_Name" instead of "FIRST_DNAM", you would change that. The analysis period settings default to starting from 2015, which works if you have data from 2015-2023. If your data starts later, like 2018, change analysis_start_year to 2018. For the seasonality threshold, keep this at 60% unless your Strategic National Team (SNT) advises otherwise - this is the WHO standard meaning that 60% or more of annual rainfall must occur within a 4-month period to be considered seasonal. A lower threshold like 50% would classify more locations as "Seasonal", while a higher threshold like 70% would classify fewer locations as "Seasonal". The output file names can be uncommented and modified to automatically save your results to Excel files with custom names. After running this code, there's no visible output since these are just settings being stored in memory for use later. Make sure your column names match your data exactly since R is case-sensitive. Verify that your start year actually exists in your dataset. The threshold should make sense for your context, though keeping it at 60 is recommended unless the SNT specifically requests a change.

::: {.panel-tabset}
## R
```r
year_column <- "Year"
month_column <- "Month"
value_column <- "mean_rain"
admin_columns <- c("FIRST_DNAM", "FIRST_CHIE")

analysis_start_year <- 2015
analysis_start_month <- 1
seasonality_threshold <- 60

# detailed_output <- "detailed_seasonality_results.xlsx"
# yearly_output <- "yearly_analysis_summary.xlsx"
# location_output <- "location_seasonality_summary.xlsx"
```
## Python
:::

---

## Step 3: Prepare data for analysis

This step validates your data structure, filters to the correct time period, and prepares administrative groupings for analysis. The code performs several checks and transformations to ensure the data is ready for seasonality calculations.

First, the code verifies that all necessary columns exist in your dataset before proceeding. It creates a list of all required columns (year, month, rainfall value, and administrative units), then checks which ones are missing from your actual data. If any required columns are missing, the code stops with an error message showing exactly which columns you need to add or rename. When successful, you won't see any output and the code will continue to the next step. If you get an error like "Missing columns: District", it means your column is named differently than what you specified in Step 2, so you need to go back and update the column names. Check spelling and capitalization exactly since R is case-sensitive.

Next, the code removes incomplete rows and ensures months are stored as numbers (1-12) for calculations. The filtering removes any rows where the year is missing, where the month is missing, or where the year is before your analysis start year. It then converts the month column to numeric format so mathematical operations will work correctly. After running this, you should see a filtered dataset appear in your environment with a row count less than or equal to your original data. Check that the row count makes sense - if you have 9 years × 12 months × 100 locations, you should have approximately 10,800 rows. If you lose more than 10% of your rows, investigate why by checking for missing data patterns in your original dataset.

Then the code combines multiple administrative levels (like District + Chiefdom) into a single identifier for grouping data. If you only have one administrative level, it simply uses that column. If you have multiple levels (like district and chiefdom), it combines them with a separator ("|") so each unique combination is treated as a separate unit. This is crucial because it ensures the analysis treats each unique geographic unit separately - without this, "Kakua Chiefdom" in different districts would be incorrectly grouped together. After running this, you'll see a new column called admin_group in your data. If you have a single level, values look like "Bo District". If you have multiple levels, values look like "Bo District | Kakua Chiefdom". This ensures that each unique geographic combination is analyzed separately.

Finally, the code confirms you have enough years of data for reliable seasonality classification. The WHO recommends at least 6 years of data to reliably detect consistent seasonal patterns. The code counts how many unique years exist in your filtered data, then checks if you have at least 6. If not, it stops with an error message. It also calculates how many complete years you can analyze - this is one less than your total years because blocks starting in the last year can't be completed (they would need data from the following year). Finally, it calculates the total number of 4-month blocks to test, which is 12 blocks per complete year. For example, if you have data from 2015-2023 (9 calendar years), you can analyze 8 complete years (2015-2022), giving you 96 total blocks to test (8 × 12 = 96). The reason we can't use all of 2023 is that a block starting in December 2023 would need data through March 2024 for the 12-month comparison, but we don't have 2024 data. After running this, you won't see output if successful, but you can check the available_years variable to see which years are in your data, and total_num_blocks to see how many time periods will be tested. If you get an error saying "Insufficient data: only 4 years found", you need more years of data - check your data source or extend the time period. If you have less than 6 years but want to proceed anyway, you must consult with the SNT first and document their approval before changing the minimum year requirement.

::: {.panel-tabset}
## R
```r
required_cols <- c(year_column, month_column, value_column, admin_columns)
missing_cols <- required_cols[!required_cols %in% colnames(data)]

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

filtered_data <- data |>
  dplyr::filter(
    !is.na(!!sym(year_column)) & 
    !is.na(!!sym(month_column)) & 
    !!sym(year_column) >= analysis_start_year
  ) |>
  dplyr::mutate(Month = as.numeric(!!sym(month_column)))

if (length(admin_columns) == 1) {
  filtered_data$admin_group <- filtered_data[[admin_columns[1]]]
} else {
  filtered_data <- filtered_data |>
    dplyr::mutate(
      admin_group = paste(!!!syms(admin_columns), sep = " | ")
    )
}

available_years <- sort(unique(filtered_data[[year_column]]))
data_span_years <- length(available_years)

if (data_span_years < 6) {
  stop(paste(
    "Insufficient data: Requires at least 6 years, but only", 
    data_span_years, "years found."
  ))
}

num_complete_years <- data_span_years - 1
total_num_blocks <- num_complete_years * 12
```
## Python
:::

---

## Step 4: Generate rolling time blocks

This step creates all possible 4-month windows across your data period. Each "block" represents a potential seasonal peak period that will be tested against the 60% threshold. The concept is that instead of assuming the rainy season always occurs in fixed months (like June-September), we test every possible 4-month period to find where the peak actually occurs. For example, if analyzing 2015-2023 data, Block 1 would be Jan-Apr 2015 (compared to the full year Jan 2015-Dec 2015), Block 2 would be Feb-May 2015 (compared to Feb 2015-Jan 2016), Block 3 would be Mar-Jun 2015 (compared to Mar 2015-Feb 2016), and this continues through Block 96 which would be Dec 2022-Mar 2023 (compared to Dec 2022-Nov 2023). Each block shifts forward by one month, creating a "rolling" window that captures peak timing regardless of when it occurs in the calendar year.

The code starts by defining month abbreviations (Jan, Feb, Mar, etc.) which makes the output human-readable - showing "Jan 2015-Apr 2015" instead of "1 2015-4 2015". Then it creates an empty dataframe to store all the block definitions, and sets the starting point using the analysis start year and month you specified earlier (typically 2015 and January). The main loop then generates each block by defining a 4-month window (the potential seasonal peak) and a corresponding 12-month window (for comparison). When a window crosses year boundaries, the code automatically adjusts the year - for example, if the 4-month window starts in December (month 12) and adds 3 months, the end month becomes March (month 3) of the following year. After defining both windows, it creates a readable date range label and stores all the information in the blocks dataframe. At the end of each iteration, the code advances to the next month, rolling over to the next year when necessary.

After running this code, you should see a dataframe called blocks with one row per time block. The first block should show your start year and month (typically "Jan 2015-Apr 2015"), and the last block should end one year before your data end year (typically "Dec 2022-Mar 2023" if your data goes through 2023). The total number of rows should equal the number of complete years times 12 - for example, if you have 8 complete years, you should see 96 blocks. You can view the blocks dataframe to verify the date ranges progress logically (Jan-Apr, Feb-May, Mar-Jun, etc.) and that year rollovers are handled correctly (Dec 2015-Mar 2016, Jan 2016-Apr 2016, etc.). This structure is crucial because each block will be tested independently to see if it captures 60% or more of annual rainfall, and the rolling approach ensures we don't miss seasonal peaks that don't align with calendar years or that vary in timing across different locations.

::: {.panel-tabset}
## R
```r
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

blocks <- data.frame()
current_year <- analysis_start_year
current_month <- analysis_start_month

for (i in 1:total_num_blocks) {
  start_4m_year <- current_year
  start_4m_month <- current_month
  end_4m_year <- current_year
  end_4m_month <- current_month + 3
  
  if (end_4m_month > 12) {
    end_4m_year <- end_4m_year + 1
    end_4m_month <- end_4m_month - 12
  }

  start_12m_year <- current_year
  start_12m_month <- current_month
  end_12m_year <- current_year
  end_12m_month <- current_month + 11
  
  if (end_12m_month > 12) {
    end_12m_year <- end_12m_year + 1
    end_12m_month <- end_12m_month - 12
  }

  date_range <- paste0(
    month_names[start_4m_month], " ", start_4m_year, "-",
    month_names[end_4m_month], " ", end_4m_year
  )
  
  blocks <- rbind(blocks, data.frame(
    block_number = i,
    start_4m_year = start_4m_year,
    start_4m_month = start_4m_month,
    end_4m_year = end_4m_year,
    end_4m_month = end_4m_month,
    start_12m_year = start_12m_year,
    start_12m_month = start_12m_month,
    end_12m_year = end_12m_year,
    end_12m_month = end_12m_month,
    date_range = date_range
  ))

  current_month <- current_month + 1
  if (current_month > 12) {
    current_month <- 1
    current_year <- current_year + 1
  }
}
```
## Python
:::

---

## Step 5: Calculate seasonality for each block

This step is where the actual seasonality testing happens. For every location and every time block, the code calculates what percentage of annual rainfall falls in that specific 4-month period. Blocks that capture 60% or more of annual rainfall are marked as "seasonal peaks." The calculation involves four key steps for each block: first, sum the rainfall in the 4-month window; second, sum the rainfall in the corresponding 12-month period; third, calculate the percentage (4-month sum divided by 12-month sum, times 100); and fourth, decide if this percentage meets or exceeds the threshold (60%). This creates a comprehensive dataset showing which time periods in which locations represent seasonal peaks.

The code begins by creating an empty dataframe to store results and getting a list of all unique locations to analyze. Then it uses a double loop structure - the outer loop goes through each location one at a time, and the inner loop tests every time block for that location. For each combination of location and block, the code first converts years and months into a single comparable number by multiplying year by 12 and adding the month (for example, March 2015 becomes 2015×12 + 3 = 24183). This makes it easy to filter data by comparing these numbers. The code then extracts all rainfall data that falls within the 4-month window by filtering for months between the start and end of that window, sums those values to get the 4-month total, and does the same for the 12-month comparison period. With both totals calculated, it computes the percentage of annual rainfall captured by the 4-month period, being careful to avoid division by zero if there's no rainfall data. The seasonal flag is set to 1 if the percentage is 60% or higher, and 0 otherwise. Finally, it builds a complete result row containing all the information - block number, date range, both totals, the percentage, the seasonal flag, and all the administrative unit identifiers - and adds this row to the growing results dataframe.

After running this code, you should see a large dataframe with one row for each location-block combination. The total number of rows equals the number of locations multiplied by the number of blocks - for example, if you have 100 locations and 96 blocks, you'll have 9,600 rows. Each row shows the block number, date range (like "Jan 2015-Apr 2015"), location identifiers (district and chiefdom), the 4-month rainfall total, the 12-month rainfall total, the percentage of annual rainfall captured, and whether it meets the seasonal threshold (1 for yes, 0 for no). When you look at the preview table, you should see Block 1 for the first location showing something like "Jan 2015-Apr 2015" with perhaps 450mm of rainfall in those 4 months out of 892mm for the full year, giving 50.44% which is below 60% so the Seasonal flag is 0. Block 2 might show "Feb 2015-May 2015" with 624mm out of 892mm for 69.89%, which exceeds 60% so the Seasonal flag is 1. You'll notice that most blocks have a Seasonal flag of 0 (not seasonal) which is normal and expected. Seasonal blocks tend to cluster together - if March-June is seasonal, then April-July likely is too because they share three months. Different locations will have their seasonal peaks at different times of year, which is exactly why we use rolling windows rather than assuming all locations peak in the same months. Check that your total row count matches expectations (locations × blocks), that the Seasonal column contains both 0s and 1s (not all the same), and that percentages range from near 0% to near 100% (not all identical, which would suggest a calculation error). If all percentages are very similar or all Seasonal flags are 0, check that you're using the correct value column for rainfall.

::: {.panel-tabset}
## R
```r
detailed_results <- data.frame()
admin_groups <- unique(filtered_data$admin_group)

for (admin_unit in admin_groups) {
  unit_data <- filtered_data |> dplyr::filter(admin_group == admin_unit)
  
  for (i in 1:nrow(blocks)) {
    block <- blocks[i, ]
    
    unit_data_ym <- unit_data[[year_column]] * 12 + unit_data$Month
    
    start_4m_ym <- block$start_4m_year * 12 + block$start_4m_month
    end_4m_ym <- block$end_4m_year * 12 + block$end_4m_month
    data_4m <- unit_data |> 
      dplyr::filter(unit_data_ym >= start_4m_ym & unit_data_ym <= end_4m_ym)
    total_4m <- sum(data_4m[[value_column]], na.rm = TRUE)

    start_12m_ym <- block$start_12m_year * 12 + block$start_12m_month
    end_12m_ym <- block$end_12m_year * 12 + block$end_12m_month
    data_12m <- unit_data |> 
      dplyr::filter(unit_data_ym >= start_12m_ym & unit_data_ym <= end_12m_ym)
    total_12m <- sum(data_12m[[value_column]], na.rm = TRUE)

    percent_seasonality <- ifelse(total_12m > 0, (total_4m / total_12m) * 100, 0)
    is_seasonal <- as.numeric(percent_seasonality >= seasonality_threshold)

    result_row <- data.frame(
      Block = i,
      DateRange = block$date_range,
      Total_4M = total_4m,
      Total_12M = total_12m,
      Percent_Seasonality = round(percent_seasonality, 2),
      Seasonal = is_seasonal,
      stringsAsFactors = FALSE
    )

    if (length(admin_columns) > 1) {
      admin_parts <- strsplit(admin_unit, " \\| ")[[1]]
      for (j in seq_along(admin_columns)) {
        result_row[[admin_columns[j]]] <- 
          ifelse(j <= length(admin_parts), admin_parts[j], NA)
      }
    } else {
      result_row[[admin_columns[1]]] <- admin_unit
    }

    detailed_results <- rbind(detailed_results, result_row)
  }
}

knitr::kable(head(detailed_results), caption = "Preview of Block-Level Results")
```
## Python
:::

---

## Step 6: Generate yearly summary

This step aggregates the detailed block-level results to answer a simpler question: "Was this location seasonal in this particular year?" A location is considered seasonal in a given year if it had at least one block during that year that met the 60% threshold. This summarization is important because it lets us see year-by-year patterns and identify any years where seasonality broke down.

The first part of the code extracts the starting year from each block's date range by splitting the text "Jan 2015-Apr 2015" into parts, taking the first part "Jan 2015", and extracting just the last four characters "2015". This creates a new column called StartYear that groups all blocks starting in the same year together, even though some blocks span across year boundaries (like Dec 2015-Mar 2016). Next, the code groups the detailed results by location and year, then counts how many blocks in that year were flagged as seasonal. For each location-year combination, it creates a summary showing the year, how many seasonal blocks were found (SeasonalCount), the total number of blocks tested that year (always 12), and a binary flag indicating whether at least one seasonal block was found (at_least_one_seasonal_block equals 1 if SeasonalCount is greater than 0, otherwise 0). The code then adds descriptive year period labels to make it clear which months belong to each year's analysis - for example, year 2015 includes blocks from "Jan 2015-Apr 2015" through "Dec 2015-Mar 2016". Finally, it arranges the results in chronological order by year and alphabetically by location for easy review.

After running this code, you'll see a yearly summary table where each row represents one location in one year. The table shows the year, location identifiers, a period label explaining which 12 blocks belong to that year, the total blocks tested (12), and whether the location was seasonal that year (1 for yes, 0 for no). For example, you might see that Bo District's Kakua Chiefdom had "at_least_one_seasonal_block" equal to 1 in years 2015, 2016, 2018, 2019, 2020, 2021, and 2022, but equal to 0 in 2017. This tells you that in most years, Kakua showed clear seasonal peaks, but in 2017 something was different - perhaps rainfall was more evenly distributed throughout the year, or there was unusual weather. The key insight from this table is identifying which locations have consistent seasonality (at_least_one_seasonal_block equals 1 every year) versus which have variable patterns (some years are 1, some are 0). Locations with even a single year of 0 will ultimately be classified as "Not Seasonal" in the final step, because SMC requires predictable, consistent timing year after year. You can optionally save this summary to an Excel file by uncommenting the writexl line. Check that the patterns make sense for your region - if a location shows seasonal behavior in most years but not one or two, this might be worth investigating further to understand what was different about those years.

::: {.panel-tabset}
## R
```r
detailed_results$StartYear <- sapply(detailed_results$DateRange, function(x) {
  parts <- strsplit(x, "-")[[1]]
  first_part <- trimws(parts[1])
  as.numeric(substr(first_part, nchar(first_part) - 3, nchar(first_part)))
})

yearly_summary <- detailed_results |>
  dplyr::group_by(dplyr::across(dplyr::all_of(admin_columns)), StartYear) |>
  dplyr::summarise(
    Year = dplyr::first(StartYear),
    SeasonalCount = sum(Seasonal, na.rm = TRUE),
    total_blocks_in_year = 12,
    at_least_one_seasonal_block = as.numeric(SeasonalCount > 0),
    .groups = 'drop'
  )

yearly_summary <- yearly_summary |>
  dplyr::mutate(
    year_period = paste0(
      "(Jan ", Year, "-Apr ", Year, ", Dec ", Year, "-Mar ", Year + 1, ")"
    )
  ) |>
  dplyr::select(
    Year, 
    dplyr::all_of(admin_columns), 
    year_period, 
    total_blocks_in_year, 
    at_least_one_seasonal_block
  ) |>
  dplyr::arrange(Year, dplyr::across(dplyr::all_of(admin_columns)))

knitr::kable(tail(yearly_summary, 10), caption = "Yearly Seasonality Summary (Last 10 rows)")
```
## Python
:::

---

## Step 7: Location-level seasonality classification

This step makes the final, most important decision for program planning: "Is this location consistently seasonal enough to qualify for SMC eligibility?" The classification rule is strict - a location must show seasonal behavior in every single analyzed year to be classified as "Seasonal." If even one year lacks a seasonal pattern, the location is classified as "Not Seasonal." This stringent approach ensures that SMC interventions, which rely on predictable timing, are only implemented in locations where the seasonal peak occurs reliably year after year.

The code first groups the yearly summary data by location and counts how many years each location showed seasonal behavior (where at_least_one_seasonal_block equals 1) and how many total years were analyzed. Then it applies the classification rule: if the number of seasonal years equals the total number of years analyzed (meaning ALL years were seasonal), the location gets classified as "Seasonal"; otherwise it gets classified as "Not Seasonal." The results are then arranged alphabetically by administrative units for easy review. For example, if Kakua Chiefdom in Bo District had seasonal patterns in 8 out of 8 analyzed years (SeasonalYears = 8, TotalYears = 8), it would be classified as "Seasonal" and would qualify for SMC. But if Tikonko Chiefdom in Bo District only had seasonal patterns in 7 out of 8 years (SeasonalYears = 7, TotalYears = 8), perhaps because 2017 showed no clear peak, it would be classified as "Not Seasonal" and would not qualify for SMC, even though it was seasonal in most years.

The reason for requiring ALL years to be seasonal (rather than just most years) is that SMC depends on precise timing. The intervention involves giving children preventive antimalarial drugs right before and during the expected transmission peak. If the peak timing varies from year to year, interventions scheduled based on one year's pattern will miss the peak in other years, reducing effectiveness and wasting resources. A location classified as "Not Seasonal" doesn't mean it has no seasonal patterns - it just means the patterns aren't consistent enough to reliably schedule time-bound interventions. These locations would benefit more from year-round strategies like continuous LLIN use promotion and strengthened case management rather than seasonal chemoprevention.

After running this code, you'll see a final classification table showing each location, how many years it was seasonal, the total years analyzed, and the final Seasonality classification. You can calculate summary statistics to understand the overall picture: use `table(location_summary$Seasonality)` to count how many locations are "Seasonal" versus "Not Seasonal", and use `prop.table(table(location_summary$Seasonality)) * 100` to see what percentage of locations fall into each category. Typically, you'd expect 40-80% of locations to be classified as "Seasonal" - if you see more than 90% classified as "Seasonal", the threshold might be too lenient and should be reviewed with the SNT; if you see fewer than 20% classified as "Seasonal", the threshold might be too strict, or your area genuinely has highly variable transmission patterns. You can optionally save this final classification to an Excel file by uncommenting the writexl line. This classification table becomes the definitive reference for determining which locations receive SMC and which need alternative intervention strategies.

::: {.panel-tabset}
## R
```r
location_summary <- yearly_summary |>
  dplyr::group_by(dplyr::across(dplyr::all_of(admin_columns))) |>
  dplyr::summarise(
    SeasonalYears = sum(at_least_one_seasonal_block, na.rm = TRUE),
    TotalYears = dplyr::n(),
    .groups = 'drop'
  )

location_summary <- location_summary |>
  dplyr::mutate(
    Seasonality = ifelse(
      SeasonalYears == TotalYears,
      "Seasonal",
      "Not Seasonal"
    )
  ) |>
  dplyr::arrange(dplyr::across(dplyr::all_of(admin_columns)))

knitr::kable(head(location_summary, 10), caption = "Location Seasonality Classification")
```
## Python
:::

---

## Step 8: Merge with spatial data for visualization

This step combines your seasonality classification results with geographic boundary data (shapefiles) so you can create maps showing which areas are seasonal and which are not. Maps are essential for program planning because they show spatial patterns, help identify geographic clusters of similar seasonality, and make it easy to communicate results to stakeholders who may not be comfortable reading tables of numbers.

The code starts by setting the path to your shapefile - this is the geographic boundary file that contains the polygon shapes for all your administrative units (districts, chiefdoms, etc.). Shapefiles actually consist of multiple files (.shp, .shx, .dbf, .prj) that must all be kept together in the same folder. The st_read function loads these files and creates a spatial dataframe. Next, it sets the path to your seasonality results file (the Excel file containing the classification you just created) and reads that data. Finally, it uses a left_join operation to merge the two datasets based on the administrative unit columns - this attaches the seasonality classification to each geographic polygon so you can color-code them on a map.

After running this code, you should see a new spatial dataframe called "merged" that contains both the geographic boundaries and the seasonality information. Check that the join was successful by looking at the merged data - each polygon should now have a Seasonality column showing either "Seasonal" or "Not Seasonal". If you see many NA values in the Seasonality column, it means some polygons didn't match between the shapefile and classification file - this usually happens because of spelling differences in location names. For example, the shapefile might say "Bo District" while your classification file says "Bo Dist" or has extra spaces. You'll need to standardize the names in one or both files before the join will work correctly. Also make sure both files use the same administrative level - if your shapefile has districts but your classification is at chiefdom level, they won't match. The join column names (specified in the by= argument) must match exactly between both datasets. If your shapefile uses different column names like "District_Name" instead of "FIRST_DNAM", you'll need to adjust the join accordingly.

::: {.panel-tabset}
## R
```r
shapefile_path <- here::here("english/data_r/shapefiles", "Chiefdom2021.shp")
spatial_data <- sf::st_read(shapefile_path)

seasonality_path <- here::here(
  "english/library/stratification/other", 
  "final_SMC_eligibility.xlsx"
)
category_data <- readxl::read_excel(seasonality_path)

merged <- spatial_data |>
  dplyr::left_join(category_data, by = c("FIRST_DNAM", "FIRST_CHIE"))
```
## Python
:::

---

## Step 9: Map years with seasonal peaks

This step creates a map showing how many years (0-8) each location showed seasonal behavior. This helps identify areas with strong versus weak seasonal patterns and reveals spatial clustering.

The code first prepares the data by converting the SeasonalYears column to text format and replacing any missing values with "0". It then defines a color palette where each number of years (0 through 8) gets a different color - gray for 0 years, and various colors for 1-8 years. The SeasonalYears values are converted to a factor (categorical variable) with levels 0 through 8 to ensure proper ordering in the legend. Next, it counts how many locations fall into each category and creates legend labels that show both the number of years and the count of locations in that category (for example, "5 years (12 locations)"). The color palette is filtered to only include colors for categories that actually exist in your data. Finally, ggplot creates the map using geom_sf to draw the polygons, colors them based on the SeasonalYears category, and adds a clean theme with legend.

When you view this map, darker colors represent locations that showed seasonal patterns in more years (stronger, more consistent seasonality), while lighter colors represent locations with seasonal patterns in fewer years (weaker, less consistent seasonality). Gray areas showed no seasonal peaks in any year analyzed. Look for spatial clustering - do neighboring locations have similar numbers of seasonal years? This might indicate regional climatic patterns. Locations showing seasonality in 7 or 8 years are very close to being classified as "Seasonal" and might warrant special review by the SNT. Areas with high variability (3-5 seasonal years) might need adaptive strategies rather than fixed-schedule SMC. The legend shows how many locations fall into each category, giving you a sense of the overall distribution. If most locations cluster at 0 or 8 years with few in between, this suggests clear geographic boundaries between seasonal and non-seasonal zones. If there's a more even distribution across all years, transmission patterns are more heterogeneous across the region.

::: {.panel-tabset}
## R
```r
merged$category <- as.character(merged$SeasonalYears)
merged$category[is.na(merged$category)] <- "0"

category_colors <- c(
  "0" = "gray80",   
  "1" = "#4E79A7",  "2" = "#F28E2B", "3" = "#E15759",
  "4" = "#76B7B2",  "5" = "#59A14F", "6" = "#EDC948",
  "7" = "#4E79A7",  "8" = "#AF7AA1"
)

merged$category <- factor(merged$category, levels = as.character(0:8))

cat_counts <- table(merged$category)
cat_counts <- cat_counts[cat_counts > 0]

legend_labels <- paste0(names(cat_counts), " (", cat_counts, ")")
names(legend_labels) <- names(cat_counts)

category_colors_filtered <- category_colors[names(category_colors) %in% names(cat_counts)]

p <- ggplot(merged) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = category_colors_filtered,
    labels = legend_labels,
    name = "Years with\nSeasonal Peak"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(title = "Number of Years with Seasonal Rainfall Patterns")

print(p)
```
## Python
:::

---

## Step 10: Map final seasonality classification

This step creates the definitive map for program planning, showing only two categories: "Seasonal" (SMC eligible) or "Not Seasonal" (not eligible). This is the map you'll use for operational decisions about where to implement SMC.

The code first handles missing values by replacing any NA values in the Seasonality column with "No data" - this accounts for locations that might exist in the shapefile but weren't included in the analysis. It defines a simple three-color scheme: blue for "Seasonal" locations (eligible for SMC), pink/red for "Not Seasonal" locations (not eligible), and white for "No data" locations. To make the map easier to read, it creates district-level boundaries by grouping all chiefdoms within each district and combining their geometries - these will be drawn as bold black lines over the chiefdom-level colors. The Seasonality values are converted to a factor with three levels to control the legend order. The code counts how many locations fall into each category and creates legend labels showing both the classification and count (for example, "Seasonal (45 locations)"). Colors are filtered to only include categories present in the data. Finally, ggplot creates a two-layer map: the first layer colors each chiefdom based on its seasonality classification, and the second layer draws district boundaries in bold black to help with geographic orientation.

This map is your primary decision-making tool. Blue areas qualify for SMC because they have predictable, consistent seasonal peaks every year - programs should focus SMC resources here with interventions timed to local peak periods. Pink/red areas don't qualify for SMC because their transmission patterns vary too much year-to-year - these areas need alternative strategies like year-round LLIN usage promotion, strengthened case management, or focal interventions based on micro-epidemiological data. White areas indicate data gaps that need attention through improved surveillance or extended data collection. The legend counts tell you the scale of SMC implementation (how many eligible locations) versus alternative strategy zones (how many ineligible locations).

When presenting this map to program managers and the SNT, be prepared to discuss borderline cases - locations that were seasonal in 7 out of 8 years might warrant special consideration or manual review. Also validate the classification against local knowledge and historical program experience. If the map shows unexpected patterns (for example, a non-seasonal chiefdom surrounded by seasonal neighbors), investigate whether this reflects real transmission differences or potential data quality issues. Document any decisions to override the algorithm-based classification with programmatic judgment, including the rationale and SNT approval. This map, combined with the underlying data tables, forms the evidence base for targeting SMC and allocating resources across your program area.

::: {.panel-tabset}
## R
```r
merged$Seasonality[is.na(merged$Seasonality)] <- "No data"

colors <- c(
  "Seasonal" = "#47B5FF",
  "Not Seasonal" = "#FFB3BA",
  "No data" = "white"
)

boundaries <- merged |>
  dplyr::group_by(FIRST_DNAM) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

merged$Seasonality <- factor(
  merged$Seasonality, 
  levels = c("Seasonal", "Not Seasonal", "No data")
)

cat_counts <- table(merged$Seasonality)
cat_counts <- cat_counts[cat_counts > 0]

legend_labels <- paste0(names(cat_counts), " (", cat_counts, ")")
names(legend_labels) <- names(cat_counts)

colors_filtered <- colors[names(colors) %in% names(cat_counts)]

p <- ggplot() +
  geom_sf(data = merged, aes(fill = Seasonality), 
          color = "gray", linewidth = 0.3) +
  geom_sf(data = boundaries, fill = NA, 
          color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = colors_filtered,
    labels = legend_labels,
    name = "Classification"
  ) +
  labs(title = "Malaria Seasonality Classification") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "black")
  )

print(p)
```
## Python
:::

---

## Common Questions and Troubleshooting

**Q: My results show ALL locations as "Not Seasonal" - what's wrong?**

This usually indicates a threshold or data issue. First, check if your threshold is too high by examining block-level results for one location and looking at the Percent_Seasonality values - if maximum values are around 50-55%, try lowering the threshold from 60% to 50%. Second, verify you're using the correct value column for rainfall - if you accidentally used a temperature or humidity column, patterns will be wrong. Third, check for data quality issues like missing months which break the calculation. Use `summary(example_location$Percent_Seasonality)` to see the distribution of percentages and diagnose the issue.

**Q: Why do some blocks show very low percentages like 5% or 10%?**

This is completely normal and expected. Blocks during the dry season will have very low rainfall in both the 4-month and 12-month periods, resulting in low percentages. Only blocks that capture the rainy season peak will show high percentages approaching or exceeding 60%. Most blocks (typically 8-10 out of 12 per year) will have low percentages.

**Q: Can I adjust the "all years must be seasonal" rule to be less strict?**

Yes, but consult SNT first and document the decision. You could modify Step 7 to allow locations that are seasonal in at least 80% of years rather than 100% of years. Change the classification logic to `Seasonality = ifelse(SeasonalYears >= (TotalYears * 0.8), "Seasonal", "Not Seasonal")`. This would classify a location as Seasonal if it showed seasonal patterns in 7 out of 8 years (87.5% of years) rather than requiring all 8. However, this increases the risk of mistiming SMC in years that don't follow the typical pattern.

**Q: My map shows unexpected patterns - is this wrong?**

Not necessarily - validate against other evidence sources. Compare results against local malaria case data from HMIS to see if case peaks align with identified seasonal periods. Review entomological data on mosquito abundance if available. Consult program staff who know local transmission patterns from years of field experience. If patterns conflict with multiple evidence sources, check data quality (are there missing months? wrong years included? incorrect rainfall values?), verify the threshold is appropriate for your setting by testing alternatives like 50% or 70%, and consider whether using case data instead of rainfall might be more accurate for locations with unusual transmission patterns.

**Q: How do I save my outputs for sharing with the SNT?**

Uncomment the save lines throughout the code. In Step 5, uncomment `writexl::write_xlsx(detailed_results, "detailed_seasonality_results.xlsx")` to save block-level results. In Step 6, uncomment the yearly summary save line. In Step 7, uncomment the location classification save line. These will create Excel files in your working directory that you can open, review, and share. For maps, add `ggsave("seasonality_map.png", plot = p, width = 10, height = 8, dpi = 300)` after creating each plot to save high-resolution images suitable for reports and presentations.

**Q: Some locations in my shapefile don't have seasonality classifications - why?**

This happens when location names don't match exactly between your shapefile and your classification results. Check spelling differences, extra spaces, special characters, or abbreviations. For example, "Bo District" versus "Bo Dist" or "Saint" versus "St.". Use `unique(spatial_data$FIRST_DNAM)` and `unique(category_data$FIRST_DNAM)` to compare the location names in both files and identify mismatches. Standardize names in one or both files before joining, being careful to preserve the original spellings in the official shapefile if that's your authoritative source.

---

## Next Steps After Analysis

After completing the analysis, schedule an SNT Review Meeting to present results. Bring both maps (years with seasonal peaks and final classification), summary statistics showing the percentage of seasonal locations, and a list of borderline cases (locations seasonal in 7 out of 8 years) for discussion. Be prepared to explain the methodology, justify the 60% threshold choice, and discuss any unexpected patterns.

Validate results with local knowledge by comparing against historical SMC implementation areas to see if new classifications align with program experience, checking rainfall patterns from national meteorological departments, and reviewing malaria case seasonality patterns from HMIS to confirm that case peaks align with identified rainfall peaks. Discrepancies between your analysis and local knowledge should be investigated and documented.

Document all decisions in a formal memo for the program record. Include the final classification criteria used (60% threshold, all years must be seasonal), any manual adjustments made to algorithm-based classifications with full justification, the rationale for adjustments based on local evidence, and SNT approval signatures authorizing use of results for operational planning. This documentation ensures transparency and provides an audit trail for future reference.

Plan interventions based on classifications. For Seasonal areas, schedule SMC rounds 4-6 weeks apart with timing aligned to local rainfall patterns, time the first round to start 2-4 weeks before the rainy season begins, and plan commodity procurement based on the number of eligible children in classified-seasonal locations. For Not Seasonal areas, strengthen year-round strategies like continuous LLIN usage promotion campaigns, enhanced case management through training and commodity supply, and focal interventions based on micro-epidemiological patterns rather than fixed seasonal schedules. Ensure both seasonal and non-seasonal areas have appropriate malaria control strategies suited to their transmission patterns.

---

## Complete Code

::: {.panel-tabset}
## R
```r
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  readxl, dplyr, openxlsx, lubridate, ggplot2, readr, stringr, 
  here, tidyr, gridExtra, knitr, writexl, sf
)

file_path <- here::here("english/data_r/modeled", "chirps_data_2015_2023_lastest.xls")
data <- readxl::read_excel(file_path)

year_column <- "Year"
month_column <- "Month"
value_column <- "mean_rain"
admin_columns <- c("FIRST_DNAM", "FIRST_CHIE")

analysis_start_year <- 2015
analysis_start_month <- 1
seasonality_threshold <- 60

required_cols <- c(year_column, month_column, value_column, admin_columns)
missing_cols <- required_cols[!required_cols %in% colnames(data)]

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

filtered_data <- data |>
  dplyr::filter(
    !is.na(!!sym(year_column)) & 
    !is.na(!!sym(month_column)) & 
    !!sym(year_column) >= analysis_start_year
  ) |>
  dplyr::mutate(Month = as.numeric(!!sym(month_column)))

if (length(admin_columns) == 1) {
  filtered_data$admin_group <- filtered_data[[admin_columns[1]]]
} else {
  filtered_data <- filtered_data |>
    dplyr::mutate(
      admin_group = paste(!!!syms(admin_columns), sep = " | ")
    )
}

available_years <- sort(unique(filtered_data[[year_column]]))
data_span_years <- length(available_years)

if (data_span_years < 6) {
  stop(paste(
    "Insufficient data: Requires at least 6 years, but only", 
    data_span_years, "years found."
  ))
}

num_complete_years <- data_span_years - 1
total_num_blocks <- num_complete_years * 12

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

blocks <- data.frame()
current_year <- analysis_start_year
current_month <- analysis_start_month

for (i in 1:total_num_blocks) {
  start_4m_year <- current_year
  start_4m_month <- current_month
  end_4m_year <- current_year
  end_4m_month <- current_month + 3
  
  if (end_4m_month > 12) {
    end_4m_year <- end_4m_year + 1
    end_4m_month <- end_4m_month - 12
  }

  start_12m_year <- current_year
  start_12m_month <- current_month
  end_12m_year <- current_year
  end_12m_month <- current_month + 11
  
  if (end_12m_month > 12) {
    end_12m_year <- end_12m_year + 1
    end_12m_month <- end_12m_month - 12
  }

  date_range <- paste0(
    month_names[start_4m_month], " ", start_4m_year, "-",
    month_names[end_4m_month], " ", end_4m_year
  )
  
  blocks <- rbind(blocks, data.frame(
    block_number = i,
    start_4m_year = start_4m_year,
    start_4m_month = start_4m_month,
    end_4m_year = end_4m_year,
    end_4m_month = end_4m_month,
    start_12m_year = start_12m_year,
    start_12m_month = start_12m_month,
    end_12m_year = end_12m_year,
    end_12m_month = end_12m_month,
    date_range = date_range
  ))

  current_month <- current_month + 1
  if (current_month > 12) {
    current_month <- 1
    current_year <- current_year + 1
  }
}

detailed_results <- data.frame()
admin_groups <- unique(filtered_data$admin_group)

for (admin_unit in admin_groups) {
  unit_data <- filtered_data |> dplyr::filter(admin_group == admin_unit)
  
  for (i in 1:nrow(blocks)) {
    block <- blocks[i, ]
    unit_data_ym <- unit_data[[year_column]] * 12 + unit_data$Month
    
    start_4m_ym <- block$start_4m_year * 12 + block$start_4m_month
    end_4m_ym <- block$end_4m_year * 12 + block$end_4m_month
    data_4m <- unit_data |> 
      dplyr::filter(unit_data_ym >= start_4m_ym & unit_data_ym <= end_4m_ym)
    total_4m <- sum(data_4m[[value_column]], na.rm = TRUE)

    start_12m_ym <- block$start_12m_year * 12 + block$start_12m_month
    end_12m_ym <- block$end_12m_year * 12 + block$end_12m_month
    data_12m <- unit_data |> 
      dplyr::filter(unit_data_ym >= start_12m_ym & unit_data_ym <= end_12m_ym)
    total_12m <- sum(data_12m[[value_column]], na.rm = TRUE)

    percent_seasonality <- ifelse(total_12m > 0, (total_4m / total_12m) * 100, 0)
    is_seasonal <- as.numeric(percent_seasonality >= seasonality_threshold)

    result_row <- data.frame(
      Block = i, DateRange = block$date_range,
      Total_4M = total_4m, Total_12M = total_12m,
      Percent_Seasonality = round(percent_seasonality, 2),
      Seasonal = is_seasonal,
      stringsAsFactors = FALSE
    )

    if (length(admin_columns) > 1) {
      admin_parts <- strsplit(admin_unit, " \\| ")[[1]]
      for (j in seq_along(admin_columns)) {
        result_row[[admin_columns[j]]] <- ifelse(j <= length(admin_parts), admin_parts[j], NA)
      }
    } else {
      result_row[[admin_columns[1]]] <- admin_unit
    }

    detailed_results <- rbind(detailed_results, result_row)
  }
}

detailed_results$StartYear <- sapply(detailed_results$DateRange, function(x) {
  parts <- strsplit(x, "-")[[1]]
  first_part <- trimws(parts[1])
  as.numeric(substr(first_part, nchar(first_part) - 3, nchar(first_part)))
})

yearly_summary <- detailed_results |>
  dplyr::group_by(dplyr::across(dplyr::all_of(admin_columns)), StartYear) |>
  dplyr::summarise(
    Year = dplyr::first(StartYear),
    SeasonalCount = sum(Seasonal, na.rm = TRUE),
    total_blocks_in_year = 12,
    at_least_one_seasonal_block = as.numeric(SeasonalCount > 0),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    year_period = paste0(
      "(Jan ", Year, "-Apr ", Year, ", Dec ", Year, "-Mar ", Year + 1, ")"
    )
  ) |>
  dplyr::select(
    Year, dplyr::all_of(admin_columns),
    year_period, total_blocks_in_year, at_least_one_seasonal_block
  ) |>
  dplyr::arrange(Year, dplyr::across(dplyr::all_of(admin_columns)))

location_summary <- yearly_summary |>
  dplyr::group_by(dplyr::across(dplyr::all_of(admin_columns))) |>
  dplyr::summarise(
    SeasonalYears = sum(at_least_one_seasonal_block, na.rm = TRUE),
    TotalYears = dplyr::n(),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    Seasonality = ifelse(SeasonalYears == TotalYears, "Seasonal", "Not Seasonal")
  ) |>
  dplyr::arrange(dplyr::across(dplyr::all_of(admin_columns)))

shapefile_path <- here::here("english/data_r/shapefiles", "Chiefdom2021.shp")
file_path_2 <- here::here("english/library/stratification/other", "final_SMC_eligibility.xlsx")

spatial_data <- sf::st_read(shapefile_path)
category_data <- readxl::read_excel(file_path_2)

merged <- spatial_data |>
  dplyr::left_join(category_data, by = c("FIRST_DNAM", "FIRST_CHIE"))

merged$category <- as.character(merged$SeasonalYears)
merged$category[is.na(merged$category)] <- "0"

category_colors <- c(
  "0" = "gray80",
  "1" = "#4E79A7", "2" = "#F28E2B", "3" = "#E15759",
  "4" = "#76B7B2", "5" = "#59A14F", "6" = "#EDC948",
  "7" = "#4E79A7", "8" = "#AF7AA1"
)

merged$category <- factor(merged$category, levels = as.character(0:8))

cat_counts <- table(merged$category)
cat_counts <- cat_counts[cat_counts > 0]

legend_labels <- paste0(names(cat_counts), " (", cat_counts, ")")
names(legend_labels) <- names(cat_counts)

category_colors_filtered <- category_colors[names(category_colors) %in% names(cat_counts)]

p <- ggplot(merged) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  scale_fill_manual(values = category_colors_filtered, labels = legend_labels, name = "Category") +
  theme_minimal() + theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(title = "Number of Years with Seasonal Rainfall Patterns")

print(p)

merged$Seasonality[base::is.na(merged$Seasonality)] <- "No data"

colors <- c(
  "Seasonal" = "#47B5FF",
  "Not Seasonal" = "#FFB3BA",
  "No data" = "white"
)

boundaries <- merged |>
  dplyr::group_by(FIRST_DNAM) |>
  dplyr::summarise(geometry = sf::st_union(geometry))

merged$Seasonality <- base::factor(
  merged$Seasonality, 
  levels = c("Seasonal", "Not Seasonal", "No data")
)

cat_counts <- base::table(merged$Seasonality)
cat_counts <- cat_counts[cat_counts > 0]

legend_labels <- base::paste0(base::names(cat_counts), " (", cat_counts, ")")
base::names(legend_labels) <- base::names(cat_counts)

colors_filtered <- colors[base::names(colors) %in% base::names(cat_counts)]

p <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = merged, ggplot2::aes(fill = Seasonality), 
                   color = "gray", linewidth = 0.3) +
  ggplot2::geom_sf(data = boundaries, fill = NA, 
                   color = "black", linewidth = 0.8) +
  ggplot2::scale_fill_manual(
    values = colors_filtered,
    labels = legend_labels,
    name = "Seasonality"
  ) +
  ggplot2::labs(title = "Malaria Seasonality Classification") +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.background = ggplot2::element_rect(fill = "white", color = "black"),
    legend.key = ggplot2::element_rect(color = "black")
  )

base::print(p)
```
## Python
:::
