# Setup ----

setwd("D:/RStudio/UL/EC6062 project") # Set working directory

library("car") # Calculate Variance Inflation Factor
library("GGally") # Correlation matrix
library("ggplot2") # Graphs
library("ggthemes") # Theme options for graphs
library("gridExtra") # Combine multiple graphs
library("lmtest") # Calculate standard errors adjusted for heteroskedasticity
library("sandwich") # Heteroscedasticity-consistent covariance matrix estimation
library("skedastic") # Breusch-Pagan and White's tests for heteroskedasticity
library("stargazer") # Regression analysis for multiple models
library("tidyverse") # Better data processing

# Download data ----

salary_potential <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv"
    )

tuition_cost <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv"
    )

diversity_school <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv"
    )

tuition_income <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv"
    )

historical_tuition <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv"
    )

state_region <-
    readr::read_csv(
        "https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv"
    )

# Cleaning and combining data

salary_potential <- salary_potential %>%
    rename(state = state_name) %>% # Rename "state_name" to state
    mutate(# Calculate "average pay" as mean of early and mid career pay
        average_pay = (early_career_pay + mid_career_pay) / 2,
        .keep = "unused")

tuition_cost <- tuition_cost %>%
    mutate(# Calculate "average cost" of university as mean of in state and out state
        average_cost = (in_state_total + out_of_state_total) / 2,
        .keep = "unused") %>%
    select("type", "name", "state", "average_cost") # Select specific columns

diversity_school <- diversity_school %>%
    filter(category == "Total Minority") %>% # Select total number of "minority" students for each school
    mutate(# Calculate"diversity_percent" as percentage of total students who belong to a "minority" group
        diversity_percent = ceiling(enrollment * 100 / total_enrollment)) %>%
    select("name", "total_enrollment", "diversity_percent", "state") # Select all columns except "category"

state_region <- state_region %>%
    rename("state" = "State") %>%
    select(-c("State Code"))

# Combine above datasets using inner joins based on matching name and state

data <- diversity_school %>%
    inner_join(tuition_cost, by = c("name", "state")) %>%
    inner_join(salary_potential, by = c("name", "state")) %>%
    inner_join(state_region, by = "state")

# Data visualization ----

# Plot top 5 states for certain attributes (percentage of students)
g1 <- data %>%
    group_by(state) %>% # For each state
    summarize(
        # Calculate median values
        median_inspired_students = median(make_world_better_percent),
        median_stem_students = median(stem_percent),
        median_minority_students = median(diversity_percent)
    ) %>%
    gather(key = "key", value = "value", -state) %>% # Calculate for each statistic
    group_by(key) %>%
    slice_max(value, n = 5) %>% # Take top 5 values
    ggplot(aes(x = state, y = value, fill = state)) + # Create plot
    geom_col() + # Bar plot
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    theme(legend.position = "none") + # Remove legend
    facet_wrap(~ key, scales = "free") + # Wrap by measured statistic
    labs(
        # Labels
        title = "Top 5 states per statistic",
        subtitle = "Bar plot, grouped by feature, ordered alphabetically",
        x = "",
        y = "Percentage of students"
    )

# Plot top 5 states for certain attributes (cost in USD)
g2 <- data %>%
    group_by(state) %>%
    summarize(
        median_total_cost = median(average_cost),
        median_average_pay = median(average_pay)
    ) %>%
    gather(key = "key", value = "value", -state) %>%
    group_by(key) %>%
    slice_max(value, n = 5) %>%
    ggplot(aes(x = state, y = value, fill = state)) +
    geom_col() +
    theme_economist() +
    scale_color_economist() +
    theme(legend.position = "none") +
    facet_wrap(~ key, scales = "free") +
    labs(caption = "n = 519",
         x = "State (alphabetical)",
         y = "Amount (USD)")

# Combine and display above graphs
grid.arrange(g1, g2)

# Calculate and plot correlation matrix
data %>%
    select(-c("name", "state", "type", "Region", "Division")) %>% # De-select categorical variables
    ggcorr(
        # Correlation plot
        hjust = 0.75,
        label = TRUE,
        layout.exp = 2,
        high = "#014d64",
        mid = "white",
        low = "#014d64"
    ) +
    theme_void() + # Colors
    theme(
        legend.position = "top",
        plot.background = element_rect(fill = "#d5e4eb"),
        legend.key.width = unit(2, "cm")
    ) +
    labs(# Labels
        title = "Correlation matrix of features",
        subtitle = "Heat map",
        caption = "n = 519")

# Plot historical tuition cost vs post-graduation income bracket data
tut1 <- tuition_income %>%
    filter(year %% 2 == 0) %>% # Only even years, prevent crowded graph
    ggplot(aes(# Plot graph
        y = net_cost,
        x = income_lvl,
        color = income_lvl)) +
    geom_boxplot() + # Specify box plot
    facet_grid(rows = vars(year), as.table = FALSE) + # Separate plot for each year
    coord_flip() + # Flip horizontally
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    theme(legend.position = "none") + # Remove legend
    labs(
        # Labels
        title = "Total university cost vs income bracket",
        subtitle = "Box plot, grouped by year",
        caption = "n = 110,448",
        y = "Total university cost (USD)",
        x = "Post-graduation income bracket"
    )

# Plot historical tuition data for public and private schools, based on course length
tut2 <- historical_tuition %>%
    filter(type != "All Institutions",
           # Keep only public and private schools
           tuition_type %in% c("4 Year Constant", "2 Year Constant")) %>%
    ggplot(aes(# Plot
        x = year,
        y = tuition_cost,
        group = type)) +
    geom_point(aes(color = type), size = 2) + # Scatter plot for points
    geom_line(aes(color = type), size = 1) + # Line plot
    scale_y_continuous(limits = c(0, 42000)) + # Start y-axis at zero
    facet_grid(rows = vars(tuition_type)) + # Group plots by tuition type
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    labs(
        # Labels
        title = "Tuition cost by year",
        subtitle = "Line plot, grouped by course length",
        x = "Year",
        y = "Tuition cost (USD)"
    )

# Combine and display above graphs
grid.arrange(tut1, tut2)

# Plot tuition cost vs average post graduation salary

kv1 <- data %>%
    ggplot(aes(# Plot
        x = log(average_cost),
        y = log(average_pay))) +
    geom_point(aes(color = type)) + # Scatter plot for points
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 3),
                color = "#6794a7") +
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    labs(
        # Labels
        title = "Average pay vs average cost (logarithm) (USD)",
        subtitle = "Scatter plot, colored by university type, third degree polynomial estimate",
        x = "Average university cost (logarithm) (USD)",
        y = "Average post graduation salary (logarithm) (USD)"
    )

# Plot geographic region vs average post graduation salary

kv2 <- data %>%
    ggplot(aes(
        # Plot
        x = Region,
        y = log(average_pay),
        group = Region
    )) +
    geom_boxplot() + # Scatter plot for points
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    labs(
        # Labels
        title = "Average pay (logarithm) vs Geographic region",
        subtitle = "Box plot, colored by university type",
        x = "Geographic region",
        y = "Average post graduation salary (logarithm) (USD)"
    )

# Plot total enrollment vs average post graduation salary

kv3 <- data %>%
    ggplot(aes(# Plot
        x = log(total_enrollment),
        y = log(average_pay))) +
    geom_point(aes(color = type)) + # Scatter plot for points
    geom_smooth(method = "lm",
                formula = y ~ x,
                color = "#6794a7") +
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    labs(
        # Labels
        title = "Average pay (logarithm) (USD) vs student body size (logarithm)",
        subtitle = "Scatter plot, colored by university type, linear estimate",
        x = "student body size (logarithm)",
        y = "Average post graduation salary (logarithm) (USD)"
    )

# Plot rank vs average post graduation salary

kv4 <-
    data %>%
    ggplot(aes(
        # Plot
        x = rank,
        y = log(average_pay),
        group = rank
    )) +
    geom_boxplot() + # Scatter plot for points
    theme_economist() + # Set theme
    scale_color_economist() + # Set colors
    labs(
        # Labels
        title = "Average pay (logarithm) vs university ranking",
        subtitle = "Box plot, colored by university type",
        x = "University ranking",
        y = "Average post graduation salary (logarithm) (USD)"
    )

# Combine and display above graphs
grid.arrange(kv1, kv2, kv3, kv4)

# Data analysis ----

# Select data for model
model_data <-
    data %>%
    select(-c(name, state, Division)) %>% # Exclude name of school and location
    mutate_at("type", as.factor) # Convert "type" to dummy variable

# Estimate models
lm_1 <-
    lm(
        log(average_pay) ~ log(average_cost)  +
            total_enrollment +
            I(make_world_better_percent^2),
        data = model_data
    )

lm_2 <-
    lm(
        log(average_pay) ~ log(average_cost)  +
            total_enrollment +
            I(rank^2),
        data = model_data
    )

lm_3 <-
    lm(
        log(average_pay) ~ log(average_cost)  +
            total_enrollment +
            rank,
        data = model_data
    )

lm_4 <-
    lm(
        log(average_pay) ~ poly(log(average_cost), 3, raw = T) +
            log(total_enrollment) +
            poly(rank, 2),
        data = model_data
    )

lm_5 <-
    lm(
        log(average_pay) ~ poly(log(average_cost), 3, raw = T) +
            log(total_enrollment) +
            poly(rank, 2) +
            Region,
        data = model_data
    )

lm_6 <-
    lm(
        log(average_pay) ~ poly(log(average_cost), 3, raw = T) +
            log(total_enrollment) +
            poly(rank, 2) +
            Region +
            make_world_better_percent,
        data = model_data
    )

# View regression analysis for models

independant_variable_names_1 = c("average cost (logarithm)",
                                 "total enrollment",
                                 "make world better \\%",
                                 "rank\\^2",
                                 "rank",
                                 "(Intercept)")

independant_variable_names_2 = c("average cost (logarithm)",
                                 "average cost (logarithm)\\^2",
                                 "average cost (logarithm)\\^3",
                                 "total enrollment (logarithm)",
                                 "rank",
                                 "rank\\^2",
                                 "Region, Northeast",
                                 "Region, South",
                                 "Region, West",
                                 "make world better \\%",
                                 "(Intercept)")

stargazer(lm_1,lm_2,lm_3,
          type="latex",
          out="./stargazer1.tex",
          font.size="tiny",
          no.space = TRUE,
          dep.var.labels = c("average pay (logarithm)"),
          covariate.labels = independant_variable_names_1,
          column.labels = c("M1","M2","M3"))

stargazer(lm_4,lm_5,lm_6,
          type="latex",
          out="./stargazer2.tex",
          font.size="tiny",
          no.space = TRUE,
          dep.var.labels = c("average pay (logarithm)"),
          covariate.labels = independant_variable_names_2,
          column.labels = c("M4","M5","M6"))

# View model metrics
lm_5 %>% summary()

# Ramsey's RESET test to determine model misspecification
resettest(lm_5)

# Calculate Variance Inflation Factor
vif(lm_5)

# Breusch-Pagan Test for heteroskedasticity
breusch_pagan(lm_5)

# White's Test for heteroskedasticity
white_lm(lm_5)

# Linear model with standard errors adjusted for heteroskedasticity
coeftest(lm_5, vcov = vcovHC(lm_5))
