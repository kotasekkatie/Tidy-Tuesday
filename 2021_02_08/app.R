library(shiny)
library(ggridges)
library(tidyverse)
library(mmbtools)
library(plyr)
mmb_load_fonts()
options(scipen = 999)

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Simple Shiny App Containing Various Plots"),
  
  # Generate a row with a sidebar
  #sidebarLayout(
  
  # Define the sidebar with one input
  #sidebarPanel(
  #  selectInput("race", "Race:", 
  #              choices=student_debt$race),
  #  hr(),
  #  helpText("Source: https://apps.urban.org/features/wealth-inequality-charts")
  #),
  
  # Create a spot for the barplot
  mainPanel(
    p("This week, for Tidy Tuesday, I wanted to focus on one type of charting: Distribution charts.
      Therefore, I created different distribution charts for the following data sets:"),
    div("1. Violin chart for the data set - student_debt"),
    div("2. Density chart for the data set - retirement"),
    div("3. Histogram chart for the data set - home_owner"),
    div("4. Boxplot chart for the data set - race_wealth"),
    div("5. Ridgeline chart for the data set - income_mean"),
    h4("Some text"),
    plotOutput("studentPlot"),
    h4("Some text"),
    plotOutput("retirementPlot"),
    h4("Some text"),
    plotOutput("homePlot"),
    h4("Some text"),
    plotOutput("avgWealthPlot"),
    h4("Some text"),
    plotOutput("medWealthPlot"),
    h4("Some text"),
    plotOutput("incomePlot")
  )

)

# DATA IMPORT
student_debt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_mean <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

# CLEAN UP DATA SETS
student_debt <- student_debt %>%
  mutate(loan_pct = round((loan_debt_pct * 100), 2)) %>%
  select(race, loan_pct)
mu <- ddply(retirement, "race", summarise, grp.mean=mean(retirement))
home_owner <- home_owner %>%
  mutate(home_pct = round((home_owner_pct * 100), 2)) %>%
  select(year, race, home_pct)
rw_avg <- race_wealth %>%
  filter(type == "Average") %>%
  select(year, race, wealth_family)
rw_med <- race_wealth %>%
  filter(type == "Median") %>%
  select(year, race, wealth_family)
income_mean <- income_mean %>%
  mutate(race = case_when(race == "All Races" ~ "All",
                          race == "Asian Alone or in Combination" ~ "Asian",
                          race == "Asian Alone" ~ "Asian",
                          race == "Black Alone or in Combination" ~ "Black",
                          race == "Black Alone" ~ "Black",
                          race == "Hispanic" ~ "Hispanic",
                          race == "White Alone" ~ "White",
                          race == "White, Not Hispanic" ~ "White (NH)")
  )
income_curr <- income_mean %>%
  filter(dollar_type == "Current Dollars" & race != "All") %>%
  select(year, race, income_quintile, income_dollars)

# PLOTS
studentPlot <- ggplot(student_debt, aes(x = race, y = loan_pct, fill = race)) + 
                 geom_violin() +
                 scale_fill_manual(values = c("#003685", "#78BE21", "#008EAA")) +
                 theme_mmb_basic(legend.position = "none",
                                 plot.title.position = "plot",
                                 axis.title.y = element_text(size = 12),
                                 panel.grid.major.y = element_line(),
                                 plot.caption = element_text(face = "italic", size = 12)
                                ) +
                 labs(title = "Distribution of student loan debt percentage by race",
                      subtitle = "Years 1989 - 2016",
                      caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                      x = "",
                      y = "Loan Debt (%)")
retirementPlot <- ggplot(retirement, aes(x = retirement, fill = race)) +
                  geom_density() +
                  geom_vline(data = mu, aes(xintercept=grp.mean), linetype="dashed") +
                  facet_grid(race ~ .) +
                  scale_fill_manual(values = c("#003685", "#78BE21", "#008EAA")) +
                  theme_mmb_basic(legend.position = "none",
                                  plot.title.position = "plot",
                                  axis.title.y = element_text(size = 12),
                                  panel.grid.major.y = element_line(),
                                  plot.caption = element_text(face = "italic", size = 12)
                                 ) +
                  labs(title = "Distribution of average retirement savings by race",
                       subtitle = "Years 1989 - 2016",
                       caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                       x = "Amount in retirement savings ($)",
                       y = "Density")
homePlot <- ggplot(home_owner, aes(x = home_pct, fill = race)) +
              geom_histogram(alpha = 0.9, color = "black") +
              facet_grid(race ~ .) +
              scale_fill_manual(values = c("#003685", "#78BE21", "#008EAA")) +
              theme_mmb_basic(legend.position = "none",
                              plot.title.position = "plot",
                              axis.title.y = element_text(size = 12),
                              panel.grid.major.y = element_line(),
                              plot.caption = element_text(face = "italic", size = 12)
                             ) +
              labs(title = "Distribution of home owners percentage by race",
                   subtitle = "Years 1976 - 2016",
                   caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                   x = "Percent home owner (%)",
                   y = "Count")
avgWealthPlot <- ggplot(rw_avg, aes(x = race, y = wealth_family, fill = race)) +
                   geom_boxplot(alpha = 0.8) +
                   scale_fill_manual(values = c("#003685", "#78BE21", "#FFC845", "#008EAA")) +
                   theme_mmb_basic(legend.position = "none",
                                   plot.title.position = "plot",
                                   axis.title.y = element_text(size = 12),
                                   panel.grid.major.y = element_line(),
                                   plot.caption = element_text(face = "italic", size = 12)
                                  ) +
                   labs(title = "Distribution of average family wealth by race",
                        subtitle = "Years 1963 - 2016",
                        caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                        x = "",
                        y = "Average wealth ($)")
medWealthPlot <- ggplot(rw_med, aes(x = race, y = wealth_family, fill = race)) +
                   geom_boxplot(alpha = 0.8) +
                   scale_fill_manual(values = c("#003685", "#78BE21", "#FFC845", "#008EAA")) +
                   theme_mmb_basic(legend.position = "none",
                                   plot.title.position = "plot",
                                   axis.title.y = element_text(size = 12),
                                   panel.grid.major.y = element_line(),
                                   plot.caption = element_text(face = "italic", size = 12)
                                  ) +
                   labs(title = "Distribution of median family wealth by race",
                        subtitle = "Years 1963 - 2016",
                        caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                        x = "",
                        y = "Median wealth ($)")
incomePlot <- ggplot(income_curr, aes(x = income_dollars, y = income_quintile, fill = race)) +
                geom_density_ridges() +
                facet_grid(race ~ .) +
                scale_fill_manual(values = c("#5D295F", "#003685", "#78BE21", "#008EAA", "#FFC845")) +
                theme_mmb_basic(legend.position = "none",
                                plot.title.position = "plot",
                                axis.title.y = element_text(size = 12),
                                panel.grid.major.y = element_line(),
                                plot.caption = element_text(face = "italic", size = 12)
                               ) +
                labs(title = "Distribution of income by race (current dollars)",
                     subtitle = "Years 1967 - 2019",
                     caption = "Source: https://apps.urban.org/features/wealth-inequality-charts",
                     x = "Income ($)",
                     y = "")
# Define server logic required to plot various variables against mpg
server <- shinyServer(function(input, output) {
  # Return the formula text for printing as a caption
  #output$caption <- reactiveText(function() {
  #  formulaText()
  #})
  # Generate student_debt plot
  output$studentPlot <- renderPlot(studentPlot,
                                   width = 600,
                                   height = 400)
  # Generate retirement plot
  output$retirementPlot <- renderPlot(retirementPlot,
                                      width = 600,
                                      height = 400)
  # Generate home_owners plot
  output$homePlot <- renderPlot(homePlot,
                                width = 600,
                                height = 400)
    
  # Generate avg_wealth plot
  output$avgWealthPlot <- renderPlot(avgWealthPlot,
                                     width = 600,
                                     height = 400)
  # Generate median_wealth plot
  output$medWealthPlot <- renderPlot(medWealthPlot,
                                     width = 600,
                                     height = 400)
  # Generate income plot
  output$incomePlot <- renderPlot(incomePlot,
                                     width = 700,
                                     height = 700)
})

shinyApp(ui = ui, server = server)