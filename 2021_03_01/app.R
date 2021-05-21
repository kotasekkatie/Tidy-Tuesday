library(shiny)
library(ggdark)
library(ggiraph)
library(tidyverse)
library(mmbtools)

mmb_load_fonts()
options(scipen = 999)

# Set up UI layout
ui <- fluidPage(
  titlePanel("Superbowl Ads for the Years 2003 & 2004"),
  #  p("This week, for Tidy Tuesday, I wanted to focus on one type of charting: Distribution charts.
  #    Therefore, I created different distribution charts for the following data sets:"),
  #  div("1. Violin chart for the data set - student_debt"),
  #  h4("Some text"),
  # Create selector field to filter by
  selectInput(
    inputId = "brand", 
    label = "Select a brand", 
    choices = c("Bud Light", "Budweiser", "NFL", "Pepsi", "Toyota"), 
    selected = "Bud Light",
    multiple = FALSE
  ), # closes selectInput

  mainPanel(
  # Display plots on the same row
  fluidRow(
    column(4, girafeOutput("scatter")),
    column(5, girafeOutput("bar"))
    ) # closes fluidRow
  ) # closes mainPanel
) # closes fluidPage

# Set up server logic
server <- shinyServer(function(input, output) {
  # Create filter
  #filter_data <- reactive({
    # filter data
  #  filter(superbowlViews, superbowlViews$brand == input$brand)
  #})
  # Generate interactive scatter plot
  output$scatter <- renderGirafe({
    girafe(ggobj = scatter,
           options = list(opts_sizing(width = 0.9),
                          # change tooltip offset when it pops up and set the 'css' option which is defined earlier
                          opts_tooltip(offx = 20, offy = 20, css = tooltip_css,
                                       # use the assigned color in the tooltip
                                       use_fill = TRUE),
                          # change size of "selected" point on mouse hover
                          opts_hover(css = "r:6px;stroke:white;stroke-width:1px;"),
                          # change opacity of "non-selected" points on mouse hover
                          opts_hover_inv(css = "opacity:0.1;"))
    )
  })
  # Generate standard bar plot
  output$bar <- renderGirafe(girafe(ggobj = bar, width_svg = 10, height_svg = 7,
                                    options = list(opts_sizing(width = 1),
                                                   # change tooltip offset when it pops up and set the 'css' option which is defined earlier
                                                   opts_tooltip(offx = 20, offy = 20, css = tooltip_css,
                                                                # use the assigned color in the tooltip
                                                                use_fill = TRUE),
                                                   # change size of "selected" point on mouse hover
                                                   opts_hover(css = "r:6px;stroke:white;stroke-width:1px;"),
                                                   # change opacity of "non-selected" points on mouse hover
                                                   opts_hover_inv(css = "opacity:0.1;")
                                    )
  ))
})

# DATA IMPORT
superbowl <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv') %>%
  select(year, brand, funny, show_product_quickly, patriotic, celebrity,
         danger, animals, use_sex, view_count, like_count, dislike_count,
         favorite_count, comment_count, title, description)

# CLEAN UP DATA SETS
superbowlFiltered <- superbowl %>%
  filter(year %in% c(2003, 2004)) %>%
  arrange(year, brand)
superbowlViews <- superbowlFiltered %>%
  filter(view_count < 400000 & !is.na(like_count)) %>%
  mutate(viewFormat = format(view_count, digits = 0,nsmall = 0, big.mark = ","),
         Views = paste("View count: ", viewFormat))

# SCATTER PLOT
scatter <- ggplot(superbowlViews, aes(x = view_count, y = like_count, color = brand)) +
  # add interactivity
  geom_point_interactive(aes(tooltip = title, data_id = title)) +
  # set colors so they show up better
  scale_color_manual(values = c("#78BE21", "#008EAA", "#FFC845", "#8D3F2B", "#97999B")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 350000)) +
  # make the whole plot dark
  dark_mode(theme_mmb_basic(plot.title.position = "plot",
                            plot.title = element_text(size = 16),
                            plot.subtitle = element_text(size = 14),
                            legend.position = "top",
                            legend.title = element_text(size = 12),
                            legend.text = element_text(size = 12),
                            axis.text.y = element_text(size = 10),
                            axis.title.y = element_text(size = 12),
                            axis.text.x = element_text(size = 10),
                            axis.title.x = element_text(size = 12),
                            panel.grid.major.y = element_line(),
                            plot.caption = element_text(face = "italic", size = 10)
  )) +
  labs(title = "Relationship between view count and like count",
       subtitle = "Mouse over to display the ad's title",
       caption = "Source: https://projects.fivethirtyeight.com/super-bowl-ads",
       x = "View Count",
       y = "Like Count",
       color = "Brand:")
# customize tooltip that displays
tooltip_css <- "font-family:calibri;font-style:italic;padding:5px;"

# BAR PLOT
bar <- superbowlViews %>%
  ggplot(aes(x = reorder(title, view_count),
             y = view_count, 
             fill = brand)) +
  geom_bar_interactive(stat = "identity", position = "dodge", aes(tooltip = Views, data_id = Views)) +
  scale_y_continuous(limits = c(0, 350000)) +
  scale_fill_manual(values = c("#78BE21", "#008EAA", "#FFC845", "#8D3F2B", "#97999B")) +
  dark_mode(theme_mmb_basic(plot.title.position = "plot",
                            legend.position = "none",
                            axis.text.y = element_text(size = 12),
                            axis.title.y = element_text(size = 12),
                            axis.text.x = element_text(size = 12),
                            axis.title.x = element_text(size = 12),
                            panel.grid.major.x = element_line(),
                            plot.caption = element_text(face = "italic", size = 10)
                           )) +
  labs(title = "Superbowl ad title by view count",
       caption = "Source: https://projects.fivethirtyeight.com/super-bowl-ads",
       x = "",
       y = "View Count") +
  coord_flip()


shinyApp(ui = ui, server = server)