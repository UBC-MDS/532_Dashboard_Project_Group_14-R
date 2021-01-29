library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

df = read.csv("data/Processed/HR_employee_Attrition_editted_processed.csv")

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
options(repr.plot.height = 200, repr.plot.width = 250)
p <- ggplot(df) +
  aes(x = Attrition,
      y = MonthlyIncome,
      fill = Attrition) +
  geom_boxplot(varwidth = TRUE) +
  theme_minimal(base_size = 16) +
  labs(y = 'Monthly Income', title = 'Monthly Income Distribution') +
  scale_y_continuous(labels = scales::label_dollar()) +
  coord_flip()

app$layout(dccGraph(figure = ggplotly(p)))

# app$layout(
#   dbcContainer(
#     htmlH1('Key Factors for Employee Attrition'),
#     list(
#       dccGraph(id='plot-area')
      # dccDropdown(
      #   id = 'depart-widget',
      #   options = purrr::map(df %>% colnames, function(col) list(label = col, value = col)),
      #   value='Sales')
    #   )
    #
    # ))
    # dbcRow(
    #   list(
    #     dbcCol(
    #       list(
    #         dccGraph(id='plot-area'),
    #         htmlLabel('Department'),
    #         dccDropdown(
    #           id = 'depart-widget',
    #           value = 'Sales'
    #           options = list(list(label = col, value = col),
    #                          list(label = "San Francisco", value = "SF")),
    #         )
    #         )
    #       ),
    #     )))))


# app$callback(
#   output('plot-area', 'figure'),
#   list(input('depart-widget', 'value')),
#
#   function(xcol) {
#     options(repr.plot.height = 200, repr.plot.width = 250)
#     p <- ggplot(df %>% filter(Department = !!sym(xcol))) +
#       aes(x = MonthlyIncome,
#           y = Attrition,
#           fill = Attrition) +
#       geom_boxplot(varwidth = TRUE) +
#       # theme_minimal(base_siz=16) +
#       labs(x = 'Monthly Income', title = 'Monthly Income Distribution') +
#       scale_x_continuous(labels = scales::label_dollar())
#     ggplotly(p)
#   }
# )

app$run_server(debug = T)


