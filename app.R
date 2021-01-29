library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(cowplot)
library(ggthemes)

df = read.csv("data/Processed/HR_employee_Attrition_editted_processed.csv")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data/Processed', 'HR_employee_Attrition_editted_processed.csv'))

app$layout(
    dbcContainer(
        list(
            htmlH1("Key Factors for Employee Attrition Dashboard"),
            dbcRow(
                list(
                    dbcCol(
                        id = 'widgets',
                        md = 3,
                        list(
                            htmlBr(),
                            dbcLabel("Gender"),
                            dccDropdown(
                                id = 'gender-widget',
                                options =  list(list(label = "Male", value = "Male"),
                                                list(label = "Female", value = "Female")),
                                value = c('Female', "Male"),
                                placeholder = 'Select Gender',
                                multi = TRUE

                            ),
                            htmlBr(),
                            dbcLabel("Department"),
                            dccDropdown(
                                id = "depart-widget",
                                options = purrr::map(unique(df$Department), function(value) list(label = value, value = value)),
                                value = 'Sales',
                                placeholder = 'Select a Department'
                            )
                        )),
                    dbcCol(
                        #id = 'plots',
                        md = 7,
                        list(
                            htmlBr(),
                            #dbcLabel("Monthly Income"),
                            dccGraph(id = 'plots')
                        ))

                ))), style = list('max-width' = '85%', 'max-height' = '85%')
        )
    )# Change left/right whitespace for the container

app$callback(
    output('plots', 'figure'),
    list(
        input('gender-widget', 'value'),
        input('depart-widget', 'value')
    ),
    function(gender='Female', depart='Sales') {
        data <- filter(df, Department %in% depart & Gender %in% gender)
        chart_income <- ggplot(data) +
            aes(x = Attrition,
                y = MonthlyIncome,
                fill = Attrition) +
            geom_boxplot(varwidth = TRUE) +
            theme_minimal(base_size = 12) +
            labs(y = 'Monthly Income', title = 'Monthly Income Distribution') +
            scale_y_continuous(labels = scales::label_dollar()) +
            coord_flip() +
            ggthemes::scale_color_tableau() +
            theme(legend.position = 'none')

        # plot1 <- ggplotly(chart_income) %>% layout(dragmode = 'select')

        # chart_travel <- data %>%
        #     ggplot(aes(x = BusinessTravel, y = n, fill = Attrition)) +
        #     geom_bar(position = "fill", stat = "identity") +
        #     geom_col(stat = "identity", position = "fill") +
        #     labs(title = 'Business Travel Frequency')


        # plot2 <- ggplotly(chart_travel) %>% layout(dragmode = 'select')

        subplot(ggplotly(chart_income),
                ggplotly(chart_income),
                ggplotly(chart_income),
                ggplotly(chart_income),
                nrows = 2,
                margin = 0.02,
                shareY = TRUE
                ) %>% layout(dragmode = 'select')
        # ggplotly(plot_sum) %>% layout(dragmode = 'select')
    }
)


app$run_server(debug = T)
#app$run_server(host = '0.0.0.0')
