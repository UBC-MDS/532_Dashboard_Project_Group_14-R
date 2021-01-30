library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(cowplot)
library(dplyr)
library(ggthemes)


#test2
#df = read.csv("data/Processed/HR_employee_Attrition_editted_processed.csv")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data', 'Processed', "HR_employee_Attrition_editted_processed.csv"))

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
                                value = "Sales",
                                    #c(unique(df$Department)),
                                placeholder = 'Select a Department',
                                #multi = TRUE
                            ),
                            htmlBr(),
                            dbcLabel("Age"),
                            dccRangeSlider(
                                id = "age-widget",
                                min = 18,
                                max = 60,
                                step = 1,
                                marks = list(
                                    "18" = "18",
                                    "25" = "25",
                                    "45" = "45",
                                    "60" = "60"
                                ),
                            value = list(18, 45)
                            )
                        )),
                    dbcCol(
                        #id = 'plots',
                        md = 8,
                        list(
                            htmlBr(),
                            #dbcLabel("Monthly Income"),
                            dccGraph(id = 'plots')
                        ), style = list('max-width' = '200%', 'height' = '800px')
                        )


                ), style = list('max-width' = '300%', 'max-height' = '300%')
                )
            ), style = list('max-width' = '200%', 'max-height' = '200%')
        )
    )# Change left/right whitespace for the container

app$callback(
    output('plots', 'figure'),
    list(
        input('gender-widget', 'value'),
        input('depart-widget', 'value'),
        input('age-widget', 'value')
    ),
    function(gender='Female', depart='Sales', age=18) {
        data <- filter(df,
                       Department %in% depart
                            & Gender %in% gender
                            & Age > age[1]
                            & Age < age[2])
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
        
        chart_tra <- data %>%
            group_by(BusinessTravel, Attrition) %>%
            summarise('Proportion' = n()) %>%
            ggplot(aes(x = BusinessTravel, y = Proportion, fill = Attrition)) +
            geom_bar(position = "fill", stat = "identity") +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            labs(y = "Proportion (%)", x = '', title = 'Business Travel Frequency') +
            theme_minimal(base_size = 12) +
            theme(
                legend.position = 'none',
                plot.title = element_text(hjust = 0.5))

        chart_env <- data %>%
          group_by(EnvironmentSatisfaction, Attrition) %>%
          summarise('Proportion' = n()) %>%
          ggplot(aes(x = EnvironmentSatisfaction, y = Proportion, fill = Attrition)) +
            geom_bar(position = "fill", stat = "identity") +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            labs(y = "Proportion (%)", x = '', title = 'Environment Satisfaction') +
            theme_minimal(base_size = 12) +
            theme(
              legend.position = 'none',
              plot.title = element_text(hjust = 0.5))

        subplot(ggplotly(chart_income),
                ggplotly(chart_env),
                ggplotly(chart_tra),
                ggplotly(chart_income),
                nrows = 2,
                margin = 0.1,
                shareY = FALSE
                ) %>% layout(dragmode = 'select')
        # ggplotly(plot_sum) %>% layout(dragmode = 'select')
    }
)


app$run_server(debug = T)
#app$run_server(host = '0.0.0.0')
