library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

msleep2 <- readr::read_csv(here::here('data', 'msleep.csv'))

app$layout(
    dbcContainer(
        list(
            htmlH1('Dashr heroky deployment'),
            dccGraph(id='plot-area'),
            htmlDiv(id='output-area'),
            htmlBr(),
            htmlDiv(id='output-area2'),
            htmlBr(),
            dccDropdown(
                id='col-select',
                options = msleep2 %>% colnames %>% purrr::map(function(col) list(label = col, value = col)),
                value='bodywt')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('col-select', 'value')),
    function(xcol) {
        p <- ggplot(msleep2) +
            aes(x = !!sym(xcol),
                y = sleep_total,
                color = vore,
                text = name) +
            geom_point() +
            scale_x_log10() +
            ggthemes::scale_color_tableau()
        ggplotly(p) %>% layout(dragmode = 'select')
    }
)

app$callback(
    list(output('output-area', 'children'),
         output('output-area2', 'children')),
    list(input('plot-area', 'selectedData'),
         input('plot-area', 'hoverData')),
    function(selected_data, hover_data) {
        list(toString(selected_data), toString(hover_data))
    }
)

app$run_server(host = '0.0.0.0')
