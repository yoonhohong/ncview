library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)


# function; spaghetti plot for fu 
spg_plot_param = . %>%
  plot_ly(x = ~Date, y = ~value, color = ~param,
          legendgroup = ~param,
          colors = "Dark2") %>%
  add_lines(name = ~param, showlegend = F) %>%
  add_markers(showlegend = F) %>%
  add_annotations(
    text = ~unique(side.nerve),
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "middle",
    yanchor = "top",
    showarrow = FALSE,
    font = list(size = 15)
  ) %>%
  layout(
    xaxis = list(
      showgrid = T, 
      tickangle = 45, 
      tickformat = "%Y-%m"
    ),
    yaxis = list(
      showgrid = T
    ), 
    margin = list(t = 70, b=70, l=50, r=50))

# Define server logic to read selected file ----
server <- function(input, output) {
  
  input_file = reactive({
    # input$file1 will be NULL initially.
    req(input$ncsFile)
    df <- read.csv(input$ncsFile$datapath)
    df$Date = as.Date(df$Date, format = "%Y-%m-%d")
    return(df)
  }) # input_file 
  
  output$ptTable <- renderDT({
    input_file() %>%
      select(Hosp, ID, Name, Date)
  }, rownames = F, selection = "single", options = list(
    pageLength = 10))
  
  df_selected = reactive({
    if (length(input$ptTable_rows_selected) == 0) return(NULL)
    df_selected = input_file()[input$ptTable_rows_selected,]
    df_selected = df_selected %>%
      gather(key = "side.nerve.param",
             value = "value", 
             c(7:118)) %>%
      separate(side.nerve.param, 
               into = c("side", "nerve", "param"), 
               sep = "\\.") %>%
      mutate(side.nerve = paste(side, nerve, sep=".")) %>%
      mutate(value = as.integer(value))
    return(df_selected)
  })
  
  df_motor = reactive({
    if (is.null(df_selected())) return(NULL)
    df_motor = df_selected() %>%
      filter(side.nerve %in% c("R.MM","L.MM","R.UM","L.UM","R.PM","L.PM","R.TM","L.TM")) %>%
      select(side.nerve, param, value)
    df_motor_A = df_motor %>%
      filter(param %in% c("DML", "Dur1", "Dur2", "Dur3", "Dur4", "FL")) %>%
      mutate(cutoff = ifelse(value > 100, "Above ULN", "WNL")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, cutoff))
    df_motor_B = df_motor %>%
      filter(param %in% c("CMAP1","CMAP2","CMAP3","CMAP4","NCV1","NCV2","NCV3")) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, cutoff))
    df_motor_all = rbind(df_motor_A, df_motor_B)
    df_motor_all = df_motor_all %>%
      mutate(side.nerve = factor(side.nerve, levels = c("R.MM","L.MM","R.UM","L.UM","R.PM","L.PM","R.TM","L.TM"))) %>%
      mutate(param = factor(param, levels = c("CMAP1","CMAP2","CMAP3","CMAP4","DML","Dur1","Dur2","Dur3","Dur4","NCV1","NCV2","NCV3","FL"))) %>%
      mutate(cutoff = factor(cutoff))
    return(df_motor_all)
  })
  
  output$tileView_motor = renderPlot({
    if (is.null(df_motor())) return(NULL) 
    p <- ggplot(df_motor(), aes(x=side.nerve, y=param, 
                                    fill = factor(cutoff))) + 
      geom_tile(color = "black") + 
      geom_text(aes(label = value), size = 8) + theme_minimal() + 
      theme(axis.text.x = element_text(size = 16, face = "bold"), 
            axis.text.y = element_text(size = 16, face = "bold"), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            plot.title = element_text(size = 24, face = "bold", hjust = -0.25),
            panel.grid = element_blank(),
            legend.text = element_text(size = 16, face = "bold"),
            plot.margin = margin(t=30, l=30)) + 
      scale_fill_manual(values = c("red","blue","grey"), 
                        name = "") +
      ggtitle("Motor nerves")
    return(p)
  })
  
  df_sensory = reactive({
    if (is.null(df_selected())) return(NULL)
    df_sensory = df_selected() %>%
      filter(side.nerve %in% c("R.MS","L.MS","R.US","L.US","R.SS","L.SS")) %>%
      mutate(side.nerve = factor(side.nerve, levels = c("R.MS","L.MS","R.US","L.US","R.SS","L.SS"))) %>%
      filter(param %in% c("SNAP1","SNAP2","SNAP3","SNAP4","NCV1","NCV2","NCV3","NCV4")) %>%
      mutate(param = factor(param, levels = c("SNAP1","SNAP2","SNAP3","SNAP4","NCV1","NCV2","NCV3","NCV4"))) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL")) %>%
      mutate(cutoff = ifelse(is.na(value), NA, cutoff)) %>%
      select(side.nerve, param, value, cutoff)
    df_sensory$cutoff = factor(df_sensory$cutoff)
    return(df_sensory)
  })
  
  output$tileView_sensory = renderPlot({
    if (is.null(df_sensory())) return(NULL)
    p <- ggplot(df_sensory(), aes(x=side.nerve, y=param,
                                  fill = cutoff)) +
      geom_tile(color = "black") +
      geom_text(aes(label = value), size = 8) + theme_minimal() +
      theme(axis.text.x = element_text(size = 16, face = "bold"),
            axis.text.y = element_text(size = 16, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 24, face = "bold", hjust = -0.6),
            panel.grid = element_blank(),
            legend.text = element_text(size = 16, face = "bold"),
            plot.margin = margin(t=30, b=80, l=30)) +
      scale_fill_manual(values = c("Below LLN" = "red",
                                   "WNL" = "green"),
                        name = "") + 
      ggtitle("Sensory nerves")
    return(p)
  })
  
  df_motor_radial = reactive({
    if (is.null(df_motor())) return(NULL) 
    motor_radial <- df_motor() 
    motor_radial <- motor_radial[complete.cases(motor_radial),]
    motor_radial <- motor_radial %>%
      filter(param %in% c("CMAP1", "CMAP2",  
                          "DML", "Dur1", "Dur2",
                          "NCV1", "FL")) %>%
      mutate(param = factor(param)) %>%
      mutate(side.nerve = factor(side.nerve)) 
    return(motor_radial)
  })
  
  output$paramView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL) 
    p <- df_motor_radial() %>%
      group_by(side.nerve) %>%
      arrange(param) %>%
      plot_ly(type = 'scatterpolar', mode = "lines+markers") %>%
      add_trace(r = ~value, 
                theta = ~param, 
                name = ~side.nerve,
                fill = "toself") %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(-50, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        margin = list(t = 70, b=70),
        legend = list(font = list(size = 20), x = 100, y = 0.5), 
        title = list(text = "Parameter view", x = 0, y = 1, font = list(size = 22, face = "bold"))
      )
    return(p)
  })
  
  output$nerveView = renderPlotly({
    if (is.null(df_motor_radial())) return(NULL) 
    p <- df_motor_radial() %>%
      group_by(param) %>%
      arrange(side.nerve) %>%
      plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(r = ~value, 
                theta = ~side.nerve, 
                name = ~param, 
                fill = "toself") %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(-50, (round(
              max(df_motor_radial()$value, na.rm = T)/50)+1)*50)
          ), 
          angularaxis = list(
            tickfont = list(size = 20)
          )
        ),
        margin = list(t = 70, b=70),
        legend = list(font = list(size = 20), x = 100, y = 0.5), 
        title = list(text = "Nerve view", x = 0, y = 1, font = list(size = 22, face = "bold"))
      )
    return(p)
  })
  
  # fu view 
  fu_selected = reactive({
    if (is.null(df_selected())) return(NULL)
    fu_selected = input_file()[input_file()$ID == df_selected()$ID, ]
    fu_selected = fu_selected %>%
      gather(key = "side.nerve.param",
             value = "value", 
             c(7:118)) %>%
      separate(side.nerve.param, 
               into = c("side", "nerve", "param"), 
               sep = "\\.") %>%
      mutate(side.nerve = paste(side, nerve, sep=".")) %>%
      mutate(value = as.integer(value)) %>%
      filter(side.nerve %in% c("R.MM","L.MM","R.UM","L.UM",
                               "R.PM","L.PM","R.TM","L.TM")) %>%
      mutate(side.nerve = factor(side.nerve, 
                                 levels = 
                                   c("R.MM","L.MM","R.UM","L.UM",
                                     "R.PM","L.PM","R.TM","L.TM"))) %>%
      filter(param %in% c("CMAP1", "CMAP2", 
                          "DML", "Dur1", "Dur2", 
                          "NCV1", "FL")) %>%
      mutate(param = factor(param, 
                            levels = c("CMAP1", "CMAP2",  
                                       "DML", "Dur1", "Dur2", 
                                       "NCV1", "FL"))) %>%
      select(Date, side.nerve, param, value)  %>%
      group_by(side.nerve) %>%
      mutate(all_na = all(is.na(value))) %>%
      filter(all_na == F)
    return(fu_selected)
  })
  
  df_date = reactive({
    if (is.null(fu_selected())) return(NULL) 
    df_date = data.frame(date = sort(unique(fu_selected()$Date)))
    return(df_date)
  })
  
  output$dateTable <- renderDT({
    if (is.null(df_date())) return(NULL) 
    df_date()
  }, rownames = T, selection = "multiple", options = list(
    pageLength = 5))  
  
  output$fuView = renderPlotly({
    if (length(input$dateTable_rows_selected) == 0) return(NULL)
    temp_date = data.frame(date = df_date()[input$dateTable_rows_selected,])
    fu_selected_sub = fu_selected() %>%
      filter(Date %in% ymd(temp_date$date)) %>%
      arrange(sort(Date, decreasing = F)) %>%
      mutate(Date = ymd(Date))
    p = fu_selected_sub %>%
      group_by(side.nerve) %>%
      do(p = spg_plot_param(.)) %>%
      subplot(nrows = 2, 
              shareY = T, shareX = T, 
              titleX = F, titleY = T) 
    p1 = style(p, traces = 1:7, showlegend = T)
    layout(p1, legend = list(font = list(size = 15)))
  })
}

