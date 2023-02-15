library(plotly)
library(DT)
library(tidyverse)

source("helpers.R")

server <- function(input, output) {
  
  input_file = reactive({
    if (is.null(input$ncsFile)) {
      df = readRDS(input$demo)
    } else {
      df <- read.csv(input$ncsFile$datapath)
    }
    df$Date = as.Date(df$Date, format = "%Y-%m-%d")
    return(df)
  }) # input_file 
  
  output$ptTable <- renderDT({
    input_file() %>%
      select(ID, Date)}, 
    rownames = F, selection = "single", 
  options = list(pageLength = 10))
  
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
      mutate(cutoff = ifelse(is.na(value), "N/C or N/A", cutoff))
    df_motor_B = df_motor %>%
      filter(param %in% c("CMAP1","CMAP2","CMAP3","CMAP4","NCV1","NCV2","NCV3")) %>%
      mutate(cutoff = ifelse(value < 100, "Below LLN", "WNL")) %>%
      mutate(cutoff = ifelse(is.na(value), "N/C or N/A", cutoff))
    df_motor_all = rbind(df_motor_A, df_motor_B)
    df_motor_all = df_motor_all %>%
      mutate(side.nerve = factor(side.nerve, levels = c("R.MM","L.MM","R.UM","L.UM","R.PM","L.PM","R.TM","L.TM"))) %>%
      mutate(param = factor(param, levels = c("CMAP1","CMAP2","CMAP3","CMAP4","DML","Dur1","Dur2","Dur3","Dur4","NCV1","NCV2","NCV3","FL"))) %>%
      mutate(cutoff = factor(cutoff, levels = c("WNL","Above ULN", "Below LLN", "N/C or N/A")))
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
            plot.title = element_text(size = 24, face = "bold", hjust = 0),
            panel.grid = element_blank(),
            legend.text = element_text(size = 16, face = "bold"),
            plot.margin = margin(t=30, l=30)) + 
      scale_fill_manual(values = c("white", "red","blue","grey"), 
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
      mutate(cutoff = ifelse(is.na(value), "N/C or N/A", cutoff)) %>%
      select(side.nerve, param, value, cutoff)
    df_sensory$cutoff = factor(df_sensory$cutoff, levels = c("WNL","Below LLN","N/C or N/A"))
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
            plot.title = element_text(size = 24, face = "bold", hjust = 0),
            panel.grid = element_blank(),
            legend.text = element_text(size = 16, face = "bold"),
            plot.margin = margin(t=30, b=80, l=30)) +
      scale_fill_manual(values = c("Below LLN" = "blue",
                                   "WNL" = "white", 
                                   "N/C or N/A" = "grey"),
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
      mutate(param = factor(param, levels = c("CMAP1","CMAP2","NCV1","DML","Dur1","Dur2","FL"))) %>%
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
        title = list(text = "Parameter view", x = 0.1, y = 1, font = list(size = 22, face = "bold"))
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
        title = list(text = "Nerve view", x = 0.1, y = 1, font = list(size = 22, face = "bold"))
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
                            levels = c("CMAP1","CMAP2","NCV1",  
                                       "DML", "Dur1", "Dur2", 
                                       "FL"))) %>%
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
  
  
  feature_table_cidp = reactive({
    if (is.null(df_motor())) return(NULL) 
    df <- df_motor() %>%
      select(-cutoff)
    df_cidp = spread(df, key = param, value = value)
    
    CMAP1 = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = NA, 
                value = CMAP1, 
                param = "CMAP1")
    DML = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(DML) ~ NA, 
        DML >=150 ~ T, 
        TRUE ~ F), 
        value = DML, 
        param = "DML"
      ) 
    NCV1 = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(NCV1) ~ NA,
        NCV1 <=70 ~ T, 
        TRUE ~ F), 
        value = NCV1, 
        param = "NCV1")  
    NCV2 = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(NCV2) ~ NA,
        NCV2 <=70 ~ T, 
        TRUE ~ F), 
        value = NCV2, 
        param = "NCV2") 
    NCV3 = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(NCV3) ~ NA,
        NCV3 <=70 ~ T, 
        TRUE ~ F), 
        value = NCV3, 
        param = "NCV3")  
    FL = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(FL) ~ NA,
        (FL >120 & CMAP1 >=80)|(FL >150 & CMAP1 <80) ~ T, 
        TRUE ~ F), 
        value = FL, 
        param = "FL")
    FA = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(CMAP1) ~ NA,
        is.na(FL) & CMAP1 >=20 ~ T, # F-wave absence
        TRUE ~ F), 
        value = ifelse(is.na(criteria), NA, 0),
        value = ifelse(isTRUE(criteria), 1, value),
        param = "FA")
    CB1 = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1 == 0 ~ NA,
        CMAP2/CMAP1 <= 0.7 & CMAP1 >=20 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1 == 0, NA, round(CMAP2/CMAP1,2)), 
        param = "CB1")
    # Exclude the tibial nerve 
    CB1$criteria[7] = NA # R.TM
    CB1$criteria[8] = NA # L.TM
    
    CB2 = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1==0 ~ NA,
        is.na(CMAP3) ~ NA,
        CMAP3/CMAP1 <= 0.7 & CMAP1 >=20 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(CMAP3/CMAP1,2)), 
        param = "CB2"
      )
    CB3 = df_cidp %>%
      group_by(side.nerve) %>%
      summarize(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1==0 ~ NA,
        is.na(CMAP4) ~ NA,
        CMAP4/CMAP1 <= 0.7 & CMAP1 >=20 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(CMAP4/CMAP1,2)), 
        param = "CB3"
      )
    TD1 = df_cidp %>%
      filter(str_detect(side.nerve, "TM", negate = T)) %>% 
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1==0 ~ NA,
        Dur2/Dur1 >1.3 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(Dur2/Dur1,2)), 
        value = ifelse(CMAP2 == 0, NA, value), 
        param = "TD1"
      )
    # 100% increase at least for the tibial nerve 
    TD1_tibial = df_cidp %>%
      filter(str_detect(side.nerve, "TM", negate = F)) %>% 
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1==0 ~ NA,
        Dur2/Dur1 >= 2 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(Dur2/Dur1,2)), 
        value = ifelse(CMAP2 == 0, NA, value), 
        param = "TD1"
      )
    TD1 = rbind(TD1, TD1_tibial)
    
    TD2 = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(CMAP1) ~ NA,
        is.na(CMAP3) ~ NA,
        CMAP1==0 ~ NA,
        Dur3/Dur1 >1.3 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(Dur3/Dur1,2)), 
        value = ifelse(CMAP3 == 0, NA, value), 
        param = "TD2"
      )
    TD3 = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(CMAP1) ~ NA,
        is.na(CMAP4) ~ NA,
        Dur4/Dur1 >1.3 ~ T,
        TRUE ~ F), 
        value = ifelse(is.na(CMAP1)|CMAP1==0, NA, round(Dur4/Dur1,2)), 
        value = ifelse(CMAP4 == 0, NA, value), 
        param = "TD3"
      )
    DUR = df_cidp %>%
      group_by(side.nerve) %>%
      summarise(criteria = case_when(
        is.na(CMAP1) ~ NA,
        CMAP1==0 ~ NA,
        Dur1 >= 100 ~ T, 
        TRUE ~ F), 
        value = Dur1, 
        param = "DUR"
      )
    
    df_cidp_table = rbind(CMAP1, DML, NCV1, NCV2, NCV3, FL, FA, CB1, CB2, CB3, 
                          TD1, TD2, TD3, DUR)
    df_cidp_table$param = factor(df_cidp_table$param, 
                                 levels = c("CMAP1", "DML","DUR",
                                            "NCV1","NCV2","NCV3",
                                            "CB1","CB2","CB3",
                                            "TD1","TD2","TD3",
                                            "FL","FA"))
    df_cidp_table$side.nerve = factor(df_cidp_table$side.nerve, 
                                      levels = c("R.MM","L.MM",
                                                 "R.UM","L.UM",
                                                 "R.PM","L.PM",
                                                 "R.TM","L.TM"))
    return(df_cidp_table)
  })
  
  output$cidp_tileView_motor = renderPlot({
    if (is.null(feature_table_cidp())) return(NULL)
    p <- ggplot(feature_table_cidp(), aes(x=side.nerve, y=param,
                                          fill = criteria)) +
      geom_tile(color = "black") + theme_minimal() +
      geom_text(aes(label = value), size = 6) + 
      labs(title = "CIDP", 
           subtitle = "At least one of the following:\n 
DML >= 150% ULN in two nerves\n
NCV <= 70% LLN in two nerves\n
FL >= 120% ULN (150% if CMAP1 < 80% LLN) in two nerves\n
FA (CMAP1 >= 20% LLN) in two nerves, plus >= 1 other parameters in >= 1 other nerve\n
CB >= 0.3 (CMAP >= 20% LLN; excluding tibial nerve) in two nerves, or 1 nerve plus >= 1 other parameters (except FA) in >= 1 other nerve\n
TD >= 1.3 (2 in the tibial nerve) in two nerves\n
DUR > 100 in one nerve, plus >=1 other parameters in >= 1 other nerve\n", 
           caption = "\n021 PNS/EFNS EDX criteria") +
      theme(axis.text.x = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 14, face = "bold"),
            title = element_text(size = 16, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_blank(),
            legend.text = element_text(size = 14, face = "bold"), 
            plot.subtitle = element_text(size = 10, lineheight = 0.5))
    p <- p + scale_fill_manual(
      values = c("white", "red", "grey"), 
      name = "")
    p
  })
}

