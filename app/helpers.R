
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


