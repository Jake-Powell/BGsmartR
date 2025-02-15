#' Chart colours for reports
#'
#' @param n number of colours (allowable values = 1,2,3,4)
#'
#' @return colours
#'
interactive_colour <- function(n){
  color = c('#f46d43', "#FDAE6B", '#848484', '#e6e6e6')
  if(n==2){
    return(color[c(1,4)])
  }
  color[1:n]
}

#' PLot a single trend over time
#'
#' @param years years
#' @param values values
#' @param name name
#' @param report_kind report_kind
#' @param separate_figure_folder separate_figure_folder
#'
#' @return
#'
single_trend_plot <- function(years, values, name, report_kind, separate_figure_folder){
  df = data.frame(years = years, values = values)
  if(report_kind == 'interactive'){
    df$text = paste0('Date: ', years, '-12-31 <br>', name,': ', values)

    fig <- plot_ly(df, x = ~years, y = ~values, type = 'scatter', mode = 'lines', text = ~text, hoverinfo = 'text', line = list(color = interactive_colour(1)))
    fig <- fig %>% layout(title = "",
                          xaxis = list(title = "Year"),
                          yaxis = list(title = paste0("Number of ", name |> tolower())))
    fig <- fig %>% layout(hovermode = 'x')

    if(separate_figure_folder){
      htmlwidgets::saveWidget(fig, file = paste0(figures_dir,'/',name,'_trend.html'),selfcontained = T)
    }
    return(fig)
  }
  if(report_kind == 'static'){
    p =  ggplot(data = df, aes(x = years, y = values)) +
      geom_line(width = 1.1) +
      labs(title="",
           x ="Year",
           y = paste0('Number of ',name))+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    if(separate_figure_folder){
      ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/', name, '_trend.pdf'),device = 'pdf', scale = 1)
    }
    return(p)
  }

}

#' PLot multiple trend lines.
#'
#' @param years years
#' @param data  data
#' @param name name
#' @param report_kind report_kind
#' @param separate_figure_folder separate_figure_folder
#' @param data_type data_type
#' @param split split
#' @param group_order group_order
#'
#' @return
#'
group_trend_plots <- function(years, data, name, report_kind, separate_figure_folder, data_type, split = FALSE, group_order = NULL){
  data_prop = data /rowSums(data)*100
  tt = data.frame(year = years, data)
  names(tt) = c('year', names(data))
  dd = reshape(tt, idvar = "year", varying = list(2:ncol(tt)),
               v.names = "count", timevar = "group", times = names(tt)[-1], direction = "long")
  if(is.null(group_order)){
    dd$group = factor(dd$group, levels = unique(dd$group))
  }else{
    dd$group = factor(dd$group, levels = group_order)
  }

  tt_prop = data.frame(year = years, data_prop)
  names(tt_prop) = c('year', names(data))
  dd_prop = reshape(tt_prop, idvar = "year", varying = list(2:ncol(tt)),
                    v.names = "count", timevar = "group", times = names(tt)[-1], direction = "long")
  dd_prop$group = factor(dd_prop$group, levels = unique(dd_prop$group))

  if(report_kind == 'static'){
    p1 = ggplot(dd, aes(x=year, y = count, group = group, color = group)) +
      geom_line(linewidth  =1.1 ) +
      guides(color=guide_legend(title=paste0(name))) +
      labs(title="",
           x ="Year",
           y = stringr::str_wrap(paste0("Number of ", data_type),width = 18)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.position="bottom")


    p2 = ggplot(dd_prop, aes(fill=group, y=count, x=year)) +
      geom_bar(position="stack", stat="identity", width = 1) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title="",
           x ="Year",
           y = "Percentage (%)") +
      guides(fill=guide_legend(title=paste0('Provenance')))

    p = ggpubr::ggarrange(p1, p2, labels = c("A", "B"),
                          common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 2, align = 'v')

    if(separate_figure_folder){
      ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/', name, '_trend.pdf'),device = 'pdf', scale = 1)
    }
    if(split){
      return(list(number = p1, proportion = p2))
    }else{
      return(p)

    }
  }
  if(report_kind == 'interactive'){
    # fig <- plot_ly(dd, x = ~year, y = ~count, color = ~group, type = 'scatter', mode = 'lines', line = list(color = interactive_colour(length(unique(dd$group)))))
    fig <- plot_ly(dd, x = ~year, y = ~count, color = ~group, type = 'scatter', mode = 'lines', colors = interactive_colour(length(unique(dd$group))))

    fig <- fig |> layout(yaxis = list(title = paste0("Number of ",data_type |> tolower(),"")),
                         xaxis = list(title = "Year"))
    fig <- fig |> layout(hovermode = 'x unified')

    if(separate_figure_folder){
      htmlwidgets::saveWidget(fig, file = paste0(figures_dir,'/',name,'_trend.html'),selfcontained = T)
    }


    dd_prop = data.frame(dd_prop, total = dd$count)

    fig2 <- plot_ly(dd_prop, x = ~year, y = ~count, color = ~group, type = 'bar',
                    hovertext = paste0(dd_prop$group,": ", round(dd_prop$count,digits = 1),'% (', dd_prop$total,')'),
                    hoverinfo = "x+text",
                    colors = interactive_colour(length(unique(dd$group))))


    fig2 <- fig2 |> layout(yaxis = list(title = paste0("Percentage")),
                           xaxis = list(title = "Year"))
    fig2 <- fig2 |> layout(hovermode = 'x unified',
                           barmode = 'stack',
                           bargap =0,
                           legend = list(traceorder = 'normal'),
                           yaxis = list(ticksuffix = '%'))
    fig2

    if(separate_figure_folder){
      htmlwidgets::saveWidget(fig2, file = paste0(figures_dir,'/',name,'_proportional_trend.html'),selfcontained = T)
    }
  }

  return(list(number = fig, proportion = fig2))
}


#' Plot a pair of trend lines.
#'
#' @param years years
#' @param data data
#' @param name name
#' @param report_kind report_kind
#' @param separate_figure_folder separate_figure_folder
#' @param data_type data_type
#' @param split split
#' @param group_order group_order
#'
#' @return
group_trend_plots_pair <- function(years, data, name, report_kind, separate_figure_folder, data_type, split = FALSE, group_order = NULL, color_binary){
  if(!is.null(group_order)){data = data[,match(names(data),group_order)]}
  data_prop = data /rowSums(data)*100

  tt = data.frame(year = years, data)
  names(tt) = c('year', names(data))

  dd = reshape(tt, idvar = "year", varying = list(2:ncol(tt)),
               v.names = "count", timevar = "group", times = names(tt)[-1], direction = "long")
  if(is.null(group_order)){
    dd$group = factor(dd$group, levels = unique(dd$group))
  }else{
    dd$group = factor(dd$group, levels = group_order)
  }

  tt_prop = data.frame(year = years, data_prop)
  names(tt_prop) = c('year', names(data))
  dd_prop = reshape(tt_prop, idvar = "year", varying = list(2:ncol(tt)),
                    v.names = "count", timevar = "group", times = names(tt)[-1], direction = "long")
  if(is.null(group_order)){
    dd_prop$group = factor(dd_prop$group, levels = unique(dd_prop$group))
  }else{
    dd_prop$group = factor(dd_prop$group, levels = group_order)
  }

  if(report_kind == 'static'){
    p1 = ggplot(dd, aes(x=year, y = count, group = group, color = group)) +
      geom_line(linewidth  =1.1 ) +
      guides(color=guide_legend(title=paste0(name))) +
      labs(title="",
           x ="Year",
           y = stringr::str_wrap(paste0("Number of ", data_type),width = 18)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.position="bottom")


    p2 = ggplot(dd_prop, aes(fill=group, y=count, x=year)) +
      geom_bar(position="stack", stat="identity", width = 1) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title="",
           x ="Year",
           y = "Percentage (%)") +
      guides(fill=guide_legend(title=paste0('Provenance')))

    p = ggpubr::ggarrange(p1, p2, labels = c("A", "B"),
                          common.legend = TRUE, legend = "bottom", ncol = 1, nrow = 2, align = 'v')

    if(separate_figure_folder){
      ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/', name, '_trend.pdf'),device = 'pdf', scale = 1)
    }
    if(split){
      return(list(number = p1, proportion = p2))
    }else{
      return(p)

    }
  }
  if(report_kind == 'interactive'){

    colours = color_binary
    fig <- plot_ly(tt,
                   x = ~year,
                   y = tt[,2],
                   type = 'scatter',
                   mode = 'lines',
                   name = names(tt)[2],
                   line = list(color = color_binary[1]),
                   visible = T, showlegend = T) |>
      add_trace(y = tt[,3],
                name = names(tt)[3],
                line = list(color = color_binary[2]),
                visible='legendonly'
      )|>
      layout(yaxis = list(title = paste0("Number of ",data_type |> tolower(),"")),
             xaxis = list(title = "Year"),
             hovermode = 'x unified')
    fig <- fig |> layout(hovermode = 'x unified')

    if(separate_figure_folder){
      htmlwidgets::saveWidget(fig, file = paste0(figures_dir,'/',name,'_trend.html'),selfcontained = T)
    }


    dd_prop = data.frame(dd_prop, total = dd$count)

    fig2 <- plot_ly(dd_prop, x = ~year, y = ~count, color = ~group, type = 'bar',
                    hovertext = paste0(dd_prop$group,": ", round(dd_prop$count,digits = 1),'% (', dd_prop$total,')'),
                    hoverinfo = "x+text",
                    colors = interactive_colour(length(unique(dd$group))))


    fig2 <- fig2 |> layout(yaxis = list(title = paste0("Percentage")),
                           xaxis = list(title = "Year"))
    fig2 <- fig2 |> layout(hovermode = 'x unified',
                           barmode = 'stack',
                           bargap =0,
                           legend = list(traceorder = 'normal'),
                           yaxis = list(ticksuffix = '%'))
    fig2

    if(separate_figure_folder){
      htmlwidgets::saveWidget(fig2, file = paste0(figures_dir,'/',name,'_proportional_trend.html'),selfcontained = T)
    }
  }

  return(list(number = fig, proportion = fig2))
}
