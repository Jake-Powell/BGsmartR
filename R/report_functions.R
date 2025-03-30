#' Chart colours for reports
#'
#' @param n number of colours (allowable values = 1,2,3,4)
#'
#' @export
#' @return colours
#'
interactive_colour <- function(n, type = 'other'){
  color = c('#f46d43', "#FDAE6B", '#848484', '#e6e6e6')
  if(n == 2 & type == 'trend'){
    return(colour[1,3])
  }
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
#' @return graph
#'
single_trend_plot <- function(years, values, name, report_kind, separate_figure_folder){
  df = data.frame(years = years, values = values)
  if(report_kind == 'interactive'){
    df$text = paste0('Date: ', years, '-12-31 <br>', name,': ', values)

    fig <- plotly::plot_ly(df, x = ~years, y = ~values, type = 'scatter', mode = 'lines', text = ~text, hoverinfo = 'text', line = list(color = interactive_colour(1)))
    fig <- fig |> plotly::layout(title = "",
                          xaxis = list(title = "Year"),
                          yaxis = list(title = paste0("Number of ", name |> tolower())),
                          hovermode = 'x')

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
           y = paste0('Number of ',name)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    if(separate_figure_folder){
      ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/', name, '_trend.pdf'),device = 'pdf', scale = 1)
    }
    return(p)
  }

}

#' PLlot multiple trend lines.
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
#' @return plot
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
#' @return plot
group_trend_plots_pair <- function(years, data, name, report_kind, separate_figure_folder, data_type, split = FALSE, group_order = NULL, color_binary = interactive_colour(4, type = 'trend')){
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

#' #' extract turnover information from enriched report (for items)
#' #'
#' #' @param report the cleansed enriched report.
#' #'
#' #' @return data frame with columns $gain_items, $loss_items and  $net_items
#' #'
#' turnover_items <- function(report){
#'   report = report[(report$LossYear >= min_year & report$LossYear <= max_year) | is.na(report$LossYear),]
#'   combined = data.frame(Year = years)
#'
#'   # Items
#'   gain = data.frame(table(report$AccYear))
#'   if(nrow(gain) == 0){
#'     gained = rep(0,nrow(combined))
#'     combined$gain_items = gained
#'   }else{
#'     names(gain) = c('Year', 'Items')
#'     gain$Year = as.numeric(as.character(gain$Year))
#'     gain = gain[gain$Year >=min_year & gain$Year <= max_year,]
#'     gained = rep(0,nrow(combined))
#'     gained[match(gain$Year, combined$Year)] = gain$Items
#'     combined$gain_items = gained
#'   }
#'
#'
#'   loss = data.frame(table(report$LossYear))
#'   if(nrow(loss) == 0){
#'     lost = rep(0,nrow(combined))
#'     combined$loss_items = lost
#'   }else{
#'     names(loss) = c('Year', 'Items')
#'     loss$Year = as.numeric(as.character(loss$Year))
#'     loss = loss[loss$Year >=min_year & loss$Year <= max_year,]
#'     lost = rep(0,nrow(combined))
#'     lost[match(loss$Year, combined$Year)] = loss$Items
#'     combined$loss_items = lost
#'   }
#'
#'
#'   combined$net_items = combined$gain_items - combined$loss_items
#'   return(combined)
#' }
#'
#' #' plot turnover graphs
#' #'
#' #' @param turnover turnover
#' #' @param trend trend
#' #' @param report_kind type of report (i.e either ggplot graphs or plotly graphs)
#' #' @param text text
#' #' @param separate_figure_folder separate_figure_folder
#' #' @param data_type data_type
#' #' @param colors colors for gain and loss
#' #'
#' #' @return list containing the plots
#' plots_turnover <- function(turnover, trend, report_kind, text = '', separate_figure_folder, data_type = 'items', colors = color_binary |> rev()){
#'   years = turnover$Year
#'   dataA = data.frame(turnover[,c(1,3)], rep('Lost', nrow(turnover)))
#'   names(dataA) = LETTERS[1:3]
#'   dataB = data.frame(turnover[,c(1,2)], rep('Gain', nrow(turnover)))
#'   names(dataB) = LETTERS[1:3]
#'   items_data = rbind(dataA, dataB)
#'   if(report_kind == 'static'){
#'
#'     p = ggplot(data=items_data, aes(x=A, y=B, group=C)) +
#'       geom_line(aes(color=C)) +
#'       # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#'       labs(title=paste0("Turnover of ",text," in the LC (By ",data_type,")"),
#'            x ="Year",
#'            y = paste0('Number of ',data_type,'')) +
#'       guides(color=guide_legend(title=paste0('Gain/Loss'))) +
#'       # theme(text = element_text(size=table_font_size)) +
#'       scale_color_manual(values=c("blue", "red"))+
#'       theme(legend.direction = "horizontal", legend.position = "top", legend.justification = "right")
#'
#'
#'     if(separate_figure_folder){
#'       ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/','turnover_',stringr::str_replace_all(text, ' ', '_'),'_gain_loss_',data_type,'.pdf'),device = 'pdf', scale = 1, width = 20, height = 12, limitsize = FALSE)
#'     }
#'     p1=p
#'
#'     # Color based on value
#'     color <- ifelse(turnover$net_items < 0, "pink", "lightblue")
#'     p = ggplot(turnover, aes(x = Year, y = net_items)) +
#'       geom_bar(stat = "identity",
#'                show.legend = FALSE,
#'                fill = color,      # Background color
#'                color = "white") + # Border color
#'       # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#'       labs(title=paste0("Net turnover of ",text," in the LC (By ",data_type,")"),
#'            x ="Year",
#'            y = paste0('Number of ',data_type,''))
#'     # theme(text = element_text(size=table_font_size))
#'
#'     if(separate_figure_folder){
#'       ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/','Net_turnover_',stringr::str_replace_all(text, ' ', '_'),'_',data_type,'.pdf'),device = 'pdf', scale = 1, limitsize = FALSE)
#'     }
#'     p2=p
#'
#'
#'     # Trend plot.
#'     trend_data = data.frame(year = turnover$Year, trend = trend)
#'     p = ggplot(trend_data, aes(x = year, y = trend)) +
#'       geom_line(width = 1.1) +
#'       labs(title="",
#'            x ="Year",
#'            y = paste0('Number of ',data_type,''))
#'     p3 = p
#'     return(list(gain_loss = p1, net = p2, trend = p3))
#'   }
#'   if(report_kind == 'interactive'){
#'     # gain_loss plot
#'     fig <- plot_ly(items_data, x = ~A, y = ~B, color = ~C, colors = colors, type = 'scatter', mode = 'lines+markers')
#'     fig <- fig |> layout(yaxis = list(title = paste0("Number of ",data_type,"")),
#'                          xaxis = list(title = "Year"))
#'     fig <- fig |> layout(hovermode = 'x unified')
#'
#'     if(separate_figure_folder){
#'       htmlwidgets::saveWidget(fig, file = paste0(figures_dir, '/','line-turnover_',stringr::str_replace_all(text, ' ', '_'),'_gain_loss_',data_type,'.html'))
#'     }
#'     fig1 = fig
#'
#'     # net plot
#'     color <- ifelse(turnover$net_items < 0, 'B', 'A')
#'     texto = paste0('Net: ', turnover$net_items, '<br>',
#'                    'Gain: ', turnover$gain_items, '<br>',
#'                    'Loss: ', turnover$loss_items, '<br>'
#'     )
#'     fig = plot_ly(turnover, x = ~Year, y = ~net_items, type = 'bar', hoverinfo = "x+text",  hovertext = texto, color = color, colors = colors, showlegend = FALSE)
#'     fig <- fig |> layout(yaxis = list(title = paste0("Net number of ",data_type,"")),
#'                          xaxis = list(title = "Year"))
#'     fig <- fig |> layout(hovermode = 'x unified')
#'
#'     if(separate_figure_folder){
#'       htmlwidgets::saveWidget(fig, file = paste0(figures_dir, '/','Bar-Net_',stringr::str_replace_all(text, ' ', '_'),'turnover_of_',data_type,'.html'))
#'     }
#'     fig2 = fig
#'
#'     # Trend plot.
#'     trend_data = data.frame(year = turnover$Year, trend = trend)
#'     fig = plot_ly(trend_data, x = ~year, y = ~trend, type = 'scatter', mode = 'line', hoverinfo = "x+text", hovertext = trend)
#'     fig <- fig |> layout(yaxis = list(title = paste0("Number of ",data_type,"")),
#'                          xaxis = list(title = "Year"))
#'     fig <- fig |> layout(hovermode = 'x unified')
#'     fig3= fig
#'
#'     # Combined trend and net.
#'     fig <- plot_ly()
#'     # Add traces
#'     ay <- list(
#'       tickfont = list(color = "black"),
#'       overlaying = "y",
#'       side = "right",
#'       title = paste0("Total ", tolower(data_type)))
#'
#'     fig <- fig %>% add_trace(x = years, y = turnover$net_items, name = "Net", type = 'bar', hoverinfo = "x+text",  hovertext = texto, color = color, colors = colors, showlegend = FALSE)
#'
#'     fig <- fig %>% add_trace(x = years, y = trend, name = "Total", mode = "lines+markers", type = "scatter", line = list(color = 'black'), marker = list(color = 'black'),yaxis = "y2")
#'
#'
#'     # Set figure title, x and y-axes titles
#'     fig <- fig |> layout(
#'       title = "", yaxis2 = ay,
#'       xaxis = list(title="Year"),
#'       yaxis = list(title = paste0("Net turnover of ", tolower(data_type)))
#'     ) |>
#'       layout(plot_bgcolor='white',
#'              xaxis = list(
#'                zerolinecolor = '#ffff',
#'                zerolinewidth = 2,
#'                gridcolor = 'ffff'),
#'              yaxis2 = list(
#'                zeroline = TRUE,
#'                zerolinecolor = 'ffff',
#'                zerolinewidth = 2,
#'                gridcolor = 'ffff',
#'                showgrid = FALSE),
#'              yaxis = list(
#'                zeroline = TRUE,
#'                zerolinecolor = '#f0f0f0',
#'                zerolinewidth = 1,
#'                gridcolor = '#f0f0f0',
#'                griddash = 'solid',
#'                gridwidth = 1,showgrid = T),
#'              margin = list(t = 10, l = 20, r = 70, b = 20, pad = 4),
#'              autosize = T
#'       )
#'     fig <- fig |> layout(hovermode = 'x unified')
#'     fig4 = fig
#'
#'     return(list(gain_loss = fig1, net = fig2, trend = fig3, trend_and_net = fig4))
#'
#'   }
#' }
#'
#' #' plot proportional turnover graphs
#' #'
#' #' @param turnover turnover
#' #' @param overall_turnover overall_turnover
#' #' @param collection_proportion collection_proportion
#' #' @param separate_figure_folder separate_figure_folder
#' #' @param report_kind report_kind
#' #' @param text text
#' #' @param data_type data_type
#' #' @param quantity quantity
#' #'
#' #' @return list containing the plots
#' #'
#' proportional_plots_turnover <- function(turnover,
#'                                         overall_turnover,
#'                                         collection_proportion,
#'                                         separate_figure_folder,
#'                                         report_kind,
#'                                         text = '',
#'                                         data_type = 'items',
#'                                         quantity = ''){
#'   if(report_kind == 'static'){
#'     # Gain
#'     plot_data = data.frame(year = turnover[,1],
#'                            specific = turnover[,2],
#'                            all = overall_turnover[,2])
#'     plot_data$prop_specific = round(plot_data$specific / plot_data$all *100,digits=2)
#'     plot_data$prop_specific_collection = collection_proportion*100
#'     plot_data_gain = plot_data
#'     p = ggplot(plot_data, aes(y=prop_specific, x=year)) +
#'       geom_bar(stat="identity", width = 1, fill = 'lightblue', col = 'black') +
#'       geom_point(aes(x = year, y = prop_specific_collection))+
#'       labs(title=paste0("Proportion of gained ",data_type," that are ",text," in the LC"),
#'            x ="Year",
#'            y = "Percentage (%)") +
#'       # theme(text = element_text(size=table_font_size)) +
#'       labs(caption = paste0("Black points are proportion of ",data_type," in the LC that are ",text," each year."))
#'     p1 = p
#'
#'     if(separate_figure_folder){
#'       ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/',stringr::str_replace_all(text,' ','_'),'_gain_',data_type,'_proportion.pdf'),device = 'pdf', scale = 1, limitsize = FALSE)
#'     }
#'
#'     ### Plot of proportion of lost items being native.
#'
#'     plot_data = data.frame(year =  turnover[,1],
#'                            specific = turnover[,3],
#'                            all = overall_turnover[,3])
#'     plot_data$prop_specific = round(plot_data$specific / plot_data$all *100,digits=2)
#'     plot_data$prop_specific_collection = collection_proportion*100
#'     plot_data_loss = plot_data
#'
#'     p = ggplot(plot_data, aes(y=prop_specific, x=year)) +
#'       geom_bar(stat="identity", width = 1, fill = 'pink', col = 'black') +
#'       geom_point(aes(x = year, y = prop_specific_collection))+
#'       # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#'       labs(title=paste0("Proportion of lost ",data_type," that are ",text," in the LC"),
#'            x ="Year",
#'            y = "Percentage (%)") +
#'       # theme(text = element_text(size=table_font_size)) +
#'       labs(caption = paste0("Black points are proportion of ",data_type," in the LC that are ",text," each year."))
#'
#'     if(separate_figure_folder){
#'       ggplot2::ggsave(plot = p, filename = paste0(figures_dir, '/',stringr::str_replace_all(text,' ','_'),'_loss_',data_type,'_proportion.pdf'),device = 'pdf', scale = 1, limitsize = FALSE)
#'     }
#'
#'     return(list(gain = p1, loss = p, data_gain = plot_data_gain, data_loss = plot_data_loss))
#'   }
#'   if(report_kind =='interactive'){
#'     # Gain
#'     plot_data = data.frame(year = turnover[,1],
#'                            specific = turnover[,2],
#'                            all = overall_turnover[,2])
#'     plot_data$prop_specific = round(plot_data$specific / plot_data$all *100,digits=2)
#'     plot_data$prop_specific_collection = collection_proportion*100
#'     plot_data_gain = plot_data
#'     fig = plot_ly(plot_data, x = ~year, y = ~prop_specific, type = 'bar',
#'                   name = paste0('New Accessions'),
#'                   marker = list(color = color_binary[2]),
#'                   hovertemplate = paste("%{y:.2f}% of new accessions <extra></extra>")
#'     )
#'
#'     fig <- fig |> layout(yaxis = list(title = paste0("Percentage")),
#'                          xaxis = list(title = "Year"))
#'     fig <- fig |> add_trace(x = ~year, y = ~prop_specific_collection, type = 'scatter', mode = 'lines+markers',
#'                             name = paste0('Collection'),
#'                             marker = list(color = 'black'),
#'                             line = list(color = 'black'),
#'                             hovertemplate = paste("%{y:.2f}% of the collection <extra></extra>")
#'     )
#'     fig <- fig |> layout(hovermode = 'x unified',
#'                          yaxis = list(ticksuffix = '%'),
#'                          xaxis = list(hoverformat = paste0('fvf')),
#'                          legend=list(title=list(text=paste0('<b>',text, '</b>'))))
#'
#'     if(separate_figure_folder){
#'       htmlwidgets::saveWidget(fig, file = paste0(figures_dir, '/','Line_Bar-Proportion_of_gained_',stringr::str_replace_all(text, ' ', '_'),'_',data_type,'.html'))
#'     }
#'
#'     fig1 = fig
#'
#'     ### Plot of proportion of lost items being native.
#'
#'     plot_data = data.frame(year =  turnover[,1],
#'                            specific = turnover[,3],
#'                            all = overall_turnover[,3])
#'     plot_data$prop_specific = round(plot_data$specific / plot_data$all *100,digits=2)
#'     plot_data$prop_specific_collection = collection_proportion*100
#'     plot_data_loss = plot_data
#'
#'     fig = plot_ly(plot_data, x = ~year,
#'                   y = ~prop_specific,
#'                   type = 'bar',
#'                   name = paste0('Lost Accessions'),
#'                   marker = list(color = color_binary[1]),
#'                   hovertemplate = paste("%{y:.2f}% of lost accessions <extra></extra>"))
#'     fig <- fig |> layout(yaxis = list(title = paste0("Percentage")),
#'                          xaxis = list(title = "Year"))
#'     fig <- fig |> add_trace(x = ~year,
#'                             y = ~prop_specific_collection,
#'                             type = 'scatter',
#'                             mode = 'lines+markers',
#'                             name = paste0('Collection'),
#'                             marker = list(color = 'black'),
#'                             line = list(color = 'black'),
#'                             hovertemplate = paste("%{y:.2f}% of the collection<extra></extra>"))
#'     fig <- fig |> layout(hovermode = 'x unified',
#'                          yaxis = list(ticksuffix = '%'),
#'                          legend=list(title=list(text=paste0('<b>',text, '</b>'))))
#'
#'     if(separate_figure_folder){
#'       htmlwidgets::saveWidget(fig, file = paste0(figures_dir, '/','Line_Bar-Proportion_of_lost_',stringr::str_replace_all(text, ' ', '_'),'_',data_type,'.html'))
#'     }
#'
#'     return(list(gain = fig1, loss = fig,  data_gain = plot_data_gain, data_loss = plot_data_loss))
#'
#'   }
#' }
