# Publish with: 
# rsconnect::deployApp("C:/Users/hydro/OneDrive/Documents/GitHub/snotel_plot")

library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(RCurl)
library(plotly)
library(leaflet)
library(sf)
library(stringr)

metadata <- read.csv("snotel_metadata.csv")

sf_data <- st_as_sf(x = metadata,
                    coords = c("longitude", "latitude"),
                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Snotel Sites"),

    # Sidebar with a dropdown
    
    
    sidebarLayout(
        sidebarPanel(
          # Drop down of sites
          selectInput("state_selected", "Select State", choices = sort(unique(metadata$state)),selected = 'ID'),
          
          uiOutput("secondSelection"),
          
          # Drop down of Units
          selectInput("units_selected", "Select Units", choices = c('Metric','"Freedom units"'),selected = 'Metric'),
          
          # Drop down of water years
          selectInput("water_year_selected", "Select Water Year", choices = c(2023,2024,2025),selected = 2025),
          
          # Start date
          selectInput('start_month_selected','Select Start Month',choices = c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','June','July','Aug','Sept'),selected = 'Oct'),
          selectInput('start_date_selected','Select Start Day',choices = 1:31,selected = 1),
          
          # Prior point
          sliderInput("prior_point", "Number of hours prior:",min = 0, max = 96,value = 24,step = 3),
          
          # SWE Range
          uiOutput('swe_range'),
 
          # Depth Range
          uiOutput('depth_range'),
          
          # Download Button
          downloadButton("downloadData", "Download Data (csv)"),
          
          # Download graph Button
          downloadButton("downloadFigure", "Download Annotation Plot (png)")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("label"),
           hr(),
           tabsetPanel(
             tabPanel('Annotation plot',plotOutput('snotel_plot')),
             tabPanel('Interactive plot', plotlyOutput('snotel_plotly')),
             tabPanel('State Map', leafletOutput('snotelmap')),
             tabPanel('State Table',DT::dataTableOutput('data_table')))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    observeEvent(input$site_name_selected,{
      points <-  sf_data[sf_data$state == input$state_selected,]
      updateSelectInput(session, "site_name_selected", "Select Site to Plot",
                        choices =  sort(as.character(metadata[metadata$state==input$state_selected,"site_name"])),
                        selected = c(input$site_name_selected))
    })
  
    # Initialize the selected station as NULL
    selected_site <- reactiveVal(NULL)

    output$secondSelection <- renderUI({
      selectizeInput("site_name_selected", "Select Site to Plot", choices = sort(as.character(metadata[metadata$state==input$state_selected,"site_name"])))
    })
    
    # Initialize the selected station as NULL
    swe_range <- reactiveVal(NULL)
    
    output$swe_range <- renderUI({
      sliderInput("swe_range", "SWE Plot Y-axis:",min = 0, max = ceiling(max(data()$WTEQ)*2),value = c(floor(min(data()$WTEQ[as.Date(data()$Date) >= as.Date(min_x_date())])),ceiling(max(data()$WTEQ))))
    })
    
    swe_y <- reactive({
      swe_y <-input$swe_range
    })
    
    # Initialize the selected station as NULL
    depth_range <- reactiveVal(NULL)
    
    output$depth_range <- renderUI({
      sliderInput("depth_range", "Depth Plot Y-axis:",min = 0, max = ceiling(max(data()$SNWD)*2),value = c(floor(min(data()$SNWD[as.Date(data()$Date) >= as.Date(min_x_date())],na.rm=T)),ceiling(max(data()$SNWD))))
    })
    
    depth_y <- reactive({
      depth_y <-input$depth_range
    })
    
    min_x_date <- reactive({months_df <- data.frame('month'=c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','June','July','Aug','Sept'),
                           'number'=c(10,11,12,1,2,3,4,5,6,7,8,9))
      month_num <- as.numeric(months_df$number[months_df$month == input$start_month_selected])
      day <- as.integer(input$start_date_selected)
      if(month_num %in% c(4,6,9,11)){day <- min(day,30)}
      if(month_num %in% c(2)){day <- min(day,28)}
      if(month_num > 9){year <- as.integer(input$water_year_selected)-1}else{year <- input$water_year_selected}
      min_x <- as.POSIXct(paste0(year,'-',month_num,'-',day),format = '%Y-%m-%d')
      min_x
    })
    
    data <- reactive({
      site_name_i <- input$site_name_selected
      wateryear <- input$water_year_selected
      site_i <- metadata$site_id[metadata$site_name == site_name_i]
      units <- input$units_selected
      
      baseurl <- paste0("https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Historic+&report=STAND&timeseries=Hourly&format=copy&sitenum=",site_i,"&year=",wateryear,"&month=WY&day=NULL")
      
      tmp <- RCurl::getURL(baseurl)
      tmpData <- read.table(textConnection(tmp),header = TRUE, skip = 6, strip.white = T, stringsAsFactors = FALSE,sep = ",",na.strings = c(-99.9))
      if(nrow(tmpData)>0){
        tmpData$X <- NULL
        tmpData$POSIXct <- as.POSIXct(paste(as.character(tmpData$Date),as.character(tmpData$Time), sep = " "),format = "%Y-%m-%d %H:%M",tz = "MST")
        tmpData <- tmpData[,names(tmpData) %in% c('Site.Id','Date','Time','WTEQ.I.1..in.','PREC.I.1..in.','TOBS.I.1..degC.','SNWD.I.1..in.','POSIXct')]
        if(units == 'Metric'){
          tmpData$WTEQ <- tmpData$WTEQ.I.1..in. * 2.54 * 10 # in * cm/in * mm/cm  = mm
          tmpData$WTEQ_unit <- 'mm'
          tmpData$PREC <- tmpData$PREC.I.1..in. * 2.54 * 10 # in * cm/in * mm/cm  = mm
          tmpData$PREC_unit <- 'mm'
          tmpData$TOBS <- tmpData$TOBS.I.1..degC
          tmpData$TOBS_unit <- 'C'
          tmpData$SNWD <- tmpData$SNWD.I.1..in. * 2.54 # in * cm/in = cm
          tmpData$SNWD_unit <- 'cm'
        }else{
          tmpData$WTEQ <- tmpData$WTEQ.I.1..in.
          tmpData$WTEQ_unit <- 'in'
          tmpData$PREC <- tmpData$PREC.I.1..in.
          tmpData$PREC_unit <- 'in'
          tmpData$TOBS <- tmpData$TOBS.I.1..degC * 9/5 + 32 # C to F
          tmpData$TOBS_unit <- 'F'
          tmpData$SNWD <- tmpData$SNWD.I.1..in.
          tmpData$SNWD_unit <- 'in'
        }
        tmpData$PREC <- as.numeric(tmpData$PREC)
        
        tmpData <- tmpData[,names(tmpData) %in% c('Site.Id','Date','Time','WTEQ','WTEQ_unit','PREC','PREC_unit','TOBS','TOBS_unit','SNWD','SNWD_unit','POSIXct')]
        
        for(m in c('WTEQ','TOBS','SNWD')){
          col_m <- which(names(tmpData) == m)
          index <- which(is.na(tmpData[,col_m]))
          if(length(index)>0){
            tmpData[,col_m][index] <- approx(x = tmpData$POSIXct,y = tmpData[,col_m],xout = tmpData$POSIXct[index],method = 'linear',rule = 2)$y
          }
        }
      }else{
        df_i <- data.frame('Site.Id'= site_i,'Date'=NULL,'Time'=NULL,'WTEQ'=NULL,'WTEQ_unit'=NULL,'PREC'=NULL,'PREC_unit'=NULL,'TOBS'=NULL,'TOBS_unit'=NULL,'SNWD'=NULL,'SNWD_unit'=NULL,'POSIXct'=NULL)
      }
      
      df_i <- tmpData
      df_i
    })
    
    # Read in current Snotel Data
    snotel_today  <- eventReactive(input$state_selected,{
      
      snotel_today <- metadata[metadata$state==input$state_selected,]
      snotel_today$total_snow_depth <- 0
      snotel_today$change_in_depth_24_hr <- 0
      units <- input$units_selected
      snotel_today$depth_units <- ifelse(units == 'Metric','cm','in')
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = paste0("Fetching data for: ",input$state_selected), value = 0)
      # Number of times we'll go through the loop
      
      for(i in 1:nrow(snotel_today)){
        
        progress$inc(1/nrow(snotel_today), detail = paste0("Site ",i,' of ',nrow(snotel_today)))
        
        baseurl <- paste0("https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Current+&report=STAND&timeseries=&format=copy&sitenum=",snotel_today$site_id[i],"&current=DAY")
        # baseurl <- paste0("https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Current+&report=STAND&timeseries=&format=copy&sitenum=423&current=DAY")
        tmp <- RCurl::getURL(baseurl)
        tmpData <- read.table(textConnection(tmp),header = TRUE, skip = 6, strip.white = T, stringsAsFactors = FALSE,sep = ",",na.strings = c(-99.9))
        if(nrow(tmpData>0)){
          tmpData$X <- NULL
          tmpData$POSIXct <- as.POSIXct(paste(as.character(tmpData$Date),as.character(tmpData$Time), sep = " "),format = "%Y-%m-%d %H:%M",tz = "MST")
          if(!('WTEQ.I.1..in.' %in% colnames(tmpData))){tmpData$WTEQ.I.1..in. <- NA}
          tmpData <- tmpData[,names(tmpData) %in% c('Site.Id','Date','Time','WTEQ.I.1..in.','PREC.I.1..in.','TOBS.I.1..degC.','SNWD.I.1..in.','POSIXct')]
          tmpData$WTEQ.I.1..in. <- as.numeric(tmpData$WTEQ.I.1..in.)
          tmpData$PREC.I.1..in. <- as.numeric(tmpData$PREC.I.1..in.)
          tmpData$TOBS.I.1..degC. <- as.numeric(tmpData$TOBS.I.1..degC.)
          tmpData$SNWD.I.1..in. <- as.numeric(tmpData$SNWD.I.1..in.)
          
          if(units == 'Metric'){
            tmpData$WTEQ <- tmpData$WTEQ.I.1..in. * 2.54 * 10 # in * cm/in * mm/cm  = mm
            tmpData$WTEQ_unit <- 'mm'
            tmpData$PREC <- tmpData$PREC.I.1..in. * 2.54 * 10 # in * cm/in * mm/cm  = mm
            tmpData$PREC_unit <- 'mm'
            tmpData$TOBS <- tmpData$TOBS.I.1..degC
            tmpData$TOBS_unit <- 'C'
            tmpData$SNWD <- tmpData$SNWD.I.1..in. * 2.54 # in * cm/in = cm
            tmpData$SNWD_unit <- 'cm'
          }else{
            tmpData$WTEQ <- tmpData$WTEQ.I.1..in.
            tmpData$WTEQ_unit <- 'in'
            tmpData$PREC <- tmpData$PREC.I.1..in.
            tmpData$PREC_unit <- 'in'
            tmpData$TOBS <- tmpData$TOBS.I.1..degC * 9/5 + 32 # C to F
            tmpData$TOBS_unit <- 'F'
            tmpData$SNWD <- tmpData$SNWD.I.1..in.
            tmpData$SNWD_unit <- 'in'
          }
          
          for(m in c('WTEQ','TOBS','SNWD')){
            col_m <- which(names(tmpData) == m)
            tmpData[,col_m] <- as.numeric(tmpData[,col_m])
            # index <- which(is.na(tmpData[,col_m]))
            # if(length(index)>0 & length(index) < nrow(tmpData)){
            #   tmpData[,col_m][index] <- approx(x = tmpData$POSIXct,y = tmpData[,col_m],xout = tmpData$POSIXct[index],method = 'linear',rule = 2)$y
            # }
          }
          snotel_today$total_snow_depth[i] <- round(max(as.numeric(tmpData$SNWD),na.rm=T),1)
          snotel_today$change_in_depth_24_hr[i] <- round(max(as.numeric(tmpData$SNWD),na.rm=T)-min(as.numeric(tmpData$SNWD),na.rm=T),1)
        }else{
          snotel_today$total_snow_depth[i] <- 0
          snotel_today$change_in_depth_24_hr[i] <- 0
        }
      }
      snotel_today <- snotel_today[,c('site_name','total_snow_depth','change_in_depth_24_hr','depth_units','latitude','longitude','elev','county')] 
    })
    
    annotatedPlot <- reactive({
      min_x <- min_x_date()
      df_i <- data()
      site_name_i <- input$site_name_selected
      units <- input$units_selected
      # storm_totals <- storm_totals()
      if(units == 'Metric'){
        y1 = -5
        y2 = 0
      }else{
        y1 = 20
        y2 = 32
      }
      
      depth_gain_24_hour <- round(df_i$SNWD[nrow(df_i)]-df_i$SNWD[nrow(df_i)-input$prior_point],1)
      
      temp <- ggplot(data = df_i)+
        theme_bw()+
        annotate('rect',
                 xmin = max(min(df_i$POSIXct),min_x),
                 xmax = max(df_i$POSIXct),
                 ymin = -Inf,
                 ymax = y1,
                 fill = "blue",
                 alpha = 0.1)+
        annotate('rect',
                 xmin = max(min(df_i$POSIXct),min_x),
                 xmax = max(df_i$POSIXct),
                 ymin = y1,
                 ymax = y2,
                 fill = "cyan",
                 alpha = 0.5)+
        annotate('rect',
                 xmin = max(min(df_i$POSIXct),min_x),
                 xmax = max(df_i$POSIXct),
                 ymin = y2,
                 ymax = Inf,
                 fill = "orange",
                 alpha = 0.1)+
        geom_hline(yintercept = y2,color = 'red')+
        geom_line(aes(x = POSIXct,y = TOBS))+
        ylab(paste0('Air Temp [',unique(df_i$TOBS_unit),']'))+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = 'Date')+
        # scale_y_continuous(expand = c(0,0))+
        geom_vline(xintercept = max(df_i$POSIXct)-60*60*input$prior_point,lty=2)+
        theme(plot.title = element_text(size = 12))
      
      swe <- ggplot(data = df_i)+
        annotate('rect',
                 xmin = max(min(df_i$POSIXct),min_x),
                 xmax = max(df_i$POSIXct),
                 ymin = -Inf,
                 ymax = Inf,
                 fill = "grey",
                 alpha = 0.1)+
        geom_line(aes(x = POSIXct,y = WTEQ))+
        theme_bw()+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = 'Date')+
        scale_y_continuous(limits = c(swe_y()[1],swe_y()[2]))+
        geom_vline(xintercept = max(df_i$POSIXct)-60*60*input$prior_point,lty=2)+
        ylab(paste0('SWE [',unique(df_i$WTEQ_unit),']'))
      
      depth <- ggplot(data = df_i)+
        geom_line(aes(x = POSIXct,y = SNWD))+
        theme_bw()+
        annotate('rect',
                 xmin = max(min(df_i$POSIXct),min_x),
                 xmax = max(df_i$POSIXct),
                 ymin = -Inf,
                 ymax = Inf,
                 fill = "grey",
                 alpha = 0.1)+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = 'Date')+
        scale_y_continuous(limits = c(depth_y()[1],depth_y()[2]))+
        geom_vline(xintercept = max(df_i$POSIXct)-60*60*input$prior_point,lty=2)+
        ggtitle(paste0('Site: ',site_name_i,', ',input$prior_point,' hr change: ',depth_gain_24_hour,' [',unique(df_i$SNWD_unit),'], Updated: ',max(df_i$POSIXct)))+
        ylab(paste0('Depth [',unique(df_i$SNWD_unit),']'))
      
      gridExtra::grid.arrange(depth,swe,temp)
    })
    
    output$snotel_plot <- renderPlot({
      annotatedPlot()
    })
    
    output$snotel_plotly <- renderPlotly({
      min_x <- min_x_date()
      df_i <- data()
      units <- input$units_selected
      
      site_name_i <- input$site_name_selected
      
      depth_gain_24_hour <- round(df_i$SNWD[nrow(df_i)]-df_i$SNWD[nrow(df_i)-input$prior_point],1)
      
      temp <- ggplot(data = df_i)+
        theme_bw()+
        geom_hline(yintercept = 0,color = 'red')+
        geom_line(aes(x = POSIXct,y = TOBS))+
        ylab(paste0('Air Temp [',unique(df_i$TOBS_unit),']'))+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = '')+
        theme_minimal()+
        theme(plot.title = element_text(size = 8))
      
      swe <- ggplot(data = df_i)+
        geom_line(aes(x = POSIXct,y = WTEQ))+
        theme_bw()+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = '')+
        theme_minimal()+
        ylab(paste0('SWE [',unique(df_i$WTEQ_unit),']'))
      
      depth <- ggplot(data = df_i)+
        geom_line(aes(x = POSIXct,y = SNWD))+
        theme_bw()+
        scale_x_datetime(limits = c(min_x,NA),expand = c(0,0),name = 'Date')+
        theme_minimal()+
        ylab(paste0('Depth [',unique(df_i$SNWD_unit),']'))
      
      temp_ply <- ggplotly(temp) %>% add_annotations(text = paste0("Air Temperature [degrees ",unique(df_i$TOBS_unit),']'),x = 0,
                                                     y = 1,
                                                     yref = "paper",
                                                     xref = "paper",
                                                     xanchor = "left",
                                                     yanchor = "top",
                                                     yshift = 20,
                                                     showarrow = FALSE,
                                                     font = list(size = 15))
      swe_ply <- ggplotly(swe) %>% add_annotations(text = paste0("Snow Water Equivalent [",unique(df_i$WTEQ_unit),']'),x = 0,
                                                   y = 1,
                                                   yref = "paper",
                                                   xref = "paper",
                                                   xanchor = "left",
                                                   yanchor = "top",
                                                   yshift = 20,
                                                   showarrow = FALSE,
                                                   font = list(size = 15))
      depth_ply <- ggplotly(depth) %>% add_annotations(text = paste0("Snow Depth [",unique(df_i$SNWD_unit),']'),x = 0,
                                                       y = 1,
                                                       yref = "paper",
                                                       xref = "paper",
                                                       xanchor = "left",
                                                       yanchor = "top",
                                                       yshift = 20,
                                                       showarrow = FALSE,
                                                       font = list(size = 15))
      
      subplot(depth_ply,swe_ply,temp_ply,nrows = 3, shareX = TRUE, margin = 0.06)
      
    })
    
    output$snotelmap <- renderLeaflet({
      points <-  sf_data[sf_data$state == input$state_selected,]
      units <- input$units_selected
      
      df <- snotel_today()
      
      pal <- colorNumeric(palette = c("brown","grey","blue"), domain = df$change_in_depth_24_hr)
      
      leaflet(points) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(radius = 4,fillOpacity = 0.8,group = "markers",
                   popup = ~paste(site_name, "<br>",
                                  elev, "ft above sea level<br>")
                   ,label = ~site_name,color = ~pal(df$change_in_depth_24_hr)) %>%
        addCircles(data = metadata[metadata$site_name == input$site_name_selected,],lng = ~longitude,lat = ~latitude,
                   popup = ~paste(site_name, "<br>",
                                  elev, "ft above sea level<br>"),label =~site_name,color = 'black') %>%
        addLegend(
          values=~happiness_score, 
          opacity=0.9, 
          title = "24 hr change in depth", 
          position = "bottomleft",
          colors = c('blue',"grey","brown",'black'),
          labels = c(round(max(df$change_in_depth_24_hr,na.rm=T),1),round(median(df$change_in_depth_24_hr,na.rm=T),1),round(min(df$change_in_depth_24_hr,na.rm=T),1),'Selected'),
          group = "markers"
        )
    })
    
    observeEvent(input$snotelmap_marker_click, {
      points <-  metadata[metadata$state==input$state_selected,]
      click <- input$snotelmap_marker_click
      map_click_site_name <- points[which(points$latitude == click$lat & points$longitude == click$lng),]$site_name
      updateSelectInput(session, "site_name_selected", "Select Site to Plot", 
                        choices = sort(as.character(metadata[metadata$state==input$state_selected,"site_name"])), 
                        selected = as.character(map_click_site_name))
    })
    
    # output$label <- renderText({
    #   df_i <- data()
    #   depth_gain_24_hour <- round(df_i$SNWD[nrow(df_i)]-df_i$SNWD[nrow(df_i)-24],1)
    #   paste0('Site: ',input$site_name_selected,', 24 hr change: ',depth_gain_24_hour,' [',unique(df_i$SNWD_unit),'], Updated: ',max(df_i$POSIXct))
    # })
    
    output$data_table <- DT::renderDataTable({
      DT::datatable(data = snotel_today(),caption = 'Snotel Data')
    })

    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$site_name_selected,Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
    
    # storm_totals <- reactive({
    #   df_daily <- data() %>%
    #     group_by(Date) %>%
    #     summarize(max_swe = max(WTEQ)) %>%
    #     mutate(storm_total = c(0,diff(max_swe)))
    # 
    #   storms <- df_daily[df_daily$storm_total>0,]   
    #   storms$date_diff <- as.numeric(c(diff(storms$Date),0))
    #   storms$date_diff[storms$date_diff == 1] <- 0
    #   storms$date_diff[storms$date_diff > 1] <- 1
    #   storms$storm <- cumsum(storms$date_diff)
    #   storm_totals <- storms %>%
    #     group_by(storm) %>%
    #     summarise(start_date = min(Date),end_date = max(Date),storm_total = sum(storm_total))
    #   
    #   storm_totals
    # })
 
    # Downloadable figure of selected dataset ----
    output$downloadFigure <- downloadHandler(
      filename = function() {
        paste0(stringr::str_replace_all(string = input$site_name_selected,' ',replacement = '_'),Sys.Date(),'.png')
      },
      content = function(file) {
        ggsave(filename = file,plot = annotatedPlot(), width = 12, height = 8, units = "in", device = "png")
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
