library("shiny")
library("DT")
library("rvest")
library('tidyr')
library("lubridate")
library("stringi")
library("ggplot2")
library("dplyr")
# My functions

get_first_google_link <- function(name, root = TRUE) {
  url = URLencode(paste0("https://www.google.com/search?q=",name))
  page <- xml2::read_html(url)
  # extract all links
  nodes <- rvest::html_nodes(page, "a")
  links <- rvest::html_attr(nodes,"href")
  # extract first link of the search results
  link <- links[startsWith(links, "/url?q=")][1]
  # clean it
  link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)
  return(link)
}

generate_tffrs_df <- function(df,df_name,event_names,tffrs_df) {
  for (track_events in event_names) {
    if (grepl(track_events,df_name,fixed=TRUE) == TRUE) { 
      if (grepl("Indoor",df_name,fixed=TRUE) == TRUE) {
        Time <- as.list(as.data.frame(t(df[1])))
        Meet <- as.list(as.data.frame(t(df[2])))
        Date <- as.list(as.data.frame(t(df[3])))
        Event <- rep(track_events,length(Time))
        Season <- rep("Indoor",length(Time))
      }
      else { 
        Time <- as.list(as.data.frame(t(df[1])))
        Meet <- as.list(as.data.frame(t(df[2])))
        Date <- as.list(as.data.frame(t(df[3])))
        Event <- rep(track_events,length(Time))
        Season <- rep("Outdoor",length(Time))
      }
    }
    else { 
      next
    }
  }
  for (i in 1:length(Time)) {
    tffrs_df[nrow(tffrs_df) + 1,] <- list(Event[i], Season[i], paste(Time[i]), paste(Meet[i]), paste(Date[i]))
  }
  return(tffrs_df)
}

initialize_tffrs <- function(webpage){
  event_history <- html_nodes(webpage,"#event-history")
  tbl <- html_table(html_nodes(event_history,"table"), fill = TRUE)
  event_names <- c("60 Meters","60 Hurdles","100 Meters","110 Hurdles", "200 Meters", "400 Meters", "500 Meters", "600 Meters",
                   "800 Meters", "1000 Meters","1500 Meters", "Mile", "3000 Meters", "3000 Steeplechase", "5000 Meters", "10,000 Meters")
  
  tffrs_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Event","Season", "Time", "Meet","Date"))
  tffrs_df_new <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Event","Season", "Time", "Meet","Date"))
  
  for (i in 1:length(tbl)){
    temp = data.table::rbindlist(tbl[i])
    df<- as.data.frame(temp)
    df_name <- names(df[1])
    
    if (grepl("Relay",df_name,fixed=TRUE)) {
      break
    } else if (grepl("k",df_name,fixed=TRUE)) {
      break
    }
    
    tffrs_df = generate_tffrs_df(df,df_name,event_names,tffrs_df)
  }
  return(tffrs_df)  
}

clean_up_df <-  function(tffrs_df){
  
  tffrs_df[tffrs_df$Time != "DNF", ]  
  tffrs_df[tffrs_df$Time != "DNS", ]  
  
  tffrs_df$Time<-ms(tffrs_df$Time)
  tffrs_df$Time<-as.numeric(tffrs_df$Time)
  
  months = list()
  days = list()
  years = list()
  
  for (i in 1:length(tffrs_df$Date)) {
    days = paste(append(days,stri_extract(tffrs_df$Date[[i]], regex = "\\d+")))
    years = paste(append(years,stri_extract(tffrs_df$Date[[i]], regex = "\\d{4}")))
    months = paste(append(months,stri_extract(tffrs_df$Date[[i]], regex = "\\w*")))
    tffrs_df$Date[i]<- paste(months[i],days[i],years[i],sep=" ")
  }
  
  tffrs_df$Day <- days
  tffrs_df$Month <- months
  tffrs_df$Year <- years
  tffrs_df$Date <- mdy(tffrs_df$Date)
  return(tffrs_df)
}

split_tffrs<-function(tffrs_df){
  dfs<- split(tffrs_df,tffrs_df$Event)
  return(dfs)
}

make_boxplots <- function(dfs) {
  my_boxplots = list()
  
  for (i in 1:length(dfs)){
    my_boxplots[[i]] <- ggplot(dfs[[i]], aes(x=Year, y=Time)) + 
      geom_boxplot() +
      ggtitle(paste(dfs[[i]]$Event)) +
      ylab("Time (seconds)")
  }
  return(my_boxplots)
} 

make_violinplots <- function(dfs) {
  my_violinplots = list()
  
  for (i in 1:length(dfs)){
    my_violinplots[[i]]<- ggplot(dfs[[i]], aes(x=Year, y=Time)) + 
      geom_violin() +
      geom_boxplot(width=0.03)+
      ggtitle(paste(dfs[[i]]$Event)) +
      ylab("Time (seconds)")
  }
  return(my_violinplots)
}

sec_to_min <- function(sec) {
  mins = sec%/%60
  remainder = sec%%60
  print(round(remainder))
  if (stri_length(floor(remainder))==1) {
    time_ms = paste(toString(mins),":0",toString(round(remainder,2)),sep="")
    return(time_ms)
  } else {
    time_ms = paste(toString(mins),":",toString(round(remainder,2)),sep="")
    return(time_ms)
  }
}

aggregate_table <- function(df) {
  final_stats_df = list()
  for (i in 1:length(df)) {
    mean_df = aggregate(Time~Year + Month,data=df[[i]],mean)
    std_df = aggregate(Time~Year + Month,data=df[[i]],sd)
    
    stats_df <- mean_df %>% 
      rename("Mean" = "Time")
    stats_df$Mean <-paste(lapply(stats_df$Mean,sec_to_min))
    stats_df$sd_seconds<-round(std_df$Time,2)
    final_stats_df[[i]]<-stats_df
  }
  return(final_stats_df)
  
}

# Define UI ----
ui <- fluidPage(
  titlePanel("College Track Analytics Dashboard"),
  navbarPage("CTAD",
             tabPanel("Athlete Analysis",
                      fluidRow(
                        column(12, # column width goes from 1 to 12 where 12 is the whole screen
                               sidebarPanel( # default 4 / 12 ... in this case 3/4 of 8
                                 titlePanel("Input athlete name in format seen below:"),
                                 textInput("url", "Search Athlete", value = "Aiden Smyth Stony Brook TFFRS"),
                                 actionButton("goButton", "Go")
                               )
                        )
                      ),
                      sidebarPanel(width = 6,align = "leftAlign",
                                DTOutput("TABLE"),
                                uiOutput('df_pick'),
                                DTOutput("Table2")
                      ),
                      sidebarPanel(width = 6, align = "rightAlign",
                                plotOutput("plot1"),
                                plotOutput("plot2")
                      )
                      ),
             tabPanel("Compare Athletes"),
             tabPanel("Boston U Indoor Track Analysis Paper",
                      htmltools::includeMarkdown("C:/myapp/bu_mile.Rmd")
                      ),
             tabPanel("Boston U Indoor Track Analysis Visualization Tool"),
             tabPanel("Info",
                      htmltools::includeMarkdown("C:/myapp/information.Rmd"))
  ),
  
)

# Define server logic ----
server <- function(input, output) {
  
  table.data <- eventReactive(input$goButton,{
    link <- get_first_google_link({input$url})
    webpage <- read_html(link)
    tffrs_df <- initialize_tffrs(webpage)
    tffrs_df <- clean_up_df(tffrs_df)
    return(tffrs_df)
  })
  
  output[["TABLE"]] <- renderDataTable(table.data()) 
  
  table.data2 <- eventReactive(input[["df_pick"]],{
    tffrs_df = table.data()
    events = sort(unique(tffrs_df$Event))
    dfs <- split_tffrs(table.data())
    final_stats_df <- aggregate_table(dfs)
    print(typeof(final_stats_df[match(input[["df_pick"]],events)]))
    return(final_stats_df[[match(input[["df_pick"]],events)]])
  })
    
  output[["Table2"]]<- renderDataTable(table.data2())
  
  output[["df_pick"]] <- renderUI({
    tffrs_df = table.data()
    events = sort(unique(tffrs_df$Event))
    selectInput("df_pick",
                label = "Select Event",
                choices = events) 
  })
    
  output[["plot1"]] <- renderPlot({
    tffrs_df = table.data()
    events = sort(unique(tffrs_df$Event))
    dfs <- split_tffrs(table.data())
    my_boxplots <- make_boxplots(dfs)
    
    return(my_boxplots[match(input[["df_pick"]],events)])
  
    })
  
  output[["plot2"]] <- renderPlot({
    tffrs_df = table.data()
    events = sort(unique(tffrs_df$Event))
    dfs <- split_tffrs(table.data())
    my_violinplots <- make_violinplots(dfs)
    
    return(my_violinplots[match(input[["df_pick"]],events)])
    
  })


}

# Run the app ----
shinyApp(ui = ui, server = server)

