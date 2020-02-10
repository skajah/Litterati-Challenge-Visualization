library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(dplyr)
library(comprehenr)
library(hashmap)
library(leaflet)
library(readr)
library(DT)

data = read.table(file = "litterati challenge-65.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

data$litterTimestamp = parse_date_time(data$litterTimestamp, "Ymd HMS", tz = 'America/Chicago')

users = unique(data$username)
unnamed_users = users[grepl('litterati', users, fixed=TRUE)]
assigned_names = to_vec(for(index in 1:length(unnamed_users)) paste("user", as.character(index), sep=''))
old_new_name = hashmap(unnamed_users, assigned_names)

data$username = to_vec(for(user in data$username) if (old_new_name$has_key(user)) old_new_name[[user]] else user)


lat_outliers = boxplot(data$lat, plot=FALSE)$out
data = data[-which(data$lat %in% lat_outliers),]

lon_outliers = boxplot(data$lon, plot=FALSE)$out
data = data[-which(data$lon %in% lon_outliers),]


data$tags[data$tags==''] = "untagged"
data$tags = strsplit(data$tags, ",")
ALL_TAGS = unlist(data$tags, recursive = TRUE)
ALL_UNIQUE_TAGS = unique(ALL_TAGS)
ALL_SINGLE_TAGS = ALL_TAGS[-Vectorize(function(tag) grepl(' ', tag, fixed=TRUE))(ALL_TAGS)]
ALL_UNIQUE_SINGLE_TAGS = unique(ALL_SINGLE_TAGS)

DAYS = c("Monday", "Tuesday", "Wednesday", 
         "Thursday", "Friday", "Saturday", "Sunday")

blueish = "#0072B2" 
brownish = "#D55E00"
pinkish = "#CC79A7"

data = data %>% mutate(day=weekdays(data$litterTimestamp)) %>%
  mutate(hour=hour(data$litterTimestamp)) %>%
  mutate(date=as.Date(data$litterTimestamp))

pickers = data %>% group_by(username) 
top10PickersDF = (pickers %>% tally() %>% arrange(desc(n)))
top10Pickers = to_vec(for (row in 1:nrow(top10PickersDF)) paste(top10PickersDF[row, 1], top10PickersDF[row, 2], sep=" => "))

tags = data.frame(tag=ALL_TAGS, stringsAsFactors = FALSE) %>% group_by(tag) 
top10TagsDF = (tags %>% tally() %>% arrange(desc(n)))
top10Tags = top10TagsDF$tag

tag_in_tags = function(data, tag) {
  to_vec(for(tag_list in data$tags) tag %in% tag_list)
}

count_tags = function(data){
  counts = hashmap(ALL_UNIQUE_TAGS, integer(length(ALL_UNIQUE_TAGS)))
  for(tag_list in data$tags) {
    for(tag in tag_list){
      counts[[tag]] = counts[[tag]] + 1
    }
  }
  data.frame(tag=counts$keys(), count=counts$values())
}

am_pm = to_vec(for(hour in 0:23) if (hour == 0) "12 AM" else if (hour < 12) paste(hour, " AM") else if (hour == 12) "12 PM" else paste(hour - 12, " PM"))
am_pm_map = hashmap(0:23, am_pm)

convert_to_am_pm = function(data){
  data$hour = to_vec(for(hour in as.integer(data$hour)) am_pm_map[[hour]])
  data$hour = factor(data$hour, levels=am_pm, ordered=TRUE)
  data
}

get_time_data = function(data, time_of_day){
  if (time_of_day == "Morning"){
    subset(data, hour >=6 & hour < 12)
  } else if (time_of_day == "Afternoon"){
    subset(data, hour >= 12 & hour < 17)
  } else if (time_of_day == "Evening"){
    subset(data, hour >= 17 & hour < 22)
  } else if (time_of_day == "Night") {
    subset(data, hour >= 22 | hour < 6)
  } else {
    data
  }
}

data$day = factor(data$day, levels=DAYS, ordered=TRUE)

byHour = data %>% group_by(hour)
byHour$hour = factor(byHour$hour, levels=0:23, ordered=TRUE)

data2019 = data[year(data$date) == 2019,]


about_page = read_file("about.html")

trash_icon <- makeIcon(
  iconUrl = "trash_can.jpg",
  iconWidth = 16, iconHeight = 30,
  iconAnchorX = 10, iconAnchorY = 20
)

map_object = leaflet()
map_styles_string = c("Open Street", "National Geographic", "Positron")
map_styles = hashmap(map_styles_string, c(providers$OpenStreetMap.HOT, providers$Esri.NatGeoWorldMap, providers$CartoDB.Positron))

timeOfDay = c("Morning", "Afternoon", "Evening", "Night")

ui = dashboardPage(
  
  dashboardHeader(title = "Litter Picked in Forest Park", titleWidth=450),
  
  dashboardSidebar(
    box(width=NULL, height=NULL,
      box(width=NULL, height=NULL, title="Total Litter"
      ),
      box(
        width=NULL, height=NULL, title=paste(nrow(data), " Pieces")
      )
    ),
    sidebarMenu(
      menuItem("Plot View", tabName = "plotView", icon=icon("chart-bar")),
      menuItem("Table View", tabName="tableView", icon=icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    fluidRow(
      column(12,
             selectInput(inputId="top10Pickers",
                         label="Top Pickers",
                         choices= c("", top10Pickers)),
             selectInput(inputId="top10Tags",
                         label="Top Tags",
                         choices=c("", top10Tags)),
             selectInput(inputId="timeOfDay", 
                         label="Time of Day",
                         choices=c("",timeOfDay)),
             selectInput(inputId="mapStyle",
                         label="Map Sytle",
                         choices=map_styles_string)
             )
    ) # end fluid row 
  ), # end side bar
  
  dashboardBody(
    tabItems(
      tabItem(tabName="plotView",
        fluidRow(
          column(6, plotOutput(outputId="litterByDay")),
          column(6, plotOutput(outputId="litterByTag")),
          column(6, leafletOutput(outputId="map")),
          column(6, plotOutput(outputId="litterByHour")),
          column(12, plotOutput(outputId="litterOverYear"))
        ) # end fluidRow
      ), # end tab 1
      tabItem(tabName="tableView", 
        fluidRow(
          column(4, 
                 box(
                   title = "Litter by Day", height=NULL, width=NULL, status="info",
                   div(DT::dataTableOutput('litterByDayTable'))
                 )),
          column(4, 
                 box(
                   title="Litter by Tag", height=NULL, width=NULL, status="info",
                   div(style="overflow-y: scroll", DT::dataTableOutput('litterByTagTable'))
                 )),
          column(4, 
                 box(
                   title="Litter by Hour", height=NULL, width=NULL, status="info",
                   div(style="overflow-y: scroll", DT::dataTableOutput('litterByHourTable'))
                 )),
          column(6, offset=2, leafletOutput(outputId="map2")),
          column(12, 
                 box(
                   title = "Litter by Date", width = NULL, status = "primary",
                   div(style = 'overflow-x: scroll', DT::dataTableOutput('litterOverYearTable'))
                 ))
        ) # end fluidRow
      ), # end tab 2
    tabItem(tabName="about",
      HTML(about_page)
      ) # end tab 3
    ) # end tab items
  ) # end dashboard boday
) # end dashboard page


server = function(input, output){
  

  last_user = reactiveVal('')
  last_tag = reactiveVal('')
  last_time = reactiveVal('')
  user_on = reactiveVal(F)
  tag_on = reactiveVal(F)
  time_on = reactiveVal(F)
  
  which_on = eventReactive(c(input$top10Pickers, input$top10Tags, input$timeOfDay), {
    user = input$top10Pickers
    user_on(if (user != last_user()) TRUE else FALSE)
    tag = input$top10Tags
    tag_on(if (tag != last_tag()) TRUE else FALSE)
    tme = input$timeOfDay
    time_on(if (tme != last_time()) TRUE else FALSE)
    
    #user_on()
    #tag_on()
    #time_on()
    last_tag(tag)
    last_user(user)
    last_time(tme)
  })
  
  ############################################# PLOT OUTPUT START ########################################
  ############################################# PLOT OUTPUT START ########################################
  output$litterByDay = renderPlot({
    which_on()
    if (user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data[data$username == picker,] 
      picker_data$day = factor(picker_data$day, levels=DAYS, ordered=TRUE)
      picker_data = picker_data %>% mutate(user=picker)
      total_data = data %>% mutate(user="Total")
      
      grouped_data = rbind(total_data, picker_data) %>% group_by(user, day) %>% summarise(count=n()) 
      
      
      ggplot(data=grouped_data, aes(day, count, fill=user)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Day") + ylab("Pieces") +
        labs(title=paste("Daily Litter Picked by ", picker)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(brownish, blueish))
    } else if (tag_on()) {
      tag = input$top10Tags
      tag_data = data[tag_in_tags(data, tag), ] 
      tag_data = cbind(tag_data, tag_total=tag)
      tag_data$day = factor(tag_data$day, levels=DAYS, ordered=TRUE) 
      
      total_tag_data = cbind(data, tag_total="Total")
    
      grouped_data = rbind(total_tag_data, tag_data) %>% group_by(tag_total, day) %>% summarise(count=n()) 
      
      ggplot(data=grouped_data, aes(day, count, fill=tag_total)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Day") + ylab("Pieces") +
        labs(title=paste("Daily Litter Picked: ", tag)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(blueish, brownish))
    } else if (time_on()){
      the_time = input$timeOfDay
      time_data = (get_time_data(data, the_time) %>% mutate(time=the_time))[, c("day", "time")]
      total_data = (data %>% mutate(time="All Day"))[, c("day", "time")]
      
  
      grouped_data = rbind(total_data, time_data) %>% group_by(time, day) %>% summarise(count=n()) 
      
      
      ggplot(data=grouped_data, aes(day, count, fill=time)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Day") + ylab("Pieces") +
        labs(title="Daily Litter Picked") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(blueish, brownish))
    } else {
        byDay = data %>% group_by(day) %>% tally() %>% arrange(day)
      ggplot(byDay, aes(day, n)) + 
        geom_bar(stat="identity", fill=blueish) + xlab("Day") + ylab("Pieces") + 
        labs(title="Daily Litter Picked") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1))
      }
  })
  
  output$litterByTag = renderPlot({
    if(user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data[data$username == picker,]
      tag_freq = count_tags(picker_data) %>% filter(count > 0) %>% arrange(desc(count))
      tag_freq = cbind(tag_freq, top_tag=picker)
      if (nrow(tag_freq) > 10){
        tag_freq = tag_freq[1:10,]
      }
      
      total_tag_freq = count_tags(data)
      total_tag_freq = total_tag_freq[total_tag_freq$tag %in% tag_freq$tag,] %>% mutate(top_tag="Total")
      
      
      grouped_data = rbind(tag_freq, total_tag_freq) %>% group_by(tag, top_tag)
      
      ggplot(data=grouped_data , aes(tag, count, fill=top_tag)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5)) + xlab("Tag") + ylab("Pieces") + 
        labs(title=paste("Top Litter Types Picked by ", picker)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(brownish, blueish))
    }
    else if (tag_on()){
      tag = input$top10Tags
      tag_match = tag_in_tags(data, tag)
      tag_data = data[tag_match,] 
      tag_freq = count_tags(tag_data) 
      tag_freq = tag_freq[tag_freq$tag == tag,] %>% mutate(tag_type=tag)
      

      total_tag_freq = top10TagsDF[1:10,] 
      colnames(total_tag_freq) = c("tag", "count")
      total_tag_freq = total_tag_freq %>% mutate(tag_type="Top 10")
      
      grouped_data = rbind(tag_freq, total_tag_freq) %>% group_by(tag_type)
      
      # print(grouped_data)
      
      ggplot(data=grouped_data, aes(tag, count, fill=tag_type)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5)) + xlab("Tag") + ylab("Pieces") + 
        labs(title=paste("Litter Picked: ", input$top10Tags)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(brownish, blueish))
        
    } else if (time_on()) {
      the_time = input$timeOfDay
      time_data = get_time_data(data, the_time)
      tag_freq = count_tags(time_data) %>% filter(count > 0) %>% arrange(desc(count)) %>% 
        mutate(time=the_time)
      if (nrow(tag_freq) > 10){
        tag_freq = tag_freq[1:10,]
      }
      
      total_tag_freq = count_tags(data)
      
      if (nrow(tag_freq) == 0){
        total_tag_freq = (total_tag_freq %>% arrange(desc(count)))[1:10,] %>% mutate(time="All Day")
        for(row in 1:nrow(total_tag_freq)){
          tag_freq = rbind(tag_freq, data.frame(tag=total_tag_freq$tag[row], count=0, time=the_time))
        }
        tag_freq$count = as.integer(tag_freq$count)
      } else {
        total_tag_freq = total_tag_freq[total_tag_freq$tag %in% tag_freq$tag,] %>% mutate(time="All Day")
      }
      
      grouped_data = rbind(tag_freq, total_tag_freq) %>% group_by(tag, time)
      
      ggplot(data=grouped_data , aes(tag, count, fill=time)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5)) + xlab("Tag") + ylab("Pieces") + 
        labs(title="Top Litter Types Picked") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(brownish, blueish))
      
    } else {
      ggplot(top10TagsDF[1:10,] %>% arrange(desc(n)), aes(tag, n)) + 
        geom_bar(stat="identity", fill=blueish) + xlab("Tag") + ylab("Pieces") + 
        labs(title="Top Types of Litter Picked") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$litterByHour = renderPlot({
    if (user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data %>% filter(username == picker) %>% mutate(user=picker)
      total_data = data %>% mutate(user="Total")
      
      grouped_data = convert_to_am_pm(rbind(total_data, picker_data)) %>% group_by(user, hour) %>% summarise(count=n()) 
      
      ggplot(data=grouped_data, aes(hour, count, fill=user)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Hour") + ylab("Pieces") +
        labs(title=paste("Hourly Litter Picked by ", picker)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = -90, hjust = 1)) +
        scale_fill_manual(values=c(brownish, blueish))
    } else if (tag_on()) {
      tag = input$top10Tags
      tag_match = tag_in_tags(data, tag)
      tag_data = data[tag_match,]  %>% mutate(tag_total=tag)
      total_tag_data = cbind(data, tag_total="Total")
      
      grouped_data = convert_to_am_pm(rbind(total_tag_data, tag_data)) %>% group_by(tag_total, hour) %>% summarise(count=n()) 
      
      ggplot(data=grouped_data, aes(hour, count, fill=tag_total)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Hour") + ylab("Pieces") +
        labs(title=paste("Hourly Litter Picked: ", tag)) +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = -90, hjust = 1)) +
        scale_fill_manual(values=c(blueish, brownish))
    } else if (time_on()){
      the_time = input$timeOfDay
      time_data = (get_time_data(data, the_time) %>% mutate(time=the_time))[, c("hour", "time")]
      total_data = (data %>% mutate(time="All Day"))[, c("hour", "time")]
      
      
      grouped_data = convert_to_am_pm(rbind(total_data, time_data)) %>% group_by(time, hour) %>% summarise(count=n()) 
      
      ggplot(data=grouped_data, aes(hour, count, fill=time)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Hour") + ylab("Pieces") +
        labs(title="Hourly Litter Picked") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = -90, hjust = 1)) +
        scale_fill_manual(values=c(blueish, brownish))
    }
    else{
      ggplot(convert_to_am_pm(byHour %>% tally() %>% arrange(hour)), aes(hour, n)) + 
      geom_bar(stat="identity", fill=blueish) + xlab("Hour") + ylab("Pieces") + 
      labs(title="Hourly Litter Picked") +
      theme(text = element_text(size=20), 
            axis.text.x = element_text(angle = -90, hjust = 1))
    }
  })
  
  output$litterOverYear = renderPlot({
    if (user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data2019 %>% filter(username == picker) %>% mutate(user=picker)
      total_data = data %>% mutate(user="Total")
      
      grouped_data = rbind(total_data, picker_data) %>% group_by(user, date) %>% summarise(count=n())
      
      ggplot(data=grouped_data, aes(date, count, fill=user)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5)) + xlab("Date") + ylab("Pieces") + 
        labs(title=paste("Litter Picked (2019) by ", picker)) + 
        theme(text = element_text(size=20)) + 
        scale_fill_manual(values=c(brownish, blueish))
    } else if (tag_on()){
      tag = input$top10Tags
      tag_match = tag_in_tags(data2019, tag)
      tag_data = data2019[tag_match,] %>% mutate(tag_total=tag)
      total_tag_data = data2019 %>% mutate(tag_total="Total")
      
      grouped_data =rbind(total_tag_data, tag_data) %>% group_by(tag_total, date) %>% summarise(count=n()) 
      
      ggplot(data=grouped_data, aes(date, count, fill=tag_total)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5)) + xlab("Date") + ylab("Pieces") + 
        labs(title=paste("Litter Picked (2019): ", tag)) + 
        theme(text = element_text(size=20)) + 
        scale_fill_manual(values=c(brownish, blueish))
    } else if (time_on()){
      the_time = input$timeOfDay
      time_data = get_time_data(data, the_time) %>% mutate(time=the_time)
      total_data = data %>% mutate(time="All Day")
      
      grouped_data = rbind(total_data, time_data) %>% group_by(time, date) %>% summarise(count=n())
      
      ggplot(data=grouped_data, aes(date, count, fill=time)) + 
        geom_bar(stat="identity", position=position_dodge(width=0.5))  +
        xlab("Date") + ylab("Pieces") +
        labs(title="Litter Picked (2019)") +
        theme(text = element_text(size=20), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c(blueish, brownish))
      
    }
    else {
      ggplot(data2019 %>% group_by(date) %>% tally() %>% arrange(date), aes(date, n)) + 
        geom_bar(stat="identity", fill=blueish) + xlab("Date") + ylab("Pieces") + 
        labs(title="Litter Picked (2019)") + 
        theme(text = element_text(size=20))
    }
  })
  
  ############################################# TABLE OUTPUT START ########################################
  ############################################# TABLE OUTPUT START ########################################
  dayTableHeader = c("Day", "Pieces")
  tagTableHeader = c("Tag", "Pieces")
  hourTableHeader = c("Hour", "Pieces")
  yearTableHeader = c("Date", "Pieces")
  
  output$litterByDayTable = DT::renderDataTable({
    which_on()
    DT::datatable(
      if (user_on()){
        picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
        picker_data = data[data$username == picker,] 
        picker_data$day = factor(picker_data$day, levels=DAYS, ordered=TRUE)
        picker_data = picker_data %>% mutate(user=picker)
        total_data = data %>% mutate(user="Total")
        grouped_data = rbind(total_data, picker_data) %>% group_by(user, day) %>% summarise(count=n())
        
        table_data_left = (grouped_data %>% filter(user == picker))[, c("day", "count")]
        table_data_right = (grouped_data %>% filter(user == "Total"))[, c("day", "count")]
        
        result_table = cbind(table_data_left, table_data_right[table_data_right$day %in% table_data_left$day,][,"count"])
        colnames(result_table) = c("Day", picker, "Total")
        
        result_table
        
      } else if (tag_on()) {
        tag = input$top10Tags
        tag_data = data[tag_in_tags(data, tag), ] 
        tag_data$day = factor(tag_data$day, levels=DAYS, ordered=TRUE) 
        tag_data = cbind(tag_data, tag_total=tag)
        total_tag_data = cbind(data, tag_total="Total")
        
        grouped_data = rbind(total_tag_data, tag_data) %>% group_by(tag_total, day) %>% summarise(count=n()) 
        
        table_data_left = (grouped_data %>% filter(tag_total == tag))[, c("day", "count")]
        table_data_right = (grouped_data %>% filter(tag_total == "Total"))[, c("day", "count")]
        
        result_table = cbind(table_data_left, table_data_right[table_data_right$day %in% table_data_left$day,][,"count"])
        colnames(result_table) = c("Day", tag, "Total")
        
        result_table
      } else if (time_on()){
        the_time = input$timeOfDay
        time_data = get_time_data(data, the_time) %>% mutate(time=the_time)
        total_data = data %>% mutate(time="All Day")
        
        grouped_data = rbind(total_data, time_data) %>% group_by(time, day) %>% summarise(count=n()) 
        
        right_data = (grouped_data %>% filter(time=="All Day"))[, c("day", "count")]
        left_data = (grouped_data %>% filter(time==the_time))[, c("day", "count")]
        if(nrow(left_data) == 0){
          for (row in 1:nrow(right_data)){
            left_data = rbind(left_data, data.frame(the_time=the_time, day=right_data$day[row], count=0))
          }
          left_data = left_data[, c("day", "count")] %>% mutate(count=as.integer(count))
        }
        
        result_table = cbind(left_data, right_data[right_data$day %in% left_data$day,][,"count"])
        colnames(result_table) = c("Day", the_time, "Total Count")
        
        result_table
        
      }
      else {
        byDay = data %>% group_by(day)  %>% tally() %>% arrange(day)
        colnames(byDay) = dayTableHeader
        byDay
      }, options(dom='t', paging=FALSE)
    )
    
  })
  
  output$litterByTagTable = DT::renderDataTable({
    DT::datatable(
      if(user_on()){
        picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
        picker_data = data[data$username == picker,]
        tag_freq = count_tags(picker_data) %>% arrange(desc(count)) %>% filter(count > 0) # %>% mutate(top_tag=picker)
        if (nrow(tag_freq) > 10){
          tag_freq = tag_freq[1:10,]
        }
        
        total_tag_freq = count_tags(data)
        
        total_tag_freq_unordered = total_tag_freq[total_tag_freq$tag %in% tag_freq$tag,] # %>% mutate(top_tag="Total")
        total_tag_freq = data.frame(tag=character(), count=integer())
        
        for(tag in tag_freq$tag){
          total_tag_freq = rbind(total_tag_freq, total_tag_freq_unordered[total_tag_freq_unordered$tag == tag,])
        }
        
        result_table = cbind(tag_freq, total_tag_freq[, "count"])
        colnames(result_table) = c("Tag", picker, "All Day")
        
        result_table 
        
      }
      else if (tag_on()){
        tag = input$top10Tags
        tag_match = tag_in_tags(data, tag)
        tag_data = data[tag_match,] 
        tag_freq = count_tags(tag_data) 
        tag_freq = tag_freq[tag_freq$tag == tag,] %>% mutate(tag_type="single")
        
        
        total_tag_freq = top10TagsDF[1:10,] 
        colnames(total_tag_freq) = c("tag", "count")
        total_tag_freq = total_tag_freq %>% mutate(tag_type="Top 10")
        
        grouped_data = rbind(tag_freq, total_tag_freq) %>% group_by(tag_type)
        
      
        grouped_data
      } else if (time_on()){
        the_time = input$timeOfDay
        time_data = get_time_data(data, the_time)
        tag_freq = count_tags(time_data) %>% filter(count > 0) %>% arrange(desc(count)) %>% 
          mutate(time=the_time)
        if (nrow(tag_freq) > 10){
          tag_freq = tag_freq[1:10,]
        }
        
        total_tag_freq = count_tags(data)
        
        if (nrow(tag_freq) == 0){
          total_tag_freq = (total_tag_freq %>% arrange(desc(count)))[1:10,] %>% mutate(time="All Day")
          for(row in 1:nrow(total_tag_freq)){
            tag_freq = rbind(tag_freq, data.frame(tag=total_tag_freq$tag[row], count=0, time=the_time))
          }
          tag_freq$count = as.integer(tag_freq$count)
        } else {
          total_tag_freq = total_tag_freq[total_tag_freq$tag %in% tag_freq$tag,] %>% mutate(time="All Day")
        }
        
        grouped_data = rbind(tag_freq, total_tag_freq) %>% group_by(tag, time)
        
        left_data = (grouped_data %>% filter(time == the_time))[, c("tag", "count")]
        right_data = (grouped_data %>% filter(time == "All Day"))[, "count"]
        
        result = cbind(left_data, right_data)
        colnames(result) = c("Tag", the_time, "All Day")
        result
        
      } else{
        tag_freq = top10TagsDF[1:10,]
        colnames(tag_freq) = tagTableHeader
        tag_freq
      }, options = list(paging=FALSE, searching=FALSE)
    )
  })
  
  output$litterByHourTable = DT::renderDataTable({
    DT::datatable(
      if (user_on()){
        picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
        picker_data = data %>% filter(username == picker)
        picker_data$hour = factor(picker_data$hour, levels=0:23, ordered=TRUE)
        picker_data = picker_data %>% mutate(user=picker)
        total_data = data %>% mutate(user="Total")
        total_data$hour = factor(total_data$hour, levels=0:23, ordered=TRUE)
        
        grouped_data = convert_to_am_pm(rbind(total_data, picker_data)) %>% group_by(user, hour) %>% summarise(count=n())
        
        table_data_left = (grouped_data %>% filter(user == picker))[, c("hour", "count")]
        table_data_right = (grouped_data %>% filter(user == "Total"))[, c("hour", "count")]
        
        result_table = cbind(table_data_left, table_data_right[table_data_right$hour %in% table_data_left$hour,][,"count"])
        colnames(result_table) = c("Hour", picker, "Total")
        
        result_table
      } else if (tag_on()) {
        tag = input$top10Tags
        tag_match = tag_in_tags(data, tag)
        tag_data = data[tag_match,]
        tag_data$hour = factor(tag_data$hour, levels=0:23, ordered=TRUE) 
        tag_data = cbind(tag_data, tag_total=tag)
        total_tag_data = cbind(data, tag_total="Total")
        
        grouped_data = convert_to_am_pm(rbind(total_tag_data, tag_data)) %>% group_by(tag_total, hour) %>% summarise(count=n()) 
        
        table_data_left = (grouped_data %>% filter(tag_total == tag))[, c("hour", "count")]
        table_data_right = (grouped_data %>% filter(tag_total == "Total"))[, c("hour", "count")]
        
        result_table = cbind(table_data_left, table_data_right[table_data_right$hour %in% table_data_left$hour,][,"count"])
        colnames(result_table) = c("Hour", tag, "Total")
        
        result_table %>% arrange(Hour)
        
      } else if(time_on()){
        the_time = input$timeOfDay
        time_data = get_time_data(data, the_time) %>% mutate(time=the_time)
        
        grouped_data =  convert_to_am_pm(rbind(data %>% mutate(time="All Day"), time_data)) %>% group_by(time, hour) %>% summarise(count=n())
        
        the_time_data = grouped_data %>% filter(time==the_time)
        result_table =  (grouped_data %>% filter(time=="All Day"))[, c("hour", "count")] %>% mutate(time_count=0)
        for (row in 1:nrow(result_table)){
          hour = result_table$hour[row]
          if (hour %in% the_time_data$hour){
            result_table[row, "time_count"] = the_time_data[the_time_data$hour==hour, "count"]
          } else {
            result_table[row, "time_count"] = 0
          }
        }
        
        result_table$time_count = as.integer(result_table$time_count)
        
        colnames(result_table) = c("Hour", "Total", the_time)
        
        result_table[,c("Hour", the_time, "Total")]
        
      } else{
        hour_data = convert_to_am_pm(byHour %>% tally() %>% arrange(hour))
        colnames(hour_data) = hourTableHeader
        hour_data
      }, options=list(scrollY=TRUE, searching=FALSE, paging=TRUE))
  })
  
  output$litterOverYearTable = DT::renderDataTable({
    DT::datatable( 
      if (user_on()){
        picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
        picker_data = data %>% filter(username == picker) 
        picker_data$date = as.Date(picker_data$date) 
      
        dates = sort(unique(picker_data$date))
        picker_data = picker_data %>% group_by(date) %>% tally() %>% filter(n > 0) %>% arrange(date)
        counts = picker_data$n
        hor_data = data.frame(matrix(ncol=length(dates), nrow=1))
        row.names(hor_data) = c(picker)
        colnames(hor_data) = dates
        hor_data[picker,] = counts
        
        all_data = data %>% filter(date %in% picker_data$date) %>% group_by(date) %>% tally() %>% arrange(date)
        dates2 = all_data$date
        counts2 = all_data$n 
        hor_data2 = data.frame(matrix(ncol=length(dates2), nrow=1))
        row.names(hor_data2) = c("Total")
        colnames(hor_data2) = dates2
        hor_data2["Total",] = counts2
        
        rbind(hor_data, hor_data2)
      } else if (tag_on()){
        tag = input$top10Tags
        tag_match = tag_in_tags(data, tag)
        tag_data = data[tag_match,] %>% group_by(date) %>% tally() %>% filter(n > 0) %>% mutate(date=as.Date(date)) %>% arrange(date)
        dates = tag_data$date
        counts = tag_data$n
        hor_data = data.frame(matrix(ncol=length(dates), nrow=1))
        row.names(hor_data) = c(tag)
        colnames(hor_data) = tag_data$date
        hor_data[tag,] = counts
        
        all_data = data %>% filter(date %in% tag_data$date) %>% group_by(date) %>% tally() %>% arrange(date)
        dates2 = all_data$date
        counts2 = all_data$n 
        hor_data2 = data.frame(matrix(ncol=length(dates2), nrow=1))
        row.names(hor_data2) = c("Total")
        colnames(hor_data2) = dates2
        hor_data2["Total",] = counts2
        
        rbind(hor_data, hor_data2)
        
      } else if (time_on()){
        the_time = input$timeOfDay
        time_data = get_time_data(data, the_time) %>% mutate(time=the_time)
        total_data = data %>% mutate(time="All Day") 
        
        grouped_data = rbind(time_data, total_data)  %>% group_by(time, date)  %>% summarise(count=n())
        
        result_table = grouped_data %>% filter(time=="All Day") %>% mutate(time_count=0)
        the_time_table = grouped_data %>% filter(time==the_time)
        
        for(row in 1:nrow(result_table)){
          date = result_table$date[row]
          if (date %in% the_time_table$date){
            result_table[row, "time_count"] = the_time_table[the_time_table$date == date, "count"]
          } else {
            result_table[row, "time_count"] = 0
          }
        }
        
        result_table$time_count = as.integer(result_table$time_count)
        
        horizontal_table = data.frame(matrix(nrow=2, ncol=nrow(result_table)))
        horizontal_table[1,] = result_table$time_count
        horizontal_table[2,] = result_table$count
        row.names(horizontal_table) = c(the_time, "All Day")
        colnames(horizontal_table) = as.Date(result_table$date)
        
        horizontal_table
      }
      else {
        year_data = data2019 %>% group_by(date) %>% tally() %>% filter(n > 0) %>% mutate(date=as.Date(date)) %>% arrange(date)
        dates = year_data$date
        counts = year_data$n
        hor_data = data.frame(matrix(ncol=length(dates), nrow=1))
        row.names(hor_data) = c("Pieces")
        colnames(hor_data) = year_data$date
        hor_data["Pieces",] = counts
        hor_data
      }
    , options = list(dom='t')) # end dt
  })
  
  ############################################# TABLE OUTPUT END ########################################
  ############################################# TABLE OUTPUT END ########################################
  
  output$map = renderLeaflet({
    if (user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data %>% filter(username == picker)
      map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(picker_data$lon), lat1=min(picker_data$lat), lng2=max(picker_data$lon), lat2=max(picker_data$lat)) %>% 
        addMarkers(lng=picker_data$lon, lat=picker_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    } else if (tag_on()){
      tag = input$top10Tags
      tag_match = tag_in_tags(data, tag)
      tag_data = data[tag_match,]
      map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(tag_data$lon), lat1=min(tag_data$lat), lng2=max(tag_data$lon), lat2=max(tag_data$lat)) %>% 
        addMarkers(lng=tag_data$lon, lat=tag_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    } else if (time_on()){
      the_time = input$timeOfDay
      time_data = get_time_data(data, the_time)
      if (nrow(time_data) == 0){
        map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
          fitBounds(lng1=min(data$lon), lat1=min(data$lat), lng2=max(data$lon), lat2=max(data$lat))
      } else {
        map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
          fitBounds(lng1=min(time_data$lon), lat1=min(time_data$lat), lng2=max(time_data$lon), lat2=max(time_data$lat)) %>% 
          addMarkers(lng=time_data$lon, lat=time_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
      }
    } else {
      map_object%>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(data$lon), lat1=min(data$lat), lng2=max(data$lon), lat2=max(data$lat)) %>%
        addMarkers(lng=data$lon, lat=data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    }
  })
  output$map2 = renderLeaflet({
    if (user_on()){
      picker = strsplit(input$top10Pickers, ' => ')[[1]][1]
      picker_data = data %>% filter(username == picker)
      map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(picker_data$lon), lat1=min(picker_data$lat), lng2=max(picker_data$lon), lat2=max(picker_data$lat)) %>% 
        addMarkers(lng=picker_data$lon, lat=picker_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    } else if (tag_on()){
      tag = input$top10Tags
      tag_match = tag_in_tags(data, tag)
      tag_data = data[tag_match,]
      map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(tag_data$lon), lat1=min(tag_data$lat), lng2=max(tag_data$lon), lat2=max(tag_data$lat)) %>% 
        addMarkers(lng=tag_data$lon, lat=tag_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    } else if (time_on()){
      the_time = input$timeOfDay
      time_data = get_time_data(data, the_time)
      if (nrow(time_data) == 0){
        map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
          fitBounds(lng1=min(data$lon), lat1=min(data$lat), lng2=max(data$lon), lat2=max(data$lat))
      } else {
        map_object %>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
          fitBounds(lng1=min(time_data$lon), lat1=min(time_data$lat), lng2=max(time_data$lon), lat2=max(time_data$lat)) %>% 
          addMarkers(lng=time_data$lon, lat=time_data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
      }
    } else {
      map_object%>% addProviderTiles(map_styles[[input$mapStyle]]) %>%
        fitBounds(lng1=min(data$lon), lat1=min(data$lat), lng2=max(data$lon), lat2=max(data$lat)) %>%
        addMarkers(lng=data$lon, lat=data$lat, clusterOptions=markerClusterOptions(), icon=trash_icon)
    }
  })
  
} # end server

shinyApp(ui=ui, server=server)







