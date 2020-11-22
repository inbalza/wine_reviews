#############################################################
#                                                           #
#               ShinyWiny Web Application                   #
#                                                           #
#  V1.0                                                     #
#  - Released: 15/1/2020                                    #
#  - Author: Inbal Zaltsman                                 #
#  - Description: Analysis of wine reviews dataset          #
#  https://www.kaggle.com/zynicide/wine-reviews             #
#  ``                                                       #                                
#############################################################

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(shinycssloaders)


# load data
wine_df_all <- read.csv(file="wine.csv",sep = ",", header = TRUE,  encoding = "UTF-8")

# subset data and remove duplicates and rows with NA or empty values 
selected_cols <- c("country","province","points","price","title","variety","winery")
wine_df <- wine_df_all[selected_cols]
wine_df <- unique(wine_df)
wine_df <- wine_df[!duplicated(wine_df["title"]),]
wine_df <- na.omit(wine_df)
wine_df <- subset(wine_df, country!="" & variety!="")


# calc wines points and price records 
wines_high_points <- head(wine_df[order(wine_df$points, decreasing = TRUE),c("title","points")], n=20)
wines_low_points <- head(wine_df[order(wine_df$points, decreasing = FALSE),c("title","points")], n=20)

wines_high_price <- head(wine_df[order(wine_df$price, decreasing = TRUE),c("title","price")], n=20)
wines_low_price <- head(wine_df[order(wine_df$price, decreasing = FALSE),c("title","price")], n=20)


# calculate total reviews, average score and average price for each country
country_df <- plyr::count(wine_df, "country")
country_df$avgScore <- round((aggregate(wine_df$points, list(wine_df$country), mean)[,2]), digits=2)
country_df$avgPrice <- round((aggregate(wine_df$price, list(wine_df$country), mean)[,2]), digits=2)

# calculate total reviews, average score and average price for each variety
variety_df <- plyr::count(wine_df,"variety")
variety_df$avgScore <- round((aggregate(wine_df$points, list(wine_df$variety), mean)[,2]), digits=2)
variety_df$avgPrice <- round((aggregate(wine_df$price, list(wine_df$variety), mean)[,2]), digits=2)

# calculate total reviews, average score and average price for each winery
winery_df <- plyr::count(wine_df,"winery")
winery_df$avgScore <- round((aggregate(wine_df$points, list(wine_df$winery), mean)[,2]), digits=2)
winery_df$avgPrice <- round((aggregate(wine_df$price, list(wine_df$winery), mean)[,2]), digits=2)


# subset french wines
france_provinces <- c("Alsace","Beaujolais","Bordeaux","Champagne","Burgundy",
                      "Languedoc-Roussillon","Loire Valley","Provence")
france_wines <- wine_df[wine_df$province %in% france_provinces,]
france_wines$province <- as.character(france_wines$province)
france_wines$province <- as.factor(france_wines$province)


############################################
# Text and options for the different pages #
############################################

# text for about page
aboutTxt <- tags$div("ShinyWiny Web App is based on reviews gathered over 100k different wines from all around the world.", tags$br(),tags$br(),
                     "For each wine the following information is provided: score, price, winery, grape variery and country of origin.", tags$br(),tags$br(),
                     "ShinyWiny allows you to explore the different parameters and the relationships between them. ", tags$br(), tags$br(),
                     tags$ul(tags$li("What is the highest rated wine? and the lowest?"),
                     tags$li("Which are the top wine producing countries and how will it look like on a map?"),
                     tags$li("What is the scores distribution? and the prices?"),
                     tags$li("Does higher price mean higher quality?"),
                     tags$li("Do wines made from popular grape varieties offer better value for money?")),tags$br(),
                     "Check out the different tabs to find answers to these questions!")

scoreTabTxt <- "View scores top or buttom records of different wines or records of average wine scores assiciated with different
                wineries, grape varieties or countries."
priceTabTxt <- "View prices top or buttom records of different wines or records of average wine prices assiciated with different
                wineries, grape varieties or countries."

mapTxt <- tags$div("Explore the difference in scores, prices and number of wines produced in each country in a map view.", tags$br(),
                   "Choose between average wine scores, average wine price or total wines.", tags$br(),
                   p("Try zooming in to a smaller area or moving over a country for more information!", style = "font-size:0.9em"))
distTxt <- tags$div("View a histogram of prices or scores.  Add density by selecting 'show density'.", 
                    h5("* price histogram: values above 1000$ were filtered out"))

priceScoreTxt <- tags$div(p(style = "font-size:0.9em",span("Choose between two interactive plots :",style = "font-weight:bold"), tags$br(),
                          span("Price vs Score", style = "font-weight: bold;"),
                          span("of all wine reviews will allow you to investigate the relationship between the two parameters.
                                 Which wines are pricey but are not highly rated? Which offer good value for money?
                                 Hover over the different points or zoom in to find out!"),tags$br(),
                          span("* To avoid long processing time a random sample of 10k wines is selected", style="font-size:0.85em"),tags$br(),
                          span("Score/Price vs Grape variety frequency", style = "font-weight:bold"),
                          span("displays the Score/Price ratio as a function of grape variety frequency.
                               This can be interpreted as value for money vs popularity of grape variety.
                               Is there a correlation between them? Are wines made from more popular varieties show higher
                               value for money or the opposite? A regression line is added to help with the assesment.")))

frenchyTxt <- tags$div("Explore the differences between major wine regions in France:",tags$br(), tags$br(),
                      span("Frequency", style = "font-weight:bold;"),"displays the total wines produced in each region", tags$br(),
                      span("Scores", style = "font-weight:bold;"),"displays a boxplot of wine scores for each region in one plot")

# options for records page
par_options <- c("Wines"= "Wine",
                 "Wineries" = "Winery",
                 "Grape Varieties" = "Grape Variety", 
                 "Countries" = "Country")

record_options <- c("Score" = "avgScore","Price ($)" = "avgPrice")

# options for map page
map_options <- c("Average Score" = "avgScore",
                 "Average Price" = "avgPrice",
                 "Total Wines" = "freq")

# text for map hover
country_df$hover <- with(country_df, paste(country, '<br>',"Total Wines:", freq, '<br>',
                                               "Average Score:", avgScore, '<br>', "Average Price:", avgPrice))

########################################
# define UI as a navigation bar format #
########################################

ui <-  tagList(
    
        # Define style to be used by all pages
        tags$style("
                            .text { background-color:#222222;
                                    border-radius:30px;
                                    color:white;
                                    padding:15px;}
                             hr,h3,h4,h5 { color:white;}
                            .navbar { font-size: 15px; }
                            .navbar-dropdown { font-size: 15px; }
                             #sidebar { background-color: #222222;
                                        border-radius:20px;
                                        padding:15px;
                                        border:none;
                                        color:white;}"),
        
        setBackgroundImage(src = "grapes.jpg"),
        
        navbarPage("ShinyWiny",theme = shinytheme("yeti"), selected = "About",

                 # About page
                 tabPanel("About",
                        div(h2("Welcome to ShinyWiny!"), class = "text", style = "padding-top:2px; padding-left:35px;" ), tags$br(), tags$br(),
                        div(h4(aboutTxt),class = "text", style = "padding:35px;")),
                 
                 # Records page 
                 navbarMenu("Records", 
                            tabPanel("Score", 
                                     div(h4(scoreTabTxt), class = "text"),tags$br(),
                                     sidebarLayout(
                                         sidebarPanel(id="sidebar",width = 3,
                                             selectInput(inputId = "recType1", label = h5("Type:"),
                                                         choices = c("Top","Bottom"), selected = 1),
                                             selectInput(inputId = "recNum1", label = h5("Number to display:"),
                                                         choices = c(1:20), selected = 5),
                                             selectInput(inputId = "par1",label = h5("Parameter to display:"),
                                                         choices = par_options, selected = 1)
                                         ),
                                         
                                         mainPanel(
                                             h3(textOutput("tableTitle"), style = "color:white;"),
                                             tableOutput("recordScore"),
                                             tags$head(tags$style("#recordScore table {background-color: lightblue; 
                                                          border-radius: 15px; font-size:15px;}",
                                                                  media="screen", type="text/css"))
                                         )
                                     )
                                    
                            ),
                            tabPanel("Price", 
                                     div(h4(priceTabTxt),class = "text"), tags$br(),
                                     sidebarLayout(
                                         sidebarPanel(id="sidebar",width = 3, 
                                                      selectInput(inputId = "recType2", label = h5("Type:"),
                                                                  choices = c("Top","Bottom"), selected = 1),
                                                      selectInput(inputId = "recNum2", label = h5("Number to display:"),
                                                                  choices = c(1:20), selected = 5),
                                                      selectInput(inputId = "par2",label = h5("Parameter to display:"),
                                                                  choices = par_options, selected = 1)
                                         ),
                                         
                                         mainPanel(
                                             h3(textOutput("BarPlotTitle"),style = "color:white;"),
                                             plotOutput("recordPrice", height = "500px")
                                         )
                                     )
                            )
                 ),
                 
                 # Map page 
                 tabPanel("Map",
                          div(h4(mapTxt),class = "text"), tags$br(),
                          sidebarLayout(
                             sidebarPanel(id="sidebar",width=3,
                                    selectInput(inputId = "mapOpt", label = h5("Select:"),
                                                choices = map_options, selected = 1)
                             ),
                             
                             mainPanel(
                                plotlyOutput("map")
                             )
                          )
                 ),
                 
                 # Distribution page 
                 tabPanel("Distribution",
                          div(h4(distTxt),class = "text"), tags$br(),
                          sidebarLayout(
                              sidebarPanel(id="sidebar",width = 3, 
                                           selectInput(inputId = "histOpt1", label = h5("Select:"), choices = c("Score","Price"), selected = 1),
                                           prettyCheckbox(inputId = "histOpt2", label = span("Show density line",style="font-size:1.2em"),
                                                          icon = icon("check"))
                              ),
                              
                              mainPanel(
                                  plotOutput("hist", height = "500px")
                              )
                          )
                       
                 ),
                 
                 # Score Vs Price page 
                 tabPanel("Price vs Score",
                          div(h4(priceScoreTxt),class = "text"),tags$br(),
                          sidebarLayout(
                              sidebarPanel(id="sidebar", width=3,
                                           selectInput(inputId = "priceScoreIn", label = h5("Select a plot to display:"),
                                                       choices = c("Price vs Score" = "prSc", 
                                                                   "Score/Price vs Grape variety frequency" = "var"), 
                                                                    selected = 1)
                                            
                                ),
                            mainPanel(
                                  withSpinner(plotlyOutput("priceScore"), type = "4", color = "white")
                             )
                          )
                 ),
                 
                 # Francy page
                 tabPanel("Frenchy",
                          div(h4(frenchyTxt), class = "text"), tags$br(),
                          sidebarLayout(
                              sidebarPanel(id="sidebar", width = 3,
                                           selectInput(inputId = "frenchy", label = h5("Select a plot to display:"),
                                                       choices = c("Frequency","Scores"),
                                                       selected = 1)
                            ), 
                            mainPanel(
                              plotOutput("frenchy", height = "500px")
                            )
                          )
                         )
                
        )
)

#################
# Define server #
#################

server <- function(input, output) {

    # Records page - display table title
    output$tableTitle <- renderText({
        
        if( input$recType1 == "Top") { 
            title <- "Highest Rated " 
        }
        else { 
            title <- "Lowest Rated "
        }

        paste0(title,names(par_options)[match(input$par1,par_options)])
    })
    
    # Records page - display bar plot title
    output$BarPlotTitle <- renderText({
        
        if( input$recType2 == "Top") { 
            title <- "Most Expensive " 
        }
        else { 
            title <- "Least Expensive  "
        }
        
        paste0(title,names(par_options)[match(input$par2,par_options)])
    })
    
    # Records page - display table
    output$recordScore <- renderTable({
        
        # Select and display relevant data according to the user's choice
        selectData(input$par1, 'avgScore', input$recType1, input$recNum1)
    }) 
    
    # Records page - display bar plot
    output$recordPrice <- renderPlot({
    
        # Select relevant data to display
        data <- selectData(input$par2, 'avgPrice',input$recType2, input$recNum2)
        
        if (input$recType2 == "Top"){
            data <- arrange(data, data[,2])
        } else {
            data <- arrange(data, desc(data[,2]))
        }
        data[,1] <- factor(data[,1] , levels = data[,1] )
        
        # Adjust X-axis label
        x_lab <- "Average Wine Price ($)"
        
        if (input$par2 == "Wine"){
            x_lab <- "Price ($)"
        }
    
        # Display a horizontal bar plot
        ggplot(data, aes(data[,1],data[,2])) + 
        geom_col(aes(fill = data[,2]), width = 0.7) + 
        geom_text(aes(label=data[,2]), position=position_dodge(width=0.9), hjust=1.5, colour = "white") + 
        coord_flip() +
        labs(fill = "Price ($)") +  xlab(input$par2) + ylab(x_lab) + 
            theme(axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16))
    })
    
    # Map page - display world map
    output$map <- renderPlotly({
        
        # Specify some map projection/options and colorbar color ranges
        g <- list(
            showframe = FALSE,
            showcoastlines = FALSE,
            projection = list(type = 'Natural earth')
        )
        colorscale <- c( "avgScore" = "Greens",
                         "avgPrice" = "YlGnBu",
                         "freq" = "YlOrRd")
        
        # Subset chosen data to display
        selectedData <- country_df[,input$mapOpt]
        
        # Plot map
        plot_ly(country_df, z = selectedData, locations = ~country, locationmode = 'country names',
                colors = colorscale[input$mapOpt], type = 'choropleth',hoverinfo = 'text', height = 500,hovertext = ~hover) %>%
            colorbar(title = names(map_options)[match(input$mapOpt,map_options)]) %>%
            layout( geo = list ) 
    })
    
    # Distribution page - display score or price histogram
    output$hist <- renderPlot({
        
        # define plot according to choice
        if(input$histOpt1 == 'Score'){
            
            g <- ggplot(wine_df, aes(x=points)) + 
                 geom_histogram(aes(y=..density..), breaks=seq(80,100,by=2),
                               fill=brewer.pal(10,"Spectral")) +
                xlab("Score") + ylab("Density") + 
                theme(axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 16))
        } 
        else {
            g <-  ggplot(wine_df, aes(x=price)) + 
                geom_histogram(aes(y=..density..), breaks=seq(0,200,by=20),
                               fill=brewer.pal(10,"Spectral"))+
                xlab("Price") + ylab("Density") + 
                theme(axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 16))
                xlim(c(0,1000))
        }
        
        # add density line if selected
        if(input$histOpt2){
            
           g <- g + geom_density(alpha=0.2, fill="#FF6666", adjust = 1.7) 
        }
        
        g
    })
    
    # Price/Score page - display scatter plot
    output$priceScore <- renderPlotly({
        
        # plot price vs score
        if (input$priceScoreIn == "prSc"){
            
            # select random rows to plot
            sample_wines <- sample_n(wine_df,10000)
            
            plot_ly( sample_wines, x = ~points, y = ~price, color = ~points, type = "scatter", mode = "markers", 
                     hoverinfo = 'text', height = 500,
                    text = ~paste(title,"<br>Price: ", price, '$<br>Score:', points)) %>%
                layout(xaxis = list(title="Score", titlefont = list(size=20)), 
                       yaxis = list(title="Price ($)",titlefont = list(size=20)),
                       showlegend = FALSE)
        }
        # plot score/price vs variety frequency with linear regression
        else if (input$priceScoreIn == "var"){
            
            fit <- lm((avgScore/avgPrice)~freq, data = variety_df)

            plot_ly( variety_df, x = ~freq, y = ~(avgScore/avgPrice), color = ~freq,type = "scatter", mode = "markers",
                     hoverinfo = "text", height = 500,
                     text = ~paste("Variety:",variety,"<br>Frequency:",freq,"<br>Score/Price:",round(avgScore/avgPrice,digits = 2))) %>%
                add_trace(data = variety_df, x = ~freq, y = fitted(fit), mode = 'lines') %>%
                colorbar(title = "Frequency") %>%
                layout(xaxis = list(title="Frequency of grape variety", titlefont = list(size=20)), 
                       yaxis = list(title="Score/Price",titlefont = list(size=20)),
                       showlegend = FALSE)
        }
        
    })
    
    # Francy page - display frequence bar plot or score box plot per province (=region)
    output$frenchy <- renderPlot({
        
        if (input$frenchy == "Frequency"){
            
            ggplot(france_wines,aes(france_wines$province))+
                geom_bar(fill= brewer.pal(8,"Set3")) + 
                geom_text(stat='count', aes(label=..count..), vjust=1.5, colour = "black") +
                xlab("Region") + ylab("Frequency") + 
                theme(axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 16))
        }
        else {
            ggplot(france_wines, aes(x=province,y=points)) +
                geom_boxplot(fill = brewer.pal(8,"Set3"), colour = "Gray") + 
                xlab("Region") + ylab("Score") + 
                theme(axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 16))
        }
    })
   
}

##########################################################
# Function for subseting data dynamically (records page) #
# Input: par1 = wine/country/winery/variety              #
#        par2 = score/price                              #
#        recType = top/buttom                            #
#        recNum = number of rows                         #
# Output: filtered data (dataframe)                      #
##########################################################
selectData <- function(par1,par2,recType,recNum) {
    if ( par1 == "Wine" ){
    
        if ( par2 == "avgScore" ) {
            if ( recType == 'Top' ) { 
                data <- wines_high_points 
            }
            else { 
                data <- wines_low_points
            }
        }
        else if (recType == 'Top') { 
            data <- wines_high_price 
        }
        else { 
            data <- wines_low_price
        }
        
        colnames(data) <- c(paste0(par1),
                            paste0(names(record_options)[match(par2,record_options)]))
    }
    else {
        if (par1 == "Winery") { 
            data <- winery_df[,c("winery",paste0(par2))] 
        }
        else if (par1 == "Grape Variety"){ 
            data <- variety_df[,c("variety",paste0(par2))] 
        }
        else { 
            data <- country_df[,c("country",paste0(par2))] 
        }
        
        if (recType == 'Top'){    
            data <- data[order(data[2],decreasing = TRUE),]
        }
        else{
            data <- data [order(data[2],decreasing = FALSE),]
        }
        
        colnames(data) <- c(paste0(par1),
                            paste0("Average ",names(record_options)[match(par2,record_options)]))
    }
    data <- head(data, n=as.numeric(recNum))
    return(data)
}

# Run the application 
shinyApp(ui = ui, server = server)

