#setwd("~/desktop/data_viz/hw2")
library(shiny)
library(reshape2)
library(ggplot2)

pop <- read.csv('data/API_SP.POP.TOTL_DS2_en_csv_v2.csv', header = T)
fert <- read.csv('data/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = T, skip = 4)
life <- read.csv('data/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', header = T, skip = 4)
reg <- read.csv('data/Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = T)
reg <- reg[,c(1,2)]
pop <- pop[,-c(3,4,60,61,62)]
fert <- fert[,-c(3,4,60,61,62)]
life <- life[,-c(3,4,60,61,62)]

mpop <- melt(pop, id = c("Country.Name","Country.Code"))
mfert <- melt(fert, id = c("Country.Name","Country.Code"))
mlife <- melt(life, id = c("Country.Name","Country.Code"))

all <- merge(mpop,mfert, by = c('Country.Name', 'Country.Code', 'variable'), all.x = TRUE)
colnames(all) <- c('Country.Name', 'Country.Code', 'variable', 'Population', 'Fertility')
all <- merge(all, mlife, by = c('Country.Name', 'Country.Code', 'variable'), all.x = TRUE)
colnames(all) <- c('Country.Name', 'Country.Code', 'Year', 'Population', 'Fertility', 'Life.Exp')
all <- merge(all, reg, by = 'Country.Code')
all[all==""] <- NA
all <- na.omit(all)
all$Year <- gsub("X", "", all$Year)
all$Year <- as.numeric(all$Year)
theme_main <- theme(legend.text=element_text(size=11),
                    axis.text=element_text(size=14),
                    axis.title=element_text(size=15), 
                    legend.position = c(.18,.2), 
                    legend.title=element_blank(), 
                    legend.background = element_rect('transparent'))

ui <- fluidPage(
  headerPanel('Life Expectancy vs Fertility by Region'),
  fluidRow(
    column(8,plotOutput('plot1', hover = "plot_hover"), uiOutput("hover_info")),
    column(3, checkboxGroupInput(inputId = 'region', label = h5('Regions'), choices = unique(sort(all$Region)))),
    column(3, sliderInput("popweight", label = h5("Population Weight"), ticks = F, width = 150,  min = .25, max = .5, value = .5)),
    column(6,offset = 1, sliderInput("year", label = h5("Year"), width = 1500, min = 1960, max = 2014, value = 1960, animate = animationOptions(interval = 400),sep = "")))
)

server <- function(input, output) {
  
  regionData <- reactive({
    if (is.null(input$region)){
      return (NULL)
    }
    else{
      return (input$region) 
    }})
  
  popWt <- reactive({input$popweight})
  yearData <- reactive({input$year})
  
  output$plot1 <- renderPlot({
    if (is.null(regionData())){
        ggplot() + 
        theme_main +
        labs(x = 'Life Expectancy') +
        geom_point(data = subset(all, Year == yearData()), aes(x = Life.Exp, y = Fertility, size = Population, fill = Region),colour="black", pch=21) +
        xlim(10,90) + 
        ylim(0,10) + 
        scale_size(range = c(10,100)*popWt(), guide = 'none') +
        scale_fill_manual(values=c('#984ea3','#1f78b4','#ff7f00','#66c2a5','#fb9a99','#e31a1c','#ffff33')) +
        guides(fill = guide_legend(override.aes = list(size=5)))
    }
    else{
        ggplot() + 
        theme_main +
        labs(x = 'Life Expectancy') +
        geom_point(data = subset(all, Region %in% regionData() & Year == yearData()), aes(x = Life.Exp, y = Fertility, size = Population, fill = Region),colour="black", pch=21) +
        xlim(0,90) +
        ylim(0,10) + 
        scale_size(range = c(10,100) * popWt(), guide = 'none') + 
        geom_point(data = subset(all, !Region %in% regionData() & Year == yearData()),alpha = .3, aes(x = Life.Exp, y = Fertility, size = Population, fill = Region),colour="black", pch=21) +
        scale_fill_manual(values=c('#984ea3','#1f78b4','#ff7f00','#66c2a5','#fb9a99','#e31a1c','#ffff33')) +
        guides(fill = guide_legend(override.aes = list(size=5)))
    }
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(all, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country.Name,
                    "<b><br> nlife_expectancy: </b>", point$Life.Exp,
                    "<b><br> fertility_rate: </b>", point$Fertility)))
    )
  })
}

shinyApp(ui = ui, server = server)


