#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
library(tidyverse)
library(reshape2)
GPP <- readRDS("D:/PHD/Course/STAT 433/project/GPP_app/data/GPP_data_all3.rds")

#source("helpers.R")#help you make choropleth maps


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("GPP Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Create GPP figures with 
        information from the Remote Sensing data, CMIP5 and CMIP6 
                     's models results."),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("IPSL_CMIP5",
                                    "CanESM_CMIP5",
                                    "GISS_CMIP5",
                                    "IPSL_CMIP6",
                                    "CanESM_CMIP6",
                                    "UKESM_CMIP6"),
                        selected = "IPSL_CMIP5"),
            
        ),
        
        mainPanel(
            tabsetPanel( 
                tabPanel("Acuracy evaluation", plotOutput("Acuracy_evaluation_plot")), 
                tabPanel("Seasonal pattern", plotOutput("Seasonal_pattern_plot")), 
                tabPanel("Annual GPP", plotOutput("Trend_analysis_plot")),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Seasonal_pattern_plot <- renderPlot({
        
        modelGpp <- switch(input$var, 
                           "GISS_CMIP5" = GPP$GISS_mean5,# 
                           "IPSL_CMIP5" = GPP$IPSL_mean5,
                           "CanESM_CMIP5" = GPP$CanESM_mean5,
                           "UKESM_CMIP6" = GPP$UKESM1_mean6,
                           "IPSL_CMIP6" = GPP$IPSL_mean6,
                           "CanESM_CMIP6" = GPP$CanESM_mean6)
        
        color <- switch(input$var, 
                        "GISS_CMIP5" = "#D9BACE",# 
                        "IPSL_CMIP5" = "#401F18",
                        "CanESM_CMIP5" = "#8C5549",
                        "UKESM_CMIP6" = "#D99D8F",
                        "IPSL_CMIP6" = "#025959",
                        "CanESM_CMIP6" = "#D95F5F")
        
        
        
        plotdata = data.frame(modelGpp,GPP$MODIS_mean,GPP$BESS_mean,GPP$VPM_mean,GPP$FLUXCOM_mean) %>%
            mutate(num = row_number()) %>%
            melt(id = 'num')
        
        plotdata %>%
            ggplot(mapping = aes(x = num, y = value, color = variable))+
            geom_line(size =0.5) +
            xlim(c(1,144)) +
            ylim(c(0,9))+
            scale_x_continuous(breaks=c(1,37,73,109,134),
                               labels=c("2003", "2006","2009", "2012","2014"))+
            theme_classic() +
            theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                  #axis.title.x=element_blank(),
                  axis.text.x = element_text(color="black",size=15, angle=0),
                  axis.text.y = element_text( color="black", size=15, angle=0),
                  axis.title.y = element_text( color="black",size=20),
                  axis.title.x = element_text( color="black",size=20),
                  axis.line.x= element_line(color="black",size = 0.5),
                  axis.line.y= element_line(color="black",size = 0.5),
                  #legend.position = c(0.15,0.9),
                  legend.direction = "vertical",
                  legend.title = element_blank(),
                  legend.text = element_text(size=20),
                  plot.margin = unit(c(1, 0, 0, 0), "cm")) +
            scale_colour_manual(
                values = c("#F24738","#F2AE30", "#8DBF5A",  "#9879D9", "#55A6D9"),
                labels = c("Model","MODIS", "BESS", "VPM", "FLUXCOM"))+
            labs(y="GPP (g m-2 d-1)", x = "time")
        
        
        
        
    })
    output$Acuracy_evaluation_plot <- renderPlot({
        
        modelGpp <- switch(input$var, 
                           "GISS_CMIP5" = GPP$GISS_mean5,# 
                           "IPSL_CMIP5" = GPP$IPSL_mean5,
                           "CanESM_CMIP5" = GPP$CanESM_mean5,
                           "UKESM_CMIP6" = GPP$UKESM1_mean6,
                           "IPSL_CMIP6" = GPP$IPSL_mean6,
                           "CanESM_CMIP6" = GPP$CanESM_mean6)
        
        
        color <- switch(input$var, 
                        "GISS_CMIP5" = "#D9BACE",# 
                        "IPSL_CMIP5" = "#401F18",
                        "CanESM_CMIP5" = "#8C5549",
                        "UKESM_CMIP6" = "#D99D8F",
                        "IPSL_CMIP6" = "#025959",
                        "CanESM_CMIP6" = "#D95F5F")
        
        
        
        plotdata1 = data.frame(model = modelGpp,RS = GPP$GPP_RS_mean) %>%
            mutate(num = row_number())%>%
            drop_na()
        
        P = cor.test(plotdata1$RS,plotdata1$model,method="pearson")
        P2 = (P$estimate)^2
        
        RMSE = sqrt(mean(plotdata1$RS-plotdata1$model)^2)
        
        slope = round(lm(plotdata1$model ~ plotdata1$RS)$coefficients[2], 2)
        
        plotdata1 %>%
            ggplot(mapping = aes(x = RS, y = model))+
            geom_point(color = "#55A6D9" ) +
            geom_smooth(method = lm)+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                  axis.title.y = element_text( color="black",size=20),
                  axis.title.x = element_text( color="black",size=20),
                  axis.text.x = element_text(color="black",size=15, angle=0),
                  axis.text.y = element_text( color="black", size=15, angle=0)) +
            ylim(c(0,8.5))+
            xlim(c(0,8.5))+
            geom_text(
                aes( x= 0.28, y= 8,label= paste0("italic(R)^2", "==", sprintf('%.2f', P2) )),
                
                parse = TRUE, size=5)+
            geom_text(
                aes( x= 0.5, y= 7.5,label= paste0("italic(RMSE)", "==", sprintf('%.2f', RMSE) )),
                
                parse = TRUE, size=5)+
            geom_text(
                aes( x= 0.38, y= 7,label= paste0("italic(Slope)", "==", sprintf('%.2f', slope) )),
                
                parse = TRUE, size=5)+
            annotate("segment", x = 0, xend = 8.5, y = 0, yend = 8.5, colour = "grey50", size=0.5)+
            labs(y="Modeled(g m-2 d-1)", x = "Observed (g m-2 d-1)")
        
    })
    
    output$Trend_analysis_plot <- renderPlot({
        
        modelGpp <- switch(input$var, 
                           "GISS_CMIP5" = GPP$GISS_mean5,# 
                           "IPSL_CMIP5" = GPP$IPSL_mean5,
                           "CanESM_CMIP5" = GPP$CanESM_mean5,
                           "UKESM_CMIP6" = GPP$UKESM1_mean6,
                           "IPSL_CMIP6" = GPP$IPSL_mean6,
                           "CanESM_CMIP6" = GPP$CanESM_mean6)
        
        
        color <- switch(input$var, 
                        "GISS_CMIP5" = "#D9BACE",
                        "IPSL_CMIP5" = "#401F18",
                        "CanESM_CMIP5" = "#8C5549",
                        "UKESM_CMIP6" = "#D99D8F",
                        "IPSL_CMIP6" = "#025959",
                        "CanESM_CMIP6" = "#D95F5F")
        
        
        
        
        plotdata = data.frame(modelGpp,year = GPP$year,month = GPP$month,MODIS_mean = GPP$MODIS_mean,BESS_mean = GPP$BESS_mean,VPM_mean = GPP$VPM_mean,FLUXCOM_mean = GPP$FLUXCOM_mean) %>%
            group_by(year)%>%
            summarise(Model = mean(modelGpp, na.rm = TRUE),MODIS = mean(MODIS_mean, na.rm = TRUE),BESS = mean(BESS_mean, na.rm = TRUE),VPM = mean(VPM_mean, na.rm = TRUE),FLUXCOM = mean(FLUXCOM_mean, na.rm = TRUE))%>%
            # mutate(deltamodelbyyear =modelbyyear-mean(modelbyyear,na.rm = TRUE),deltaMODISbyyear =MODISbyyear-mean(MODISbyyear,na.rm = TRUE),deltaBESSbyyear =BESSbyyear-mean(BESSbyyear,na.rm = TRUE),deltaVPMbyyear = VPMbyyear-mean(VPMbyyear,na.rm = TRUE),deltaFLUXCOMbyyear = FLUXCOMbyyear-mean(FLUXCOMbyyear,na.rm = TRUE))%>%
            # select(year,deltamodelbyyear,deltaMODISbyyear,deltaBESSbyyear,deltaVPMbyyear,deltaFLUXCOMbyyear)%>%
            mutate(num = row_number()) %>%
            melt(id = c('num','year')) 
        
        plotdata %>%
            ggplot(mapping = aes(x = variable, y = value, color = variable))+
            geom_boxplot()  +
            ylim(c(0,3))+
            theme_classic() +
            theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                  axis.title.x=element_blank(),
                  axis.text.x = element_text(color="black",size=15, angle=0), # grey50
                  axis.text.y = element_text( color="black", size=15, angle=0),
                  axis.title.y = element_text( color="black",size=20),
                  #axis.title.x = element_text( color="black",size=15),
                  axis.line.x= element_line(color="grey50",size = 0.5),
                  axis.line.y= element_line(color="grey50",size = 0.5),
                  #legend.position = c(0.15,0.9),
                  legend.direction = "vertical",
                  legend.title = element_blank(),
                  legend.text = element_text(size=20),
                  plot.margin = unit(c(1, 0, 0, 0), "cm")) +
            scale_colour_manual(
                values = c("#F24738","#F2AE30", "#8DBF5A",  "#9879D9", "#55A6D9"),
                labels = c("Model","MODIS", "BESS", "VPM", "FLUXCOM"))+
            labs(y="GPP (g m-2 d-1)")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
