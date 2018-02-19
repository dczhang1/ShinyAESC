# Common language effect size Shiny App
# Don Zhang, 2018


### Required libraries
library(tidyverse)
library(readr)
library(psych)
library(mosaic)
###

### Read default dataset
SampleData <- read.csv("sampleData.csv", header = T)

# Define ui logic 
ui <- fluidPage(
        
        # Application title
        titlePanel("Alternative Effect Size Statistic Calculator"),
        
        
        # Sidebar with user input
      
        sidebarLayout(
          
                sidebarPanel(
                        
                        ### Load data file
                        ### Name of currently loaded data
                        h4(textOutput("myFileName"), style = "color:blue"), 
                        fileInput('file1', 'Upload your data file (*.csv)',
                                  accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                        
                        checkboxInput('header', 'Header', TRUE),
                        tags$hr(),
                        h4("Basic parameters"),
                        ### Number of bins
                        numericInput('bins', "Number of bins", value = 5, min = 4, max = 12, step = 1),
                        ### Create variable selector
                        selectInput("predictorVar", "Predictor", c("Predictor")),
                        selectInput("criterionVar", "Criterion", c("Criterion")),
                        ### Cutoff selector
                        numericInput("cutoffInput", "Criterion Cut-off", value = 3.5, step = 1
                        ),
                        actionButton("setmean", "Set CO to mean of criterion"),

                        tags$hr(),
                        h4("Additional parameters for effect size calculations"),
                        ### Cutoff for X (BESD Only)
                        sliderInput("cutoff.X", "Predictor Cut-off (%-ile)", min = 0.01, max = .99, value = .5, step =.01)
                        
                        ),
                
                # Show outputs
                mainPanel(
                        tabsetPanel(
                                
                                tabPanel("Expectancy Chart",h3("Expectancy Chart"), plotOutput("expectancyPlot"),hr(),h3("Expectancy Table"), tableOutput('expectancyTable')),
                                tabPanel("Raw Data", tableOutput('contents')),
                                tabPanel("Descriptives", tableOutput('descriptable'), plotOutput("histogram.X"), plotOutput("histogram.Y"), plotOutput('corplot')),
                                tabPanel("Effect Sizes",plotOutput("histogram.overlap"), hr(), h4("Descriptive Statistics separated by predictor"),tableOutput("clestable"),h4("Effect Size Statistics"),tableOutput("cles"), 
                                         h5("Common language effect size"),textOutput("cles.verbal"),
                                         hr(), h4("Binomial effect size display (BESD)"),tableOutput('besd'))
                                
                        )
                )
        )
)


# Define server logic 
server <- function(input, output, session) {
        
        dsnames <- c() #blank array for dataframe names
        
        ### Load data
        data_set <- reactive({
                inFile <- input$file1
                
                if (is.null(inFile))
                        return(SampleData)
                
                data_set<-read.csv(inFile$datapath, header=input$header)
        })
        
        ### Get file name
        file_name <- reactive({
                inFile <- input$file1
                
                if (is.null(inFile))
                        return("SampleData")
                
                return (stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
        })
        ### Render file name
        output$myFileName <- renderText({
                paste("Data file:", file_name())
                                })
        
        ### Render raw data
        output$contents <- renderTable({
                data_set()
        })
        
        ### Update select inputs
        observe({
                dsnames <- names(data_set())
                cb_options <- list()
                cb_options[dsnames] <- dsnames
                updateSelectInput(session, "predictorVar",
                                  label = "Predictor Variable",
                                  choices = cb_options,
                                  selected = cb_options[1])
                updateSelectInput(session, "criterionVar",
                                  label = "Criterion Variable",
                                  choices = cb_options,
                                  selected = cb_options[2])
        })
        
        
        ### Button for setting criterion Y as mean Y
        observeEvent(input$setmean, {
                df <- data_set()
                var.Y <- input$criterionVar
                df.Y <- select(df, var.Y)
                new.CO <- round(mean(na.omit(df.Y[,1])),2)
                updateNumericInput(session, "cutoffInput", value = new.CO)
                
                
                #mean.Y <- as.numeric(dplyr::summarise(df, mean(var.Y, na.rm = T)))
               
          
          })
        
        ### Code for calculating correlation between selected variables
        validity <- reactive({
                df = data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                exp.X <- select(df, var.X)
                exp.Y <- select(df, var.Y)
                validity <- round(cor(exp.X, exp.Y, use = "na.or.complete"),4)
        })
       
        
        ### Render correlation
        output$validity <- renderText({
                paste( "r = ", validity())
        })
        
        
        ### Create expectancy table
        df.exp <- reactive({
                df = data_set()
                bins <- input$bins
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                exp.X <- select(df, var.X)
                exp.Y <- select(df, var.Y)
                co.Y <- input$cutoffInput
                
                binsep <- 1/bins
                quantiles.X <- quantile(exp.X, seq(0,1, by = binsep), na.rm=T) ### Calculate the values for quantiles based on number of bins
                
                df$ntile.X <- as.numeric(dplyr::ntile(exp.X, bins)) #create new variable to indicate quantile for each X
                df <- dplyr::mutate(df, dicho.Y = as.numeric(exp.Y > co.Y)) #dichotomize Y into 0 and 1 relative to cutoff value
                
                exptable <- matrix(nrow = bins, ncol = 4) #empty table for expectancy chart
                for (x in 1:bins){
                        exptable[x,1] = as.numeric(quantiles.X[x])
                        exptable[x,2] = as.numeric(quantiles.X[x+1])
                        exptable[x,3] = as.numeric(dplyr::summarise(filter(df, ntile.X == x), mean(dicho.Y, na.rm = T)))
                        exptable[x,4] = as.numeric(dplyr::summarise(filter(df, ntile.X == x), sum(dicho.Y, na.rm = T)))
                }         
                df.exp <- data.frame(exptable)    
                df.exp <- dplyr::rename(df.exp, lowerBound = X1, upperBound = X2, proportion = X3, frequency = X4)
                
                df.exp$xlabels <- paste(round(df.exp$lowerBound,1), round(df.exp$upperBound,1), sep=" to ")
                df.exp
        })

        
        ### Create expectancy table output
        output$expectancyTable <- renderTable({
                df.exp()
        })
        ###
        
        ### Create descriptives table output
        output$descriptable <- renderTable({
                df <- data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                descrip.df <- select(df, var.X,var.Y)
                #print(descrip.df)
                #print(var.X)
                #print(var.Y)
                descriptable <- psych::describe(descrip.df)
        },include.rownames=T)
        
        ###
        
        ### Create histogram for X and Y variables
        output$histogram.X <- renderPlot({
                df <- data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                descrip.df <- select(df, var.X,var.Y)
                
                g <- ggplot(descrip.df, aes_string(var.X)) +
                        geom_histogram(bins = 10) + 
                        labs(title = paste("Histogram for", input$predictorVar, sep = " "))+
                  theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
                g

        
        })
        output$histogram.Y <- renderPlot({
                df <- data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                descrip.df <- select(df, var.X,var.Y)
                
                g <- ggplot(descrip.df, aes_string(var.Y)) +
                        geom_histogram(bins = 10) + 
                        labs(title = paste("Histogram for", input$criterionVar, sep = " "))+
                      theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=14,face="bold"))
                g
                
        })
        ###
        
        
        ### Create BESD Table 
        df.besd <- reactive({
                df = data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                exp.X <- select(df, var.X)
                exp.Y <- select(df, var.Y)
                co.Y <- input$cutoffInput
                co.X <- input$cutoff.X
                
                df <- dplyr::mutate(df, dicho.Y = as.numeric(exp.Y > co.Y)) #dichotomize Y into 0 and 1 relative to cutoff value
                
                cutoff.X <- as.numeric(quantile(exp.X, probs = co.X, na.rm = T))
                
                prob.Y.above <- as.numeric(dplyr::summarise(filter(df, exp.X > cutoff.X), mean(dicho.Y, na.rm = T))) 
                prob.Y.below <- as.numeric(dplyr::summarise(filter(df, exp.X < cutoff.X), mean(dicho.Y, na.rm = T))) 
                
                m = matrix(c(prob.Y.above, prob.Y.below,  1-prob.Y.above, 1-prob.Y.below), dimnames = list(c(paste(input$predictorVar,">", round(cutoff.X,2), sep = " "), paste(input$predictorVar,"<", round(cutoff.X,2), sep = " ")),
                                                                                                           c(paste("p(", input$criterionVar, ") > ", round(co.Y,2), sep = ""), paste("p(", input$criterionVar, ") < ", round(co.Y,2), sep = ""))), nrow = 2, ncol = 2)
        })
        ###
        
        
        ### Create overlapped histogram
        output$histogram.overlap <- renderPlot({
                
                df <- data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                df.Y <- select(df, var.X,var.Y)
                
                exp.X <- select(df, var.X)
                co.X <- input$cutoff.X
                cutoff.X <- as.numeric(quantile(exp.X, probs = co.X, na.rm = T))
                
                
                df.Y <- dplyr::mutate(df.Y, dicho.X = case_when(exp.X < cutoff.X ~ 0, 
                                                                  exp.X > cutoff.X ~ 1,
                                                                  TRUE ~ -1)) %>% 
                        filter(dicho.X > -1)
                
                
                
                  g<-NULL
                  g <- ggplot(df.Y, aes_string(var.Y)) +
                        geom_density(aes(fill = factor(dicho.X, labels = c(paste(input$predictorVar,"Below", round(cutoff.X,2), sep = " "), paste(input$predictorVar,"Above", round(cutoff.X,2), sep = " ")))))+
                        labs(title = paste("Density plot for", input$criterionVar, sep = " "), subtitle = "Separated by predictor standing")+
                        theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
                        theme(axis.text=element_text(size=12),
                        axis.title=element_text(size=14,face="bold"))
                
                  
                
                
                return(g)        
                
                
                })
        ###
        
        ### Calculate statistics for CLES
        df.cles <- reactive({
          df <- data_set()
                var.X <- input$predictorVar
                var.Y <- input$criterionVar
                exp.X <- select(df, var.X)
                co.X <- input$cutoff.X
                cutoff.X <- as.numeric(quantile(exp.X, probs = co.X, na.rm = T))

          df.cles <- select(df, var.X,var.Y)
          df.cles <- dplyr::mutate(df.cles, dicho.X = case_when(exp.X < cutoff.X ~ 0,
                                                                  exp.X > cutoff.X ~ 1,
                                                                  TRUE ~ -1)) %>%
                        filter(dicho.X > -1)
          print(df.cles)
          return(df.cles)
          
        })
          
        
        ### Create descriptive table for CLES
        output$clestable <- renderTable({
          df <- df.cles()
          var.Y <- input$criterionVar
        
          df1 <- filter(df, dicho.X == 0)
          df2 <- filter(df, dicho.X == 1)

          rxy <- validity()
          rxysq <- round((rxy^2)*100,1)
          mean.y1 <- mean(df1[,2], na.rm = T)
          sd.y1 <- sd(df1[,2], na.rm = T)
          mean.y2 <- mean(df2[,2], na.rm = T)
          sd.y2 <- sd(df2[,2], na.rm = T)
          n.y1 <- length(df1[,2]) 
          n.y2 <- length(df2[,2]) 
          sdpooled <- sqrt(
            ((n.y1 -1)*sd.y1^2 + (n.y2 - 1)*sd.y2^2)/(n.y1+n.y2-2)
          )
          gy1y2 <- (mean.y1-mean.y2)/sdpooled
          
          
          # formula for cles: http://core.ecu.edu/psyc/wuenschk/docs30/CL.pdf
          zDif <- (abs(mean.y1-mean.y2))/(sqrt(sd.y1^2 + sd.y2^2))
          cl <- pnorm(zDif)

          clestable <- matrix(c(mean.y1, sd.y1, n.y1, mean.y2, sd.y2, n.y2),  dimnames = list(c("Mean of Y", "SD of Y","n"),c("Below Cutoff","Above Cutoff")),nrow = 3, ncol = 2)
        },include.rownames=T)
        
        ###
        
        
        ### Calculate CLES
            output$cles <- renderTable({
              df <- df.cles()
              var.Y <- input$criterionVar
            
              df1 <- filter(df, dicho.X == 0)
              df2 <- filter(df, dicho.X == 1)
    
              rxy <- validity()
              rxysq <- round((rxy^2)*100,1)
              mean.y1 <- mean(df1[,2], na.rm = T)
              sd.y1 <- sd(df1[,2], na.rm = T)
              mean.y2 <- mean(df2[,2], na.rm = T)
              sd.y2 <- sd(df2[,2], na.rm = T)
              n.y1 <- length(df1[,2]) 
              n.y2 <- length(df2[,2]) 
              sdpooled <- sqrt(
                ((n.y1 -1)*sd.y1^2 + (n.y2 - 1)*sd.y2^2)/(n.y1+n.y2-2)
              )
              gy1y2 <- round(abs((mean.y1-mean.y2)/sdpooled),2)
              
              
              # formula for cles: http://core.ecu.edu/psyc/wuenschk/docs30/CL.pdf
              zDif <- round((abs(mean.y1-mean.y2))/(sqrt(sd.y1^2 + sd.y2^2)),3)
              cl <- round(pnorm(zDif),2)
    
              cles <- matrix(c(rxy, paste(rxysq, "%", sep = ""), gy1y2, zDif, cl), ncol = 5, dimnames = list(c("Value"), c("Pearson's r", "r-squared", "Hedge's g", "z(Difference)", "CLES")))
            },include.rownames=T)
            
        ###
            
        ### Verbal description of CLES
            output$cles.verbal <- renderText({
               df <- df.cles()
              var.Y <- input$criterionVar
            var.X <- input$predictorVar
                var.Y <- input$criterionVar
                exp.X <- select(df, var.X)
                co.X <- input$cutoff.X
                cutoff.X <- as.numeric(quantile(exp.X, probs = co.X, na.rm = T))
                
              df1 <- filter(df, dicho.X == 0)
              df2 <- filter(df, dicho.X == 1)
    
              rxy <- validity()
              rxysq <- round((rxy^2)*100,1)
              mean.y1 <- mean(df1[,2], na.rm = T)
              sd.y1 <- sd(df1[,2], na.rm = T)
              mean.y2 <- mean(df2[,2], na.rm = T)
              sd.y2 <- sd(df2[,2], na.rm = T)
              n.y1 <- length(df1[,2]) 
              n.y2 <- length(df2[,2]) 
              sdpooled <- sqrt(
                ((n.y1 -1)*sd.y1^2 + (n.y2 - 1)*sd.y2^2)/(n.y1+n.y2-2)
              )
              gy1y2 <- round(abs((mean.y1-mean.y2)/sdpooled),2)
              
              
              # formula for cles: http://core.ecu.edu/psyc/wuenschk/docs30/CL.pdf
              zDif <- round((abs(mean.y1-mean.y2))/(sqrt(sd.y1^2 + sd.y2^2)),3)
              cl <- round(pnorm(zDif),2)
    
              cles <- matrix(c(rxy, paste(rxysq, "%", sep = ""), gy1y2, zDif, cl), ncol = 5, dimnames = list(c("Value"), c("Pearson's r", "r-squared", "Hedge's g", "z(Difference)", "CLES")))
              
              cles.verbal <- paste("A randomly chosen person with", input$predictorVar, "greater than", cutoff.X, "has a", cl*100, "percent chance of obtaining a higher", input$criterionVar, "than a randomly chosen person with", input$predictorVar, "less than",cutoff.X, sep = " ")
              
            })
        
          
        ### Create BESD output
        output$besd <- renderTable({
                df.besd()
                },
                include.rownames=T)
        ###
        
        
        ###
        
        ### Code for creating plot
        output$expectancyPlot <- renderPlot({
                df <- df.exp()
                exp.plot <- ggplot(df) +
                        geom_bar(aes(x = xlabels, y = proportion), stat = "identity") + 
                        ylim(0,1) +
                        labs(x = input$predictorVar, y = paste("Proportion above", input$cutoffInput, "in", input$criterionVar, sep = " ")) +
                        theme_bw()+
                        theme(axis.text=element_text(size=12),
                        axis.title=element_text(size=14,face="bold"))
                exp.plot
        })
        
        
        

        
        ### Code for correlation plot
        output$corplot = renderPlot(
                {
                        df <- data_set()
                        gp <- NULL
                        if (!is.null(df)){
                                xv <- input$predictorVar
                                yv <- input$criterionVar
                                if (!is.null(xv) & !is.null(yv)){
                                        if (sum(xv %in% names(df))>0){ # supress error when changing files
                                                gp <- ggplot(data = df, aes_string(xv, yv)) + 
                                                        geom_point() +
                                                        scale_color_manual(values=c('black','black'))+
                                                        #Manually set the shape to hollow and solid dots;
                                                        #though these might look like the same shape with different colors,
                                                        #they actually consitute different shapes
                                                        scale_shape_manual(values=c(1,16)) +
                                                        #Add fitted regression lines for each group.
                                                        #Remove 'se=FALSE' to create confidence bands.
                                                        #Remove 'fullrange=TRUE' to create unextrapolated lines.
                                                        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
                                                        #Apply APA theme
                                                        geom_jitter(width = 0.5)+
                                                        labs(
                                                             
                                                             title="Scatterplot", 
                                                             caption = paste("Source: ", file_name(), sep = " "))+
                                                        theme_bw()
                                        }
                                }
                        }
                        return(gp)
                }
        )
        
        
        ####
}

# Run the application 
shinyApp(ui = ui, server = server)

