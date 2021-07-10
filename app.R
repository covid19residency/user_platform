library(shiny)
library(pysd2r)
library(ggplot2)
library(readxl)
library(reshape2)

setwd("D:/李昕怡/研究生/课程/capstone/covid19 impact on residence/ui platform")
target <- 'COVID-19 Sing Health Residency Training Model - NHG BS=5.mdl'
py <- pysd_connect()

py <- read_vensim(py, target)

results <- run_model(py)

results$TIME <- c(0:10)

ui <- fluidPage(
    tabsetPanel(
        id = "wizard",
        # type = "hidden",
        tabPanel("Introduction", 
                 h1(align = 'center', 'Explore the impact of Covid-19 on residency training programmes'),
                 p('This is an interactive platform which can help to explore the impact of Singapore’s epidemic on 
                Residency Training Programme so far as well as predict the 
                future impact on residency performance and resident movement, 
                especially on training delay, under ever-changing outbreak situations
                and the corresponding containment scenarios.'),
                 br(),
                 p('Sudden erruption of Covid-19 has caused great impact to Singapore from many aspects in society. 
                Residency training programme is not an exception. 
                The SingHealth Anaesthesiology Residency Programme aims to provide students with the opportunity to develop to the maximum of their ability. 
                Residents are exposed to a diverse range of clinical experience through structured subspecialty rotations that allow flexibility to meet individual goals and objectives.
                SingHealth has perfectly incorporated ACGME-I standards into the programme design.
                However, many residents will be hard to complete the required number of clinical rotations, operative assignments, and patient care encounters due to COVID-19.'),
                 br(),
                 # img(src = "sample rotation chart.png", align = "right",height='100px',width='400px'),
                 br(),
                 p('System dynamic skill(Vensim software) is applied to simulate the whole training process under pandemic scenarios.
                While different elements, such as impact of pandemic, batch size of admission, and length of pandemic, would all lead to different performances of programme,
                users are able to set parameters on their own to see corresponding results. 
                Results include 4 parts: Number of candidates of each accredited year,
                Number of graduates of each accredited year,
                Number of cumulative specialists,
                Time of backlog clearance under different scenarios
                '),
                 #add simulation picture as background
                 
                 
                 actionButton("page_12", "begin")
        ),
        tabPanel("Parameters Setting", 
                 h1("Set Parameters and Start Your Simulation"),
                 p('Based on study, impact of covid-19 on each accredited year 
               is calculated as the average length of stay (ALOS). 
               More serious the pandemic is, longer time period will be taken by residents to complete rotation tasks of each accredited year.
               Parameter "delay length of stay" (= ALOS - 1) for each accredited year and "batch size" (annual number of newly admitted residents) could be set below to see different results.'),
                 strong('Delay length of stay(Please set a number range from 0 to 1)'),
                 fluidRow(
                     column(2,
                            numericInput("alosr1", "R1", value = 0.19, min = 0, max = 1, step = 0.01)
                     ),
                     column(2,
                            numericInput("alosr2", "R2", value = 0.33, min = 0, max = 1, step = 0.01)
                     ),
                     column(2,
                            numericInput("alosr3", "R3", value = 0.24, min = 0, max = 1, step = 0.01)
                     ),
                     column(2,
                            numericInput("alossr1", "SR1", value = 0.11, min = 0, max = 1, step = 0.01)
                     ),
                     column(2,
                            numericInput("alossr2", "SR2", value = 0.06, min = 0, max = 1, step = 0.01)
                     )
                 ),
                 fluidRow(
                     column(12,
                            numericInput("bs", "Batch size(Please set a positive interger)", value = 5, min = 0, step = 1)
                     )
                 ),
                 actionButton("simulate", "Simulate"),
                 br(),
                 br(),
                 actionButton("page_21", "Back to introduction"),
                 actionButton("page_23", "See the results")
        ),
        tabPanel("Candidate", 
                 h1("Number of candidates under different scenarios"),
                 fluidRow(
                     column(4,
                            strong("Choose how many scenarios you want to see(maximum value is 4)"),
                            numericInput("scenario1", "Scenario T", value = 4, min = 0, max = 4),
                            strong("Choose which accredited year result you want to see"),
                            selectInput("year1", "Accredited year", c('R1', 'R2', 'R3', 'SR1', 'SR2')),
                            p("Up to 5 scenarios were applied to predict the impact on residency training in the future -- from best to worst situation. 
                        They respectively correspond to T = 0,1,2,3,4, where T represents the number of years CB is implemented.",
                              style = "font-size: 8pt"),
                            br(),
                            p("For instance, T = 0, no CB will be implemented anymore, thus the impact of covid-19 to the programme can be ignored and the training process is treated as normal. 
                        When T = 1, CB will be carried out in 2020 while the next 5 years will go back into normal. 
                        It is also assumed that when the pandemic is fully controlled for enough long time (one year), then CB won’t be implemented again, which means CB will only appear in consecutive years since 2020.",
                              style = "font-size: 8pt")
                            
                     ),
                     column(8,
                            plotOutput("candidate")
                     ),
                 ),
                 actionButton("page_32", "prev"),
                 actionButton("page_34", "next")
        ),
        tabPanel("Graduate", 
                 h1("Number of graduates under different scenarios"),
                 fluidRow(
                     column(4,
                            strong("Choose how many scenarios you want to see(maximum value is 4)"),
                            numericInput("scenario2", "Scenario T", value = 4, min = 0, max = 4),
                            strong("Choose which accredited year result you want to see"),
                            selectInput("year2", "Accredited year", c('R1 to R2', 'R2 to R3', 'R3 to SR1', 'SR1 to SR2', 'Graduation'))
                     ),
                     column(8,
                            plotOutput("graduate")
                     ),
                 ),
                 
                 actionButton("page_43", "prev"),
                 actionButton("page_45", "next")
        ),
        tabPanel("Cumulative Specialist", 
                 h1("Number of cumulative specialists under different scenarios"),
                 fluidRow(
                     column(4,
                            strong("Choose how many scenarios you want to see(maximum value is 4)"),
                            numericInput("scenario3", "Scenario T", value = 4, min = 0, max = 4)
                     ),
                     column(8,
                            plotOutput("specialist")
                     ),
                 ),
                 
                 actionButton("page_54", "prev"),
                 actionButton("page_56", "next")
        ),
        tabPanel("Backlog Clearance", 
                 h1("Time of backlog clearance under different scenarios"),
                 fluidRow(
                     column(4,
                            strong("Choose how many scenarios you want to see(maximum value is 4)"),
                            numericInput("scenario4", "Scenario T", value = 4, min = 0, max = 4)
                     ),
                     column(8,
                            plotOutput("BAU")
                     ),
                 ),
                 
                 actionButton("page_65", "prev")
        )
    )
)

server <- function(input, output, session) {
    # page jump
    switch_page <- function(i) {
        updateTabsetPanel(inputId = "wizard", selected = i)
    }
    
    observeEvent(input$page_12, switch_page("Parameters Setting"))
    observeEvent(input$page_21, switch_page("Introduction"))
    observeEvent(input$page_23, switch_page("Candidate"))
    observeEvent(input$page_32, switch_page("Parameters Setting"))
    observeEvent(input$page_34, switch_page("Graduate"))
    observeEvent(input$page_43, switch_page("Candidate"))
    observeEvent(input$page_45, switch_page("Cumulative Specialist"))
    observeEvent(input$page_54, switch_page("Graduate"))
    observeEvent(input$page_56, switch_page("Backlog Clearance"))
    observeEvent(input$page_65, switch_page("Cumulative Specialist"))
    
    
    # simulate the model upon 'simulate' button
    out <- eventReactive(input$simulate, {
        out <- results
        for(t in c(0:4)){
            l <- list('"BLI-R1"' = input$alosr1, 
                      '"BLI-R2"' = input$alosr2,
                      '"BLI-R3"' = input$alosr3,
                      '"BLI-SR1"' = input$alossr1,
                      '"BLI-SR2"' = input$alossr2,
                      'Admission' = input$bs,
                      '"T (COVID-19 will affect the system for howmany years?)"' = t)
            set_components(py,l)
            out_ <- run_model(py)
            out_$TIME <- c(0:10)
            out <- rbind(out, out_)
        }
        out <- out[-(1:11),] # drop original result
        out
    })
    
    
    # candidate plot
    output$candidate <- renderPlot({
        
        # get accredited year data under certain scenarios
        candidate <- reactive({
            d1 <- out()[out()$'"T (COVID-19 will affect the system for howmany years?)"' <= input$scenario1, c('TIME', input$year1,'"T (COVID-19 will affect the system for howmany years?)"')]
            d1$TIME <- d1$TIME + 2020 #保留两位小数没用
            d1 <- d1[d1$TIME <= 2025,] #restrict to current time
            colnames(d1) <- c('year', 'value', 'scenario')
            d1$scenario <- as.character(d1$scenario)
            d1
        })
        
        
        # plot
        ggplot(data = candidate(),aes(x=year,y=value,group = scenario,color=scenario))+
            geom_point()+
            geom_line()+
            xlab("Year")+ #xlabel title
            ylab("Number of Candidates")+ #ylabel title
            ggtitle(paste("Number of Candidates at", input$year1, "Under Different Scenarios"))+
            theme(plot.title = element_text(hjust = 0.5))+ #set title position in the middle
            scale_color_discrete(name="Scenario T") # change legend name
        # scale_color_manual(values = c("blue", "green", "red", "yellow", "orange"))
        # scale_y_continuous(limits = c(0, 10)) #set ylim
    })
    
    
    output$graduate <- renderPlot({
        
        # get accredited year data under certain scenarios
        graduate <- reactive({
            d2 <- out()[out()$'"T (COVID-19 will affect the system for howmany years?)"' <= input$scenario2, c('TIME', input$year2,'"T (COVID-19 will affect the system for howmany years?)"')]
            d2$TIME <- d2$TIME + 2020 #保留两位小数没用
            d2 <- d2[d2$TIME <= 2025,] #restrict to current time
            colnames(d2) <- c('year', 'value', 'scenario')
            d2$scenario <- as.character(d2$scenario)
            d2
        })
        
        # plot
        ggplot(data = graduate(),aes(x=year,y=value,group = scenario,color=scenario))+
            geom_point()+
            geom_line()+
            xlab("Year")+#xlabel title
            ylab("Number of Graduates")+#ylabel title
            ggtitle(paste("Number of Graduates at", input$year2, "Under Different Scenarios"))+
            theme(plot.title = element_text(hjust = 0.5))+ #set title position in the middle
            scale_color_discrete(name="Scenario T")
        # scale_color_manual(values = c("blue", "green", "red", "yellow", "orange"))
        # scale_y_continuous(limits = c(0, 10))#set ylim
    })
    
    output$specialist <- renderPlot({
        
        # get accredited year data under certain scenarios
        specialist <- reactive({
            d3 <- out()[out()$'"T (COVID-19 will affect the system for howmany years?)"' <= input$scenario3, c('TIME', 'Specialists','"T (COVID-19 will affect the system for howmany years?)"')]
            d3$TIME <- d3$TIME + 2020 #保留两位小数没用
            d3 <- d3[d3$TIME <= 2025,] #restrict to current time
            colnames(d3) <- c('year', 'value', 'scenario')
            d3$scenario <- as.character(d3$scenario)
            d3
        })
        
        # plot
        ggplot(data = specialist(),aes(x=year,y=value,group = scenario,color=scenario))+
            geom_point()+
            geom_line()+
            xlab("Year")+#xlabel title
            ylab("Number of Cumulative Specialists")+#ylabel title
            ggtitle(paste("Number of Cumulative Specialists Under Different Scenarios"))+
            theme(plot.title = element_text(hjust = 0.5))+ #set title position in the middle
            scale_color_discrete(name="Scenario T")
        # scale_color_manual(values = c("blue", "green", "red", "yellow", "orange"))
        # scale_y_continuous(limits = c(0, 10))#set ylim
    })
    
    output$BAU <- renderPlot({
        
        # get accredited year data under certain scenarios
        BAU <- reactive({
            d4 <- out()[out()$'"T (COVID-19 will affect the system for howmany years?)"' <= input$scenario4, c('TIME', 'Graduation', '"T (COVID-19 will affect the system for howmany years?)"')]
            d4$TIME <- d4$TIME + 2020 #保留两位小数没用
            colnames(d4) <- c('year', 'value', 'scenario')
            d4$scenario <- as.character(d4$scenario)
            d4
        })
        
        # plot
        ggplot(data = BAU(),aes(x=year,y=value,group = scenario,color=scenario))+
            geom_point()+
            geom_line()+
            xlab("Year")+#xlabel title
            ylab("Number of Graduates")+#ylabel title
            ggtitle(paste("Number of Graduates to Become Specialists Under Different Scenarios"))+
            theme(plot.title = element_text(hjust = 0.5))+ #set title position in the middle
            scale_x_continuous(breaks=seq(2020, 2030, 1))+
            scale_color_discrete(name="Scenario T")
        # scale_color_manual(values = c("blue", "green", "red", "yellow", "orange"))
        # scale_y_continuous(limits = c(0, 10))#set ylim
    })
    
    
    
}
shinyApp(ui, server)