library(shiny)
library(shinydashboard)
library(ggplot2)
library(ei)
library(eiPack)
library(eiCompare)
library(shinycssloaders)

dashboardPage(
  
  dashboardHeader(title = "Ecological Inference Analysis",
                          titleWidth=285
                  ),

  dashboardSidebar(width=285,
      fileInput('file1', 'Upload CSV file', accept=c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv')
                ),
      
      ##Reactively prompt for candidate data and names
      uiOutput('numCandidates'),
      uiOutput("numRaces"),
      tags$hr(),
      uiOutput("candDataPrompts"),
      uiOutput("candNamePrompts"),
      uiOutput('dependent1'),
      uiOutput('candName1'),
      conditionalPanel(
        condition = "input.numCandidates >= 2",
        uiOutput('dependent2'),
        uiOutput('candName2')),
      conditionalPanel(
        condition = "input.numCandidates >= 3",
        uiOutput('dependent3'),
        uiOutput('candName3')),
      
      tags$hr(),
      
      ##Reactively prompt for race data and names
      uiOutput("groupDataPrompts"),
      uiOutput("groupNamePrompts"),
      
      # uiOutput('independent1'),
      # uiOutput('raceVar1'),
      # conditionalPanel(
      #   condition = "input.numRaces >= 2",
      #   uiOutput('independent2'),
      #   uiOutput('raceVar2')),
      # conditionalPanel(
      #   condition = "input.numRaces >= 3",
      #   uiOutput('independent3'),
      #   uiOutput('raceVar3')),
      
      tags$hr(),
      uiOutput('tot.votes'),
        tags$hr(),
      uiOutput('ui.slider'),
        br(),
      uiOutput('ui.action')
                  ),

  dashboardBody(
      
    fluidRow(column(width=3,
           
           box(
             width=NULL, height=NULL,
             title = 'Instructions', status='primary', solidHeader=TRUE, 'Use this tool to analyze election results for racially polarized voting.',
             tags$br(), tags$br(),
             actionButton("pdf", 'User Guide & Walkthrough', onclick = "window.open('userGuide.pdf')"),
             tags$br(), tags$br(),
             'Sample data to practice with:',
             tags$br(),
             downloadButton("sample1", "SantaClara.csv"),
             tags$br(),
             downloadButton("sample2", "Waterbury.csv"),
             tags$br(), tags$br(),
             '1. Upload CSV file containing vote counts and demographic information for your election and region of interest. Vote and demographic data must be percentages between 0 and 1.',
             tags$br(), tags$br(),
             '2. Select the relevant columns from your dataset and input category names. Make sure you choose demographic variables that include the entire population (i.e. proportions sum to 1).',
             tags$br(), tags$br(),
             '3. Adjust the slider to select homogeneous precinct threshold.',
             tags$br(), '(by % of precincts in sample)',
             tags$br(),tags$br(),
             '4. Click "Run."',
             tags$div(tags$ul(tags$li('Note that EI analysis can take several minutes depending on the size of your dataset.'))),
             #tags$br(),tags$br(),
             '5. Review figures & tables.'
           ),
           
           box(
             width=NULL, height=NULL, status='info',
             #title='Resources', 
             tags$h6('R pkgs: ', 
                     a('ei |', href='https://cran.r-project.org/web/packages/ei/index.html'), 
                     a('eiPack |', href='https://cran.r-project.org/web/packages/eiPack/index.html'), 
                     a('eiCompare |', href='https://cran.r-project.org/web/packages/eiCompare/index.html'),
                     a('MCMCpack', href='https://cran.r-project.org/web/packages/MCMCpack/index.html')),
             #tags$br(),
             uiOutput('king'),
             uiOutput('groffman'),
             uiOutput('blacksher'),
             tags$h6(a('More...', href='https://scholar.google.com/scholar?q=ecological+inference+voting+rights&btnG=&hl=en&as_sdt=0%2C7'))
           ),
           
           box(icon('globe', lib='glyphicon'), width=NULL, background='black',
               'MGGG @ Tufts/MIT 2017',
               br(),
               #icon('random', lib='glyphicon'),
               tags$code('GIS-Hackathon 1.0')
           )  
    ),
    
    column(width=9,
           ##downloadButton('template', "Expert Witness Report Template"),
           downloadButton('report', 'Output PDF', class='outputpdf'), 
           #tags$head(tags$style(type="text/css", ".outputpdf {float:right; top:0px}")),
           tags$head(tags$style(type="text/css", "
                                  #loadmessage {
                                position: fixed;
                                bottom: 0px;
                                left: 0px;
                                width: 100%;
                                padding: 5px 0px 5px 0px;
                                text-align: center;
                                font-weight: bold;
                                font-size: 100%;
                                color: #000000;
                                background-color: #FFFF66;
                                z-index: 105;
                                }
                                ")),
           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$div("Calculating...",id="loadmessage")),
        #    tags$head(tags$style(type="text/css", "#ei.bounds_rc, #gr_rc, #gr.bounds_rc {
        #       width: 60%;
        #      display: block;
        #       margin-left: auto;
        #       margin-right: auto;
        # }"
        #    )),
           tabBox(
             width=NULL, side='right', height=NULL,
             selected='Model Comparison',
             tabPanel('Data', div(style = 'overflow-x: scroll', tableOutput('ei.compare'))),
             tabPanel('EI',
                      htmlOutput("bounds_expl"),
                      plotOutput('ei.bounds_rc',height="100%")),
             tabPanel('Goodman', 
                      htmlOutput('gr_expl'),
                      tableOutput('gr_rc'), 
                      plotOutput('gr.bounds_rc',height="100%")),
             tabPanel('Model Comparison', 
                      htmlOutput("welcome"), 
                      htmlOutput("est_expl"), 
                      tableOutput('est_rc'))
           )
          )
      )
   )
)




