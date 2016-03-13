
shinyUI(
  navbarPage(
    "Benthic Analysis",
                   tabPanel("Introduction",
                            h2("Introduction"),
                            helpText("A package for the analysis of Benthic Macroinvertebrate (BMI) data
                                     using a Reference Condition Approach. Impairment is determined using the Test Site Analysis (TSA). 
                                     This package provides functionallity for: 1)calculation of many commonly used indicator metrics for summarizing BMI communities;
                                     2) automated nearest-neighbour site matching (ANNA and RDA-ANNA) or user-defined reference sites;
                                     3) a variety of diagnostic plots and tools for assessing the confidence of the impairment rank."),
                            br(),
                            h2("Instructions"),
                            h3("Data Input"),
                            helpText("All input data files must have the same site identification structure. 
                                     All input data files must be in .csv format."),
                            helpText("Minimum data requirments are:" ),
                            helpText("1.'Biological data'"),
                            helpText("2. at least one of: 'Environmental Data' or 'User Matched Reference Sites'"),
                            helpText("3. Some sites must be selected as Reference Sites"),
                            br(),
                            h3("Individual Site Analysis"),
                            helpText("This section allows for detailed exploration of individual sites"),
                            br(),
                            h3("Batch Analysis"),
                            helpText("This section allows for analysis of large numbers of sites"),
                            helpText("Method 1: ANNA site matching"),
                            helpText("Method 2: RDA-ANNA site matching with user identified indicator metrics"),
                            helpText("Method 3: User selected site matching")
                            
                            ),
                   
                   #########################################################
                   #DATA INPUT
                   ########################################################
                   
                   navbarMenu("Data Input",
                              #########################################################
                              #Input biological Data
                              ########################################################
                              
                              tabPanel("Biological Data",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Biological Data"),
                                           helpText("Select file containing raw taxa data for calculating summary metrics.", 
                                                    "Taxa identifiers must to be split into 2 rows."),
                                           
                                           fileInput("inbioFile", label = h4("File input - Taxa")),
                                           checkboxInput("metdata",label="Input data are metrics",value=F),
                                          #numericInput("taxa.names", 
                                          #              label = h4("Number of rows used for taxa identifiers"), 
                                          #              value = 2),
                                           
                                           numericInput("site.names", 
                                                        label = h4("Number of columns used for site identifiers"), 
                                                        value = 2),
                                           br(),
                                           "-------------------------------------",
                                           br(),
                                           conditionalPanel("input.metdata==false",downloadButton('downloadmetricData', 'Export  Metrc Data'))
                                         ),
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                                       tabPanel("Taxa Data", dataTableOutput("bio.data.view")),
                                                       tabPanel("Metric Data", dataTableOutput("metric.data.view")),
                                                       tabPanel("Metric Summary", verbatimTextOutput("metric.summary.view"))
                                                       
                                           )
                                         )
                                       )),
                              
                              #########################################################
                              #Input Environmental Data
                              ########################################################
                              
                              tabPanel("Site Matching Data",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Habitat and landscape Data and/or user matched reference sites"),
                                           helpText("At least one of the two file inputs is required.",
                                             "Select file containing landscape and habitat data for site matching.",
                                                    "Site names must match format of biological data.",
                                                    "This panel will be optional in the future to allow users to just use pre-selected reference sites."),
                                           
                                           fileInput("inenvFile", label = h4("File input")),
                                           "-------------------------------------",
                                           helpText("Identify matching reference site samples for each test site sample"),
                                           fileInput("inrefmatchFile", label = h4("File input")),
                                           br(),
                                           downloadButton('downloadenvData', 'Export Environmental Data')
                                         ),
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                                       tabPanel("Environmental Data", dataTableOutput("env.data.view")),
                                                       tabPanel("Environmental Data Summary", verbatimTextOutput("env.summary.view")),
                                                       tabPanel(title="Reference Site Matches",tableOutput("usersitematch.table"))
                                           )
                                         )
                                       )),
                              
                              #########################################################
                              #Identify Reference Sites
                              ########################################################
                              
                              tabPanel("Select Reference Sites",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Select Reference sites"),
                                           helpText("Select which sites should be treated as Reference sites",
                                                    "Input file must have same site name structure as biological data file.",
                                                    "Reference sites are identified with a 1, test site with 0",
                                                    "Future implimentation - select internal reference data"),
                                           
                                           fileInput("inrefIDFile", label = h4("File input"))
                                           
                                         ),
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                                       tabPanel(title="Select Reference Sites",uiOutput("choose_columns")),
                                                       tabPanel(title="Selected Reference Sites",verbatimTextOutput("selrefID")),
                                                       tabPanel(title="Selected Test Sites",verbatimTextOutput("seltestID"))
                                           )
                                         )
                                       ))
                              
                              
                   ),
                   
                   #########################################################
                   #INDIVIDUAL SITE ANALYSIS
                   ########################################################
                   
                   navbarMenu("Individual Site Analysis",
                              
                              #########################################################
                              #Test Site Selection
                              ########################################################
                              tabPanel("Site Selection",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2("Select Test site"),
                                           helpText("Select which test site should be assessed")
                                         ),
                                         mainPanel(tabPanel(title="Select test Site",uiOutput("sel.test.site")))
                                       )),
                              
                              #########################################################
                              #Reference Site Matching
                              ########################################################
                              
                              tabPanel("Reference Site Matching",
                                sidebarLayout(
                                  sidebarPanel(
                                    h2("Reference Site Matching"),
                                    helpText("Text describing site matching"),
                                    br(),
                                    h4("User Matched Reference Sites"),
                                    checkboxInput("user.ref.sitematch","User matched Reference Sites",value=F),
                                    br(),
                                    radioButtons("nn.method", label = h4("Nearest-Neighbour Method"),
                                               choices = list("ANNA" = "ANNA", "RDA-ANNA" = "RDA-ANNA"), selected = "ANNA"),
                                    checkboxInput("adaptive","Adaptive",value=T),
                                    br(),
                                    helpText("Number of reference sites to select. Acts as upper limit if Adaptive selection used."),
                                    numericInput("k.sel", label = h4(""), value = 0)
                                ),
                                mainPanel(
                                  tabsetPanel(types="tabs",
                                              
                                              tabPanel(title="Ordination Plot",
                                                       plotOutput("nn.ord",
                                                                  brush=brushOpts(id = "nnord_brush",resetOnNew = TRUE),
                                                                  dblclick=dblclickOpts(id="nnord_dclick"),
                                                                  click=clickOpts(id="nnord_click"),
                                                                  hover=hoverOpts(id="nnord_hover")),
                                                       br(),
                                                       wellPanel(h3('Display Axis'),uiOutput("site.match.axis"))),
                                              tabPanel(title="Nearest-Neighbour Distance Plot",
                                                       plotOutput("nn.dist",
                                                                  brush=brushOpts(id = "nndist_brush",resetOnNew = TRUE),
                                                                  dblclick=dblclickOpts(id="nndist_dclick"),
                                                                  click=clickOpts(id="ndist_click"),
                                                                  hover=hoverOpts(id="ndist_hover"))),
                                              tabPanel(title="Model Results",verbatimTextOutput("nn.table")),
                                              tabPanel(title="Selected Reference Sites",verbatimTextOutput("nn.table2"))
                                              )
                                ))),
                              #########################################################
                              #Select metrics
                              ########################################################
                              tabPanel("Select Indicator Metrics",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2("Select Indicator Metrics"),
                                           helpText("Select which indicator metrics should be used for further analysis.",
                                                    "Adaptive metric selection is still possible on the selected subset of metrics"),
                                           actionButton("selectallmet", label = "Select All"),
                                           actionButton("selectnonemet", label = "Select None"),
                                           br(),
                                           checkboxInput("m.select","Automatically select indicator metrics for analysis?",value=F)
                                         ),
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                                       tabPanel(title="Select Indicator Metrics",uiOutput("choose_columns1"))
                                           )
                                         )
                                       )),
                              
                              #########################################################
                              #Test Site Analysis
                              ########################################################
                              
                              tabPanel("Test Site Anlaysis",
                                       sidebarLayout(
                                         sidebarPanel(
                                           h2("Test Site Analysis"),
                                           helpText("Text describing TSA"),
                                           br(),
                                           br(),
                                           checkboxInput("distance","Use ecological distance to weigh Mahalanobis Distance?",value=F),
                                           checkboxInput("outlier.rem","Remove outlier reference sites?",value=F),
                                           br()
                                         ),
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                                       tabPanel(title="Mahalanobis Distance Plot",
                                                                plotOutput("tsa.distplot",height=600)),
                                                       tabPanel(title="Indicator Metric Boxplots",
                                                                plotOutput("tsa.boxplot",height=600)),
                                                       tabPanel(title="Mahalanobis Distance PCOA",
                                                                plotOutput("tsa.pcoa",height=600)),
                                                       tabPanel(title="Correspondance Analysis",
                                                                plotOutput("tsa.ca",height=600)),
                                                       tabPanel(title="Selected Metrics",
                                                                verbatimTextOutput("print.sel.met")),
                                                       tabPanel(title="Tables",
                                                                tabsetPanel(types="pills",
                                                                             tabPanel(title="TSA Results",verbatimTextOutput("tsa.results")),
                                                                             tabPanel(title="Partial TSA Results",verbatimTextOutput("ptsa.results")),
                                                                             tabPanel(title="Jacknife Consistency", verbatimTextOutput("tsa.jack"))
                                                                            ))
                                           )
                                         ))
                                       )),
                   
                   #########################################################
                   #Batch ANALYSIS
                   ########################################################
                   
                   navbarMenu("Batch Analysis",
                              
                              #########################################################
                              #Batch ANNA
                              ########################################################
                              
                              tabPanel("Configure",
                                         mainPanel(
                                           tabsetPanel(types="tabs",
                                             tabPanel(title="Options",
                                                      fluidRow(wellPanel(
                                                        uiOutput("batch.nn.method"))
                                                        ),
                                                      conditionalPanel(condition="output.dataavail==1",
                                                      conditionalPanel(condition="output.envdataavail==1",
                                                                       fluidRow(wellPanel(radioButtons("nnmethod.user",label=h4("Ecological distance Calculation:"),choices=c("ANNA","RDA-ANNA"),inline=T),
                                                                                          helpText("Use an adaptive threshold to determine the number of nearest neighbour reference sites?"),
                                                                                          checkboxInput("ab.adaptive","Adaptive",value=T),
                                                                                          helpText("Number of reference sites to select. Acts as upper limit if Adaptive selection used."),
                                                                                          numericInput("ab.k.sel", label = h4(""), value = 0)
                                                                       ))
                                                      ),
                                                      fluidRow(
                                                        column(7,
                                                               conditionalPanel(condition = "input.nnmethod!='User Selected'", 
                                                                                wellPanel(
                                                                                  h4("Site Matching Options"),
                                                                                  helpText("Use an adaptive threshold to determine the number of nearest neighbour reference sites?"),
                                                                                  checkboxInput("ab.adaptive","Adaptive",value=T),
                                                                                  helpText("Number of reference sites to select. Acts as upper limit if Adaptive selection used."),
                                                                                  numericInput("ab.k.sel", label = h4(""), value = 0)
                                                               )),
                                                               wellPanel(
                                                                 h4("Test Site Analysis Options"),
                                                                 conditionalPanel(condition = "output.envdataavail1==1",checkboxInput("ab.distance","Use ecological distance to weigh Mahalanobis Distance?",value=F)),
                                                                 checkboxInput("ab.outlier.rem","Remove outlier reference sites?",value=F)
                                                               ),
                                                               wellPanel(
                                                                 h4("Ouptput and plotting Options"),
                                                                 actionButton("ab.dir","Select Directory"),
                                                                 helpText("Selection window may open minimized, check the task bar."),
                                                                 textOutput("show.sel.dir"),
                                                                 "-----------------------------------------",
                                                                 conditionalPanel("output.envdataavail1==1",checkboxInput("ab.nnscatter.plot","Print Nearest-Neighbour ordination plot?",value=F)),
                                                                 conditionalPanel("input.nnmethod!='User Selected'",checkboxInput("ab.nndist.plot","Print Nearest-Neighbour distance plot?",value=F)),
                                                                 checkboxInput("ab.tsadist.plot","Print TSA distance plot?",value=F),
                                                                 checkboxInput("ab.tsabox.plot","Print TSA boxplot?",value=F),
                                                                 checkboxInput("ab.tsascatter.plot","Print TSA ordination plot?",value=F),
                                                                 checkboxInput("ab.cascatter.plot","Print CA ordination plot?",value=F),
                                                                 checkboxInput("ab.multi.plot","Print multi plot?",value=F),
                                                                 conditionalPanel("input['ab.multi.plot']==true", 
                                                                                  uiOutput("multiplot1"))
                                                               ),
                                                               wellPanel(
                                                                 conditionalPanel(condition="output.seldir==0",
                                                                                          helpText("Select output directory to begin batch run")
                                                               ),
                                                                 conditionalPanel(condition="output.seldir==1",
                                                                                  actionButton("ab.go","Run")
                                                                 )
                                                               )
                                                        ),
                                                        column(5,
                                                               wellPanel(
                                                                 h4("Metric Selection"),
                                                                 conditionalPanel(condition="input.metdata==false",checkboxInput("ab.m.select","Automatically select indicator metrics for analysis?",value=F)),
                                                                 br(),
                                                                 helpText("Selected Indicator Metrics"),
                                                                 actionButton("ab.selectallmet", label = "Select All"),
                                                                 actionButton("ab.selectnonemet", label = "Select None"),
                                                                 uiOutput("ab.choose_columns1")
                                                               ))
                                                      ))),
                                             conditionalPanel("output.abdone==1",tabPanel(title="Results",
                                                      conditionalPanel(condition = "output.abdone==1",
                                                                       dataTableOutput("ab.results"))))

                                       ))))
                   
))