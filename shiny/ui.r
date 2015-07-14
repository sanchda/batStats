library(shiny)

# Define UI, based loosely on one of Shiny's templates
shinyUI(fluidPage(
  
  ### SIDE PANEL ###
  fluidRow(
  column(3,
    titlePanel("Dave's simple batlab viewer"),
    
    # Show reactive elements
    tabsetPanel(
      tabPanel("Data/Viz",
        h5("Variable selection"),
        fluidRow(
          column(6,uiOutput("choose_cols") ),
          column(6,uiOutput("choose_targ") )
        ),
        
      uiOutput("isNumericPanel"),

      # For visualization, which components of PCA to see?
      h5("Visualization properties:"),
      sliderInput("pca1",
        "First Principal Component (visual only):",
        min = 1,
        max = 15,
        value = 1),
      sliderInput("pca2",
        "Second Principal Component (visual only):",
        min = 1,
        max = 15,
        value = 2)      
      ),
      
    tabPanel("Analysis",
      ## Analysis panel for the STATS mode
      conditionalPanel("input.MAIN_PANEL == 'STATS'",
        radioButtons("cor2Type", "Plot type", c("fast", "thorough"), "fast"),
        conditionalPanel("input.cor2Type == 'fast'",
          sliderInput("corSigLevel","Significance Level",min=0.01,max=0.25,value=.05)
        ),
        conditionalPanel("input.cor2Type == 'thorough'",
          radioButtons("cor2Method", "Correlation coefficient",  c("pearson", "kendall", "spearman"), "pearson"),
          radioButtons("cor2Hist",   "Show histograms?", c("TRUE", "FALSE"), "TRUE")
        )
      ),
      
      ## Distribution fitting
      conditionalPanel("input.MAIN_PANEL == 'DIST'",
          column(6,
            uiOutput("distOptions")
          ),
          column(6,
            radioButtons("distDist", "Distribution:", c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull"), "gamma")
          )
      ),
    
      ## Analysis panel for the SVM mode
      conditionalPanel("input.MAIN_PANEL == 'SVM'",
      
                         # Select which kernel to use
        radioButtons("kernel", "Select SVM kernel:",
                     c("Linear (none)" = "linear",
                       "RBF" = "radial",
                       "Polynomial" = "polynomial",
                       "Sigmoid" = "sigmoid")),
        
        # Optional parameters for each kernel
        conditionalPanel( "input.kernel == 'radial'",
                          numericInput("gamma", "gamma", value=0.01)      
        ),
        
        conditionalPanel( "input.kernel == 'polynomial'",
                          numericInput("gamma", "gamma:", value=0.01),
                          numericInput("coef0", "coef0:", value=0),
                          sliderInput("deg", "degree:", min=1, max=6, value=2)
        ),
        conditionalPanel( "input.kernel == 'sigmoid'",
                          numericInput("gamma", "gamma:", value=0.01),
                          numericInput("coef0", "coef0:", value=0)
        ),
        
        # Global parameters
        numericInput("cost", "C-cost:", value=1),
        numericInput("k", "k-fold validation:", value=1),
        
        # Number of dimensions in PCA compression
        sliderInput("kPCA",
                    "kPCA complexity (compressed search only):",
                    min = 1,
                    max = 14,
                    value = 1)
    ),
    
    # Analysis panel for the GP mode
    conditionalPanel("input.MAIN_PANEL == 'GP'",
                     sliderInput("corSigLevel","Significance Level",min=0.01,max=0.25,value=.05),
                     radioButtons("cor2Method", "Correlation coefficient",  c("pearson", "kendall", "spearman"), "pearson")
    ),
    
    # Analysis panel for the GLM mode
    conditionalPanel("input.MAIN_PANEL == 'GLM'",
                     radioButtons("GLMFamily", "GLM Family", c("binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson" ), "gaussian"),
                     radioButtons("GLMMethod", "Plot Type", c("contrast", "conditional"), "contrast"),
                     uiOutput("GLMPlotVarButtons")
    )
  )
  )),
    
    
  ### MAIN PANEL
  column(9,
    tabsetPanel(id="MAIN_PANEL",
      tabPanel("Debug Info",
        uiOutput("debugInfo"),
        column(3, tableOutput("debugTable")),
        column(3, tableOutput("debugTable2")),
        column(3, tableOutput("debugTable3"))
      ),
      
      tabPanel("STATS",
        "Plot of the pairwise correlation coefficients of dependent variables.  Color indicates the direction of the correlation and size is related to significance (p-value).  The significance slider on the left can be used to mark combinations with low significance (e.g., make it 0.05 for a .95 significance test)",
        conditionalPanel("input.cor2Type == 'fast'",
                         "Plot of the pairwise correlation coefficients of dependent variables.  Color indicates the direction of the correlation and size is related to significance (p-value).  The significance slider on the left can be used to mark combinations with low significance (e.g., make it 0.05 for a .95 significance test)",
                         column(12,plotOutput("corrPlot"))),
        conditionalPanel("input.cor2Type == 'thorough'",
                         "Plot of the pairwise correlation of dependent variables.  Also attempts to create a plot of localized fit and show a histogram.  WARNING:  this is somewhat expensive, only test with relatively small number of variables",
                         column(12,plotOutput("corrPlot2")))
      ),
      
#      tabPanel("DIST",
#        plotOutput("distPlot"),
#        verbatimTextOutput("distSummary")
#      ),
      
      tabPanel("PCA",
        "Plot of the PCA clustering between variables.  Coloring is done based on the dependent variable, with the spatial ",
        plotOutput("pcaRaw")
      ),
      
      tabPanel("SVM",
               "Support Vector Machine is a classification tool based on drawing hyperplanes between points.  It isn't sensitive to the layout of the high-dimensional feature space; only the relative distance between specimens (with a distance metric defined by the kernel)",
               "WARNING:  the below attempts to do a rather primitive cross-validation between trials, so you will see performance bounce around with small sample sizes!",
        tabsetPanel(
          tabPanel("Manual Search",
           plotOutput("pcaPlot"),
           verbatimTextOutput("pcaSummary")
          ),
          tabPanel("Automatic Search",
           plotOutput("tunedPlot"),
           verbatimTextOutput("tunedSummary"),
           uiOutput("tunedPerf")
          ),
          tabPanel("Automatic Search (compressed)",
           plotOutput("tunedPCAPlot"),
           verbatimTextOutput("tunedPCASummary"),
           uiOutput("tunedPCAPerf")
          )
        )
      ),
      
      tabPanel("GP",
        plotOutput("GPPlot")
      ),
      
      tabPanel("GLM",
               "Below is some very primitive support for exploring general linear models on your data.  There is a huge literature discussing interaction between terms, which will be implemented in a future update.",
       plotOutput("GLMPlot"),
       plotOutput("GLMDiagPLot")
      )
    )
  )
)
))