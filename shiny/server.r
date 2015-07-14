# Load the libraries
library('shiny')
library('ggplot2')
library('e1071')
library('Hmisc')
library('corrplot')
library('PerformanceAnalytics')
library('visreg')
library('MASS')
library('fitdistrplus')
library('boot')


# Load the data
# Don't use data.table; this set is tiny!
# Import data
setwd("C:\\Users\\David Sanchez\\dev\\bats");
data = read.csv("simplified_export3.csv");
data = data[1:33,];

# Split identifiers from group
identifiers = data[,1];
data$Field.number = NULL;
alldata=data;

# Define server logic
shinyServer(function(input, output) {
  
  ### Import the data
  
  # Create a renderUI element to allow user to select their input data based on what is actually there.
  output$choose_cols = renderUI({
    colNames = colnames(alldata);
    
    # Create checkboxes; have first two ticked by default
    checkboxGroupInput("theseColNames", "Select independents:", colNames, colNames[ c(1,2) ]);
  })
  
  # Create renderUI element to allow user to select dependent variable
  # Provide some interpretative options based on what the variable looks like
  
  # Names of unused columns
  freeCols = reactive({
    colNames = colnames(alldata);
    theseCols = colNames[! (colNames %in% input$theseColNames) ];
    if( is.na( theseCols) || is.null( theseCols ) ) {
      return( NULL );
    } else {
      return( theseCols );
    }
  })
  
  values = reactiveValues(old="Initialize");
  
  output$choose_targ = renderUI({
    
    # Check whether there are any columns left (if not, return null, which tells other reactives to present null output and UI elements to display message accordingly)
    # If pass, check whether thisTarg is set to not Null.  If so, check to see that it's a valid column.  If it's a valid column, don't change anything.

    if( is.null( freeCols() ) || is.na( freeCols() ) ) {

      X = helpText("There are no dependent columns left.  Please make sure at least one column is not selected as an independent variable.")
    } else {
      if( !is.null( values$old ) ) {
        
        if( values$old %in% freeCols() ) {
          X = radioButtons("thisTargName", "Select dependent:", freeCols(), values$old );
          
        } else {
          X = radioButtons("thisTargName", "Select dependent:", freeCols(), freeCols()[1] );
        }
        
      } else {
        X = radioButtons("thisTargName", "Select dependent:", freeCols(), freeCols()[1] );
      }
    }
    
    isolate({ values$old = input$thisTargName });
    return(X);
    
  })
  
  # Define the raw independent and dependent variables.  Independent variable later gets transformed according to user decisions.
  indData  = reactive({
    outputDF = data.frame( matrix(0, ncol = length( input$theseColNames), nrow = nrow( alldata ) ) );
    colnames(outputDF) = input$theseColNames;
    
    # Iterate through, populating new DF
    for( var in input$theseColNames ) {
      X = alldata[, var];
      outputDF[, var] = as.numeric(X);
    }
    
    return( outputDF )
  })
  
  targRaw = reactive({ alldata[, input$thisTargName ] })
  
  targType = reactive({
    if( class( targRaw() ) == "NULL" || length( targRaw() ) == 0 || is.null( targRaw() ) ) {
      return( NULL );
    } else if( class( targRaw() ) == "numeric" ) {
      return( "numeric" );
    } else if( class( targRaw() ) == "factor" ) {
      return( "factor" ); 
    } else if( class( targRaw() ) == "character") {
      return( "character" );
    } else if( class( targRaw() ) == "integer" ) {
      return( "numeric" );
    } else {
     return( NULL ); 
    }
  })
    
  # Populate some additional input in case the target variable is numeric
  
  output$isNumericPanel = renderUI({
    if( length(input$numericType) == 0 || is.null( input$numericType) ) {
      radioButtons("numericType", "Interpret numeric:", c("continuous", "discrete") , selected = "discrete")
    } else if( targType() == "numeric" && input$numericType == 'discrete') {
      list(
        radioButtons("numericType", "Interpret numeric:", c("continuous", "discrete") , selected = "discrete"),
        sliderInput("discN","Discretization:", min = 1, max = 15, value = 3)
      )
    } else {
      radioButtons("numericType", "Interpret numeric:", c("continuous", "discrete") , selected = "discrete")
    }
  })
  
  numericType = reactive({
    if( length( input$numericType ) == 0 ) {
      return( "discrete" )
    } else {
      return( input$numericType )
    }
  })
  
  discN = reactive({
    if( length( input$discN ) == 0 ) {
      return( 3 )
    } else {
      return( input$discN )
    }
  })
  
  # Define the final target variable
  targetVar_Factor = reactive({
    # If the raw column is character, convert to factor
    if( is.null( targType() ) ) {
      return( NULL )
    } else if( targType() == "numeric" && numericType() == "discrete" ) {
      if( length( unique( targRaw() ) ) <= discN() ) {
        return( targRaw() )
      } else if( length( unique( targRaw() ) ) <= 5 ) {
        return( targRaw() )
      } else {
        Y= cut(targRaw(), as.vector(ggplot2:::breaks( targRaw(), "n", n=discN())));
        Y[ is.na(Y) ] = levels(Y)[1];
        return(Y)
        }
    } else if( targType() == "numeric" && numericType() == "continuous" ) {
      return( targRaw() )
    } else if( targType() == "character" ) {
      return( as.factor( targRaw() ) )
    } else if( targType() == "factor") {
      return( targRaw() )
    } else {
      return( NULL )
    }    
  })
  
  targetVar = reactive({
    if( class( targetVar_Factor() ) == "factor" ) {
      return( as.numeric( targetVar_Factor() ) );
    } else {
      return( targetVar_Factor() )
    }
  })
  
  targColors = reactive({
    theseColors = rainbow( length( unique( targetVar() ) ) );
    return( theseColors[ as.numeric(as.factor(targetVar())) ] );
  })
  
  output$debugInfo = renderUI({
    list(
      input$thisTargName,
      freeCols()
    )
  })
  
  output$debugTable = renderTable({
      as.data.frame( targRaw() )
  })

  output$debugTable2 = renderTable({
    as.data.frame( targetVar_Factor() )
  })

  output$debugTable3 = renderTable({
    as.data.frame( targetVar() )
  })

  ### Statistics summary
  cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
  }

  output$corrPlot = renderPlot({
    res1 <- cor.mtest(indData(), 0.95)
    res2 <- cor.mtest(indData(), 0.99)
    ## specialized the insignificant value according to the significant level
    M = cor( indData() );
    
    corrplot.mixed(M, p.mat = res1[[1]], sig.level = input$corSigLevel/100)
  })

  output$corrPlot2 = renderPlot({
    chart.Correlation(indData(), method=input$cor2Method, histogram=input$cor2Hist, pch="+")
  })
  
  
  ### Distribution fitting
  output$distPlot = renderPlot({
    fitg = fitdist( indData()[, input$distCol ] , input$distDist);
    plot(fitg, demp=input$distDemp )
  })
  
  output$distSummary = renderText({
    fitg = fitdist( indData()[, input$distCol ] , input$distDist);
    capture.output( summary(fitg) );
  })
  
  ## Make a plot of the straight-up PCA
  # Perform PCA
  output$distOptions = renderUI({
    radioButtons('distCol', 'Column to check:', colnames(indData()), colnames(indData())[1])   
    
  })
  
  pca.rows = reactive({
    x = indData();
    pca.vars = prcomp(~., x , scale = TRUE);
    as.data.frame( predict( pca.vars, x ) )
  })
  
  # Display
  output$pcaRaw = renderPlot({
    colors = targColors();
    df = cbind(pca.rows(), colors);
    
    p = ggplot(df, mapping=aes_string(x=colnames(df)[input$pca1],    y=colnames(df)[input$pca2],    colour="colors"))
    p = p + scale_color_manual(values = unique( colors ),
                               labels = unique( targetVar_Factor()  ))
    return( p + geom_point() )
  })
  

  # Run manual SVM
  model.svm = reactive({
    df = cbind( indData(), targetVar() );
    tVar = as.factor(targetVar());
    thisGamma = input$gamma;
    thisDeg = input$deg;
    thisCoef0 = input$coef0;
    thisCost  = input$cost;
    thisK     = input$k;
                      
    # Each kernel has its own hyperparameters, so might as well just call each one
    # TODO: are poly and sigmoid not converging..?
    #
    switch(input$kernel,
           "linear"  = svm(tVar ~ ., data = df, kernel = "linear",                                                       cost=thisCost, k=thisK),
           "radial"  = svm(tVar ~ ., data = df, kernel = "radial",  gamma=thisGamma,                                     cost=thisCost, k=thisK),
           "poly"    = svm(tVar ~ ., data = df, kernel = "poly",    gamma=thisGamma, deg = input$deg, coef0=thisCoef0,   cost=thisCost, k=thisK),
           "sigmoid" = svm(tVar ~ ., data = df, kernel = "sigmoid", gamma=thisGamma,                  coef0=thisCoef0,   cost=thisCost, k=thisK)
    )
  })
  
  # Run auto-tuned SVM
  tuned.svm = reactive({
    df = indData();

    switch(input$kernel,
           "linear"  = tune.svm(df, as.factor(targetVar()), kernel = "linear"),
           "radial"  = tune.svm(df, as.factor(targetVar()), kernel = "radial"),
           "poly"    = tune.svm(df, as.factor(targetVar()), kernel = "poly"),
           "sigmoid" = tune.svm(df, as.factor(targetVar()), kernel = "sigmoid")
    )
  })
  
  # Run auto-tuned SVM on a subset of the PCA space
  tuned.svm.pca = reactive({
    df = pca.rows()[,1:input$kPCA];
    
    switch(input$kernel,
           "linear"  = tune.svm(df, as.factor(targetVar()), kernel = "linear"),
           "radial"  = tune.svm(df, as.factor(targetVar()), kernel = "radial"),
           "poly"    = tune.svm(df, as.factor(targetVar()), kernel = "poly"),
           "sigmoid" = tune.svm(df, as.factor(targetVar()), kernel = "sigmoid")
    )
  })
  

  
  # Make plot of PCA along chosen coords
  output$pcaPlot <- renderPlot({
    
    # build DF holding data and colors from original PCA
    classes = targColors();
    df = cbind(pca.rows(), classes) # because ggplot2 prefers data frames
    
    # build DF holding data and colors for just the support vectors
    thisIndex = model.svm()$index;
    SV_rows = pca.rows()[thisIndex,];
    SV_colors = classes[thisIndex];
    SV_df = cbind(SV_rows, SV_colors)
    
    # Now visualize, with triangles over support vectors
    p = ggplot(df,    mapping=aes_string(x=colnames(df)[input$pca1],    y=colnames(df)[input$pca2],    colour="classes"))
    p = p + geom_point()
    p = p + geom_point(SV_df, shape=2, mapping=aes_string(x=colnames(SV_df)[input$pca1], y=colnames(SV_df)[input$pca2], colour="SV_colors", size="10"))
    p = p + scale_color_manual(values = unique( classes ),
                               labels = unique( targetVar_Factor()  ))
    return(p);
  
  })

  # Show summary statistics of manually tuned SVM
  output$pcaSummary = renderText({
    capture.output(summary( model.svm() ))
  })


  # Make a plot of the tuned SVM performance
  output$tunedPlot <- renderPlot({
    # build DF holding data and colors from original PCA
    classes = targColors();
    df = cbind(pca.rows(), classes) # because ggplot2 prefers data frames
    
    # build DF holding data and colors for just the support vectors
    thisIndex = tuned.svm()$best.model$index;
    SV_rows = pca.rows()[thisIndex,];
    SV_colors = classes[thisIndex];
    SV_df = cbind(SV_rows, SV_colors)
    
    # Now visualize, with triangles over support vectors
    p = ggplot(df,  mapping=aes_string(x=colnames(df)[input$pca1],    y=colnames(df)[input$pca2],    colour="classes"))
    p = p + geom_point()
    p = p + geom_point(SV_df, shape=2, mapping=aes_string(x=colnames(SV_df)[input$pca1], y=colnames(SV_df)[input$pca2], colour="SV_colors", size="10"))
    p = p + scale_color_manual(values = unique( classes ),
                               labels = unique( targetVar_Factor()  ))
    return(p);
    
  })

  # Show summary statistics of automatically tuned SVM
  output$tunedSummary = renderText({
    capture.output(summary( tuned.svm()$best.model ))
  })
  
  # Show perf of best automatically tuned SVM
  output$tunedPerf = renderUI({
    list(
      "Best performance: ",
       1 - tuned.svm()$best.performance,
      "% success rate."
    )
  })


  # Make a plot of the tuned SVM (on PCA data) performance
  output$tunedPCAPlot <- renderPlot({
    # build DF holding data and colors from original PCA
    classes = targColors();
    df = cbind(pca.rows(), classes) # because ggplot2 prefers data frames
    
    # build DF holding data and colors for just the support vectors
    thisIndex = tuned.svm.pca()$best.model$index;
    SV_rows = pca.rows()[thisIndex,];
    SV_colors = classes[thisIndex];
    SV_df = cbind(SV_rows, SV_colors)
    
    # Now visualize, with triangles over support vectors
    p = ggplot(df,  mapping=aes_string(x=colnames(df)[input$pca1],    y=colnames(df)[input$pca2],    colour="classes"))
    p = p + geom_point()
    p = p + geom_point(SV_df, shape=2, mapping=aes_string(x=colnames(SV_df)[input$pca1], y=colnames(SV_df)[input$pca2], colour="SV_colors", size="10"))
    p = p + scale_color_manual(values = unique( classes ),
                               labels = unique( targetVar_Factor()  ))
    return(p);
    
  })

  # Show summary statistics of automatically tuned SVM
  output$tunedPCASummary = renderText({
    capture.output(summary( tuned.svm.pca()$best.model ))
  })

  # Show perf of best automatically tuned SVM
  output$tunedPCAPerf = renderUI({
    list(
      "Best performance: ",
       1 - tuned.svm.pca()$best.performance,
      "% success rate."
    )
  })

  # Perform a GLM on the data

  output$GLMPlotVarButtons = renderUI({
    radioButtons("thisGLMPlotVar", "Select GLM plot variable:", input$theseColNames, input$theseColNames[1]);
  })

  output$GLMPlot = renderPlot({
    Y = targetVar();
    df = cbind(Y, indData() );

    glm.fit = glm(Y ~., data=df, family=input$GLMFamily );
    return( visreg( glm.fit, input$thisGLMPlotVar, type=input$GLMMethod ) );
    
  })
  
  output$GLMDiagPLot = renderPlot({
    Y = targetVar();
    df = cbind(Y, indData() );
    
    glm.fit = glm(Y ~., data=df, family=input$GLMFamily );
    glm.diag = glm.diag( glm.fit );
    glm.diag.plots(glm.fit, glm.diag)
  })
})