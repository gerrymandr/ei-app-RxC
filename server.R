library(shiny)
library(shinydashboard)
library(ggplot2)
library(ei)
library(eiPack)
library(eiCompare)
library(shinycssloaders)
library(gridExtra)

shinyServer(function(input, output, session) {
  
  url1 <- a("King's EI page", href='https://gking.harvard.edu/category/research-interests/methods/ecological-inference')
  output$king <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url1))
  })
  
  url2 <- a('Notes from Gingles Expert Witness (.pdf)', href='http://www.socsci.uci.edu/~bgrofman/74%20Grofman%201992.%20Expert%20Witness%20Testimony....pdf')
  output$groffman <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url2))
  })
  
  url3 <- a('Blacksher & Menefee (HeinOnline)', href='http://heinonline.org/HOL/LandingPage?handle=hein.journals/hastlj34&div=9&id=&page=')
  output$blacksher <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url3))
  })
  
  filedata <- reactive({ # Take in file
    req(input$file1) # require that the input is available
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, stringsAsFactors=F)
  })
  
  output$numCandidates <- renderUI({ #Prompt for number of candidates
    df <- filedata()
    if (is.null(df)) return(NULL)
    numericInput("numCandidates", label = "Number of candidates:", value = 3, min = 2, max = 20, step=1)
  })
  
  output$numRaces <- renderUI({ #Prompt for number of races
    df <- filedata()
    if (is.null(df)) return(NULL)
    numericInput("numRaces", label = "Number of minority demographic groups:", value =3, min = 2, max = 20, step=1)

  })
  
  output$candDataPrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numCandidates)) return(NULL)
    numCandidates <- as.integer(input$numCandidates)
    items=names(df)
    names(items)=items
    
    lapply(1:numCandidates, function(i) {
      varName1 <- paste("dependent",i, sep = "")
      text1 <- paste("Candidate ", i, " data: ", sep= "")
      selectInput(varName1,text1,items)
    })

  })
  
  output$candNamePrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numCandidates)) return(NULL)
    numCandidates <- as.integer(input$numCandidates)
    items=names(df)
    names(items)=items
    
    lapply(1:numCandidates, function(i) {
      varName2 <- paste("candidate",i, sep = "")
      text2 <- paste("Name of candidate ", i, ": ", sep= "")
      textInput(varName2, text2)
    })
  })
  
  ##Prompts for candidate data (column names) and names
  # output$dependent1 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent1','Candidate 1 data:',items, selected='pct_for_hardy2')  #CHANGE SELECTED LATER
  # })
  # 
  # output$candName1 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate1', 'Name of candidate 1:', value='Hardy')  #CHANGE VALUE LATER
  # })
  # 
  # output$dependent2 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent2','Candidate 2 data:',items, selected='pct_for_kolstad2') #CHANGE SELECTED LATER
  # })
  # 
  # output$candName2 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate2', 'Name of candidate 2:', value='Kolstad') #CHANGE VALUE LATER
  # })
  # 
  # output$dependent3 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('dependent3','Candidate 3 data:',items, selected='pct_for_nadeem2')
  # })
  # 
  # output$candName3 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   textInput('candidate3', 'Name of candidate 3:', value = 'Nadeem')
  # })
  
  
  ##Prompts for race data and names
  
  output$groupDataPrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numRaces)) return(NULL)
    numRaces <- as.integer(input$numRaces)
    items=names(df)
    names(items)=items
    
    lapply(1:numRaces, function(i) {
      varName1 <- paste("independent",i, sep = "")
      text1 <- paste("Demographic variable ", i, " data: ", sep= "")
      selectInput(varName1,text1,items)
    })
    
  })
  
  output$groupNamePrompts <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$numRaces)) return(NULL)
    numRaces <- as.integer(input$numRaces)
    items=names(df)
    names(items)=items
    
    lapply(1:numRaces, function(i) {
      varName2 <- paste("raceName",i, sep = "")
      text2 <- paste("Name of demographic group ", i, ": ", sep= "")
      textInput(varName2, text2)
    })
  })
  
  # output$independent1 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('independent1', 'Demographic variable 1:', items, selected='pct_ind_vote') #CHANGE SELECTED LATER
  # })
  # 
  # output$raceVar1 <- renderUI({
  #   df <- filedata()  
  #   if (is.null(df)) return(NULL)
  #   textInput('raceName1', 'Name of demographic group 1:', value='Indian') #CHANGE VALUE LATER
  # })
  # 
  # output$independent2 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('independent2', 'Demographic variable 2:', items, selected='pct_e_asian_vote')
  # })
  # 
  # output$raceVar2 <- renderUI({
  #   df <- filedata()  
  #   if (is.null(df)) return(NULL)
  #   textInput('raceName2', 'Name of demographic group 2:', value = "East Asian")
  # })
  # 
  # output$independent3 <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   items=names(df)
  #   names(items)=items
  #   selectInput('independent3', 'Demographic variable 3:', items, selected='pct_non_asian_vote')
  # })
  # 
  # output$raceVar3 <- renderUI({
  #   df <- filedata()  
  #   if (is.null(df)) return(NULL)
  #   textInput('raceName3', 'Name of demographic group 3:', value = "Non-Asian")
  # })
  
  
  ##Prompts for total votes
  
  output$tot.votes <- renderUI({ #Prompt for column to use for total votes
    df <- filedata()
    if(is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('tot.votes', 'Total votes cast:',items, selected='total2') #CHANGE SELECTED LATER
  })
  
  output$ui.slider <- renderUI({
    if (is.null(input$file1)) return()
    sliderInput('slider', 'Homogeneous precincts threshold', width='100%', min=0, max=25, step=1, ticks=T, post='%', value=5)
  })
  
  output$ui.action <- renderUI({
    if (is.null(input$file1)) return()
    actionButton('action', ' Run', icon('refresh', lib='glyphicon'))
  })
  
  dependents <- eventReactive(input$action, {
    numCandidates <- input$numCandidates
    cands <- c()
    candNames <- c()
    for(i in 1:numCandidates){
      cands <- c(cands, input[[paste("dependent",i,sep="")]])
      candNames <- c(candNames, input[[paste("candidate",i,sep="")]])
    }
    list(cands = cands, candNames = candNames, numCandidates = numCandidates)
    
  })
  
  independents <- eventReactive(input$action, {
    numRaces <- input$numRaces
    groups <- c()
    groupNames <- c()
    for(i in 1:numRaces){
      groups <- c(groups, input[[paste("independent",i,sep="")]])
      groupNames <- c(groupNames, input[[paste("raceName",i,sep="")]])
    }
    list(groups=groups, groupNames = groupNames, numRaces = numRaces)
    
  })
  
  run_model_22 <- function(independent, dependent, tot.votes, candidate){
    # Function that generates the table, goodman plot, and EI metric (with confidence plot), given variables
    
    df <- filedata()[,c(independent, dependent, tot.votes)]
    names(df) <- c('x', 'y', 'z')
    
    # homogeneous precincts
    df <- df[order(df$x),]
    hp <- round(input$slider/100*dim(df)[1], digits=0)
    hp.low <- 1:hp
    hp.high <- (dim(df)[1]-hp+1):dim(df)[1]
    
    df$threshold <- 0
    df$threshold[hp.low] <- 1
    df$threshold[hp.high] <-1
    
    df$hp <- NA
    df$hp[hp.low] <- 1
    df$hp[hp.high] <- 1
    
    df$hp.text <- NA
    df$hp.text[hp.low] <- 'low'
    df$hp.text[hp.high] <- 'high'
    
    hp.low.mean <- mean(df$y[df$hp.text=='low'], na.rm=T)
    hp.high.mean <- mean(df$y[df$hp.text=='high'], na.rm=T)
    
    # goodman estimates
    ger <- lm(y~x, data=df)
    
    # ei estimate for table and confidence interval
    table.names <- c('ei.minority', 'ei.white')
    ei.out <- ei_est_gen('y', '~ x', 'z',
                         data = df[,c(1:3),], table_names = table.names, sample=1000) # eiCompare
    #ei.out <- ei(y~x, total=input$tot.votes, data=df) # ei
    edf.t <- data.frame(w=c(paste('All but ', input$raceName, ' support', sep=''),
                            hp.low.mean,
                            ger$coefficients[1],
                            ei.out$ei.white[1]/100,
                            ei.out$ei.white[2]/100),
                        m=c(paste(input$raceName, ' support', sep=''),
                            hp.high.mean,
                            ger$coefficients[1]+ger$coefficients[2],
                            ei.out$ei.minority[1]/100,
                            ei.out$ei.minority[2]/100))
    row.names(edf.t) <- c(candidate, 'Homogeneous precincts', 'Goodman ER', 'Ecol Inf', 'EI.se')
    
    # generates goodman plot
    gr.plot <- ggplot(df, aes(x=x,y=y)) +
      xlab(independent) + ylab(dependent) +
      geom_smooth(method='lm', se=T, colour='black', fullrange=TRUE) +
      scale_x_continuous(expand=c(0,0), limits=c(0,1)) +
      scale_y_continuous(expand=c(0,0), limits=c(-1.5,1.5)) +
      coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
      geom_point(size=3, aes(colour=as.factor(df$threshold))) +
      geom_point(pch=1, size=3) +
      geom_point(pch=1, size=5, aes(colour=as.factor(df$hp))) +
      scale_color_manual('Homogeneous precincts', breaks=c(0,1), values=c('Gray', 'Red'), labels=c('No', paste('Most extreme ', input$slider,'%', sep=''))) +
      geom_hline(yintercept=0.5, linetype=2, colour='lightgray') +
      theme_bw() + ggtitle("Goodman's Ecological Regression") + labs(x = paste('% population ', input$raceName, sep=''),
                                                                     y= paste('% vote for ', candidate, sep=''),
                                                                     caption = paste('Election data from', input$electionsource, 'and demographic data from', input$demsource, sep = ' '))
    
    # generates ei table
    ei.table <- as.data.frame(t(edf.t))
    for(i in 2:5){
      ei.table[,i] <- as.numeric(as.character(ei.table[,i]))
    }
    ei.table.final <- ei.table[,c(1:4)]
    
    # original data with ei estimates
    #df.ei <- df[,c(1:3)]
    #df.ei$EI.est.min <- eiread(ei.out, 'betab')
    #df.ei$EI.est.white <- eiread(ei.out, 'betaw')
    
    # generates ei dotplot
    
    ei.plot.df <- ei.table[,c(1,4,5)]
    names(ei.plot.df) <- c('race', 'ei.est', 'ei.se')
    
    ei.plot <- ggplot(ei.plot.df, aes(x=ei.est, y=1, col=as.factor(race))) +
      geom_hline(yintercept=1, col='black') +
      geom_point(size=6, shape=3) + labs(y=(''), x = paste('Support for candidate ', candidate, sep=''),
                                         caption = paste('Election data from', input$electionsource, 'and demographic data from', input$demsource, sep = ' '))  + scale_x_continuous(limits=c(-.25,1.25)) +
      scale_y_continuous(limits=c(0,2), breaks=c(0,0.5,1,1.5,2), labels=c('','','','','')) +
      scale_color_manual('Race', values=c('gray40', 'midnightblue'), labels=c(paste('All but ', input$raceName, sep=''), input$raceName)) +
      geom_errorbarh(aes(xmin=(ei.est) - 2*(ei.se), xmax=(ei.est) + 2*(ei.se), height=0.3), size=2, alpha=0.7, height=0.3) +
      theme_bw() + ggtitle('Ecological Inference')
    
    
    list(gr.plot = gr.plot, ei.table = ei.table.final, ei.plot = ei.plot)
  }
  
  run_model_rc <- function(independents, dependents){
    # Function that generates the table, goodman plot, and EI metric (with confidence plot), given variables
    
    # dep_vec <- NULL
    # #numCandidates <- 3
    # for(i in 1:numCandidates){
    #   dep_vec <- c(dep_vec, paste("input$dependent", i, sep = ""))
    # }
    
    columns <- c(independents[[1]], dependents[[1]])
    
    df <- filedata()[,columns]
    
    cands <- unlist(dependents$cands)

    candidate_name <- unlist(dependents$candNames)
    
    table_names <- unlist(independents$groupNames)
    races <- unlist(independents$groups)
    
    #make sure all of the demographics add up to 1
    #df$allbut <- rep(1,nrow(df)) - sum(df[,1:5])
    
    # Generate formula for passage to ei.reg.bayes() function
    form_start <- paste("cbind(")
    form_end <- paste(")")
    form_dep <- NULL
    for(i in 1:length(cands)){
      if(i == length(cands)){
        new_form_dep <- paste(cands[i])
      }else{
        new_form_dep <- paste(cands[i], ", ", sep = "")
      }
      form_dep <- paste(form_dep, new_form_dep, sep = "")
    }
    
    form_indep <- NULL
    for(i in 1:length(races)){
      if(i == length(races)){
        new_form_indep <- paste(races[i])
      }else{
        new_form_indep <- paste(races[i], ", ", sep = "")
      }
      form_indep <- paste(form_indep, new_form_indep, sep = "")
    }
    
    form_comp <- paste(form_start, form_dep, form_end, "~", form_start, form_indep, form_end, sep = "")
    
    form <- as.formula(form_comp)
    
    # Run Bayesian model
    ei_bayes <- ei.reg.bayes(form, data=df, sample=10, truncate=TRUE)
    
    # Table Creation, using function bayes_table_make
    ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector= cands, table_names = table_names)
    
    ei.df <- NULL
    for(i in 1:length(races)){
      for(k in 1:length(cands)){
        new_row <- c(paste('Candidate', candidate_name[i], sep=' '),
                     paste(table_names[k], ' support', sep=''),
                     ei_bayes_res[k,i+1]/100,
                     ei_bayes_res[2*k,i+1]/100)
        ei.df <- rbind(ei.df, new_row)
      }
    }
    colnames(ei.df) <- c("Candidate", "Race", "Estimate", "Se")
    rownames(ei.df) <- c()
    ei.df <- as.data.frame(ei.df)
    ei.df$Estimate <- round(as.numeric(as.character(ei.df$Estimate)), 4)
    ei.df$Se <- round(as.numeric(as.character(ei.df$Se)), 4)
    
    base_plot <- ggplot()  +
      scale_x_continuous(limits=c(-1,2))+
      scale_y_continuous(limits=c(0,2))
    
    #setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    #setSessionTimeLimit(cpu = Inf, elapsed = Inf)
    for(i in 1:length(cands)){
      plot_dat <- ei.df[(i*length(races)-length(races)+1):(i * length(races)),]
      plot_dat <- as.data.frame(plot_dat)
      plot_dat$Estimate <- as.numeric(as.character(plot_dat$Estimate))
      plot_dat$Se <- as.numeric(as.character(plot_dat$Se))
      new_plot <- base_plot +
        geom_hline(yintercept=1, col='black') +
        geom_point(data = plot_dat, aes(x = Estimate, y = 1, col = as.factor(Race)),size=6, shape=3) +
        #ylab('') + xlab(paste('Support for candidate ', candidate, sep='')) +
        #labels=c('','','','','')) #+
        #scale_color_manual('Race', values=c('gray40', 'midnightblue'), labels=c(paste('All but ', input$raceName, sep=''), input$raceName)) +
        geom_errorbarh(data = plot_dat, aes(x = Estimate, y = 1, xmin=(Estimate) - 2*(Se), xmax=(Estimate) + 2*(Se), 
                                            height=0.3, col = as.factor(Race)), size=2, alpha=0.7, height=0.3) +
        theme_bw() + ggtitle(paste('Ecological Inference for Candidate', candidate_name[i]))+
        guides(col=guide_legend(title="Group Support"))
      
      if(i == 1){
        comb_plot <- new_plot
      }else{
        comb_plot <-  grid.arrange(comb_plot, new_plot, nrow = 2, heights = c(i-1, 1))
      }
    }
    
    
    list(ei.table = ei.df, ei.plot = comb_plot) 
  }
  
  # 2x2 case
  
  # models <- eventReactive(input$action, {
  #   models <- list()
  #   for(i in 1:dependents()$numCandidates){
  #     #name = paste("model",i, sep = "")
  #     new <- run_model_22(independents()$groups[1], dependents()$cands[i],
  #                                  input$tot.votes, dependents()$candNames[i])
  #     models[[i]] <- new
  #   }
  #   models
  # })
  # 
  # 
  # output$goodmanPlots = renderPlot({
  #   req(input$action)
  #   ptlist <- list()
  #   for(i in 1:input$numCandidates){
  #     ptlist[[i]] <- models()[[i]]$gr.plot
  #   }
  # 
  #   grid.arrange(grobs=ptlist)
  # })
  # 
  # output$ests = renderTable({
  #   req(input$action)
  #   tt1 <- ttheme_default()
  #   ptlist <- list()
  #   for(i in 1:input$numCandidates){
  #     ptlist[[i]] <- tableGrob(models()[[i]]$ei.table, theme = tt1)
  #   }
  #   
  #   grid.arrange(grobs=ptlist, as.table=TRUE)
  # })
  
  
  #RxC case
  
  model_rc <- eventReactive(input$action, {
    if (input$numRaces < 2) return(NULL)
    run_model_rc(independents(),dependents())
  })
  
  
  output$est_rc <- renderTable({
    # generates table
    if (input$numRaces < 2) return(NULL)
    req(input$action)
    model_rc()$ei.table}, align='c', digits=3)
  
  observeEvent(input$action, {
    # generates EI bounds plot
    if (input$numRaces < 2) return(NULL)
    output$ei.bounds_rc <- renderPlot({
      plot(model_rc()$ei.plot)
    }, width=650, height=800)
  })
  
  output$ei.compare <- renderTable({
    if (input$numRaces < 2) return(NULL)
    filedata()}, spacing = "xs")
  
  output$template <- downloadHandler(
    filename = "template.docx",
    content = function(file) {
      file.copy("ExpertWitnessTemplate.docx", file)
    }
  )
  
  output$welcome <- renderUI({
    req(is.null(input$file1)) # require that the input is null
    HTML(paste("<br/><br/><br/><br/><br/><br/>", tags$h2(tags$b("Welcome"), align="center"),
               tags$h5(tags$i("No data is currently loaded."), align="center") ))
  })
  
  observeEvent(input$action, {
    output$report <- downloadHandler(
      filename = "report.pdf",
      
      
      content = function(file) {
        
        #copy report to temporary file
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        
        # Knit the document, passing in the `params` list
        rmarkdown::render(tempReport, output_file = file,
                          params = list(file1 = input$file1,
                                        independent = input$independent1,
                                        dependent1 = input$dependent1,
                                        dependent2 = input$dependent2,
                                        tot.votes = input$tot.votes,
                                        candidate1 = input$candidate1,
                                        candidate2 = input$candidate2,
                                        input_slider = input$slider,
                                        raceName = input$raceName),
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })
  
})