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
  
  # output$candDataPrompts <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   if (is.null(input$numCandidates)) return(NULL)
  #   numCandidates <- as.integer(input$numCandidates)
  #   items=names(df)
  #   names(items)=items
  #   
  #   lapply(1:numCandidates, function(i) {
  #     varName1 <- paste("dependent",i, sep = "")
  #     text1 <- paste("Candidate ", i, " data: ", sep= "")
  #     selectInput(varName1,text1,items)
  #   })
  # 
  # })
  # 
  # output$candNamePrompts <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   if (is.null(input$numCandidates)) return(NULL)
  #   numCandidates <- as.integer(input$numCandidates)
  #   items=names(df)
  #   names(items)=items
  #   
  #   lapply(1:numCandidates, function(i) {
  #     varName2 <- paste("candidate",i, sep = "")
  #     text2 <- paste("Name of candidate ", i, ": ", sep= "")
  #     textInput(varName2, text2)
  #   })
  # })
  
  ##Prompts for candidate data (column names) and names
  output$dependent1 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent1','Candidate 1 data:',items, selected='pct_for_hardy2')  #CHANGE SELECTED LATER
  })

  output$candName1 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate1', 'Name of candidate 1:', value='Hardy')  #CHANGE VALUE LATER
  })

  output$dependent2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent2','Candidate 2 data:',items, selected='pct_for_kolstad2') #CHANGE SELECTED LATER
  })

  output$candName2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate2', 'Name of candidate 2:', value='Kolstad') #CHANGE VALUE LATER
  })

  output$dependent3 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('dependent3','Candidate 3 data:',items, selected='pct_for_nadeem2')
  })

  output$candName3 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('candidate3', 'Name of candidate 3:', value = 'Nadeem')
  })
  
  
  ##Prompts for race data and names
  
  # output$groupDataPrompts <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   if (is.null(input$numRaces)) return(NULL)
  #   numRaces <- as.integer(input$numRaces)
  #   items=names(df)
  #   names(items)=items
  #   
  #   lapply(1:numRaces, function(i) {
  #     varName1 <- paste("independent",i, sep = "")
  #     text1 <- paste("Demographic variable ", i, " data: ", sep= "")
  #     selectInput(varName1,text1,items)
  #   })
  #   
  # })
  # 
  # output$groupNamePrompts <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   if (is.null(input$numRaces)) return(NULL)
  #   numRaces <- as.integer(input$numRaces)
  #   items=names(df)
  #   names(items)=items
  #   
  #   lapply(1:numRaces, function(i) {
  #     varName2 <- paste("raceName",i, sep = "")
  #     text2 <- paste("Name of demographic group ", i, ": ", sep= "")
  #     textInput(varName2, text2)
  #   })
  # })
  
  output$independent1 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent1', 'Demographic variable 1:', items, selected='pct_ind_vote') #CHANGE SELECTED LATER
  })

  output$raceVar1 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('raceName1', 'Name of demographic group 1:', value='Indian') #CHANGE VALUE LATER
  })

  output$independent2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent2', 'Demographic variable 2:', items, selected='pct_e_asian_vote')
  })

  output$raceVar2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('raceName2', 'Name of demographic group 2:', value = "East Asian")
  })

  output$independent3 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput('independent3', 'Demographic variable 3:', items, selected='pct_non_asian_vote')
  })

  output$raceVar3 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    textInput('raceName3', 'Name of demographic group 3:', value = "Non-Asian")
  })
  
  
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
    
    ####
    #### rxc Ecological Regression
    ####
    # goodman estimates
    form_indep <- NULL
    for(i in 1:(length(races)-1)){
      if(i == (length(races)-1)){
        new_form_indep <- paste(races[i])
      }else{
        new_form_indep <- paste(races[i], " + ", sep = "")
      }
      form_indep <- paste(form_indep, new_form_indep, sep = "")
    }
    
    #creating formulas and models
    forms <- list()
    mod <- list()
    for(j in 1:length(cands)){
      #j <- 1
      forms[[j]] <- paste(cands[j], " ~ ", form_indep, sep = "")
      forms[[j]] <- as.formula(forms[[j]])
      mod[[j]] <- lm(forms[[j]], data = df)
    }
    
    full_tab <- NULL
    cand_dat <- NULL
    for(i in 1:length(cands)){
      #i <- 1
      coeff <- as.numeric(summary(mod[[i]])$coefficients[,1])
      cand_dat <- NULL
      for(j in 1:length(coeff)){
        if(j == length(coeff)){
          new_row <- coeff[1]
        }else{
          new_row <- coeff[1] + coeff[j+1]
        }
        cand_dat <- rbind(cand_dat, new_row)
      }
      full_tab <- cbind(full_tab, cand_dat)
    }
    
    full_tab <- round(full_tab, 3)
    
    rownames(full_tab) <- table_names
    colnames(full_tab) <- candidate_name
    
    full_tab <- cbind(table_names,full_tab)
    colnames(full_tab)[1] <- "Demographic Group"
    
    # generates goodman plot
    # left to do
    
    ####
    #### rxc Ecological Inference
    ####
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
    
    
    list(gr.tab = full_tab,ei.table = ei.df, ei.plot = comb_plot) 
  }
  
  #RxC case
  
  model_rc <- eventReactive(input$action, {
    if (input$numRaces < 2) return(NULL)
    run_model_rc(independents(),dependents())
  })
  
  output$gr_rc <- renderTable({
    # generates table
    req(input$action)
    model_rc()$gr.tab}, align='c', digits=3)
  
  output$est_rc <- renderTable({
    # generates table
    req(input$action)
    model_rc()$ei.table}, align='c', digits=3)
  
  observeEvent(input$action, {
    # generates EI bounds plot
    output$ei.bounds_rc <- renderPlot({
      plot(model_rc()$ei.plot)
    }, width=650, height=800)
  })
  
  output$ei.compare <- renderTable({
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

    output$est_expl <- renderUI({
      HTML(paste("First, we compare predictions from three different models for 
                 each candidate's vote share given demographic and total vote data.", "<br/>","<br/>"))
    })
    
    output$bounds_expl <- renderUI({ 
      HTML(paste("<br/>","Finally, we calculate ecological inference predictions for each candidate's vote share and plot them with credible intervals. These credible intervals
                 give us ranges of possible vote shares by race. We are 95% confident that the true vote shares for each candidate will fall in these", input$numCandidates, "ranges. In other 
                 words, if we did 100 ecological inference predictions, 95 times out of 100, the vote share would fall in these intervals. <br/> <br/>", "<br/>","<br/>"))
    })
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