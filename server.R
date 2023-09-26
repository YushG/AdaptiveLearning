# ======================================Server Logic===============================================================

server <- function(input, output, session) {
  
  doOne <- reactiveValues(time = TRUE)
  rerun <- reactiveValues(analytics = TRUE, my_data = NULL)
  my_data_nrow <- reactiveValues(total = 0)
  search_nrow <- reactiveValues(entries = 0)
  irrelevant_words <- c("of", "in", "and", "the", "with", "have", "has", "is")
  
  # ====================================Observe Events=============================================================
  
  # Positive Button
  observeEvent(input$pos, {
    store <- searchIt(my_data)
    
    if(input$filter) {
      # On
      my_data[store == TRUE & my_data$userInput == "", c("userInput")] <<- "Positive"
    } else {
      # Off
      my_data[store == TRUE & my_data$userInput != "" & my_data$userInput != "Positive", c("flag")] <<- "✅ " 
      my_data[store == TRUE, c("userInput")] <<- "Positive"
    }
    
    search_set$out <- "Positive"
    search_set$count <- nrow(my_data[store == TRUE & my_data$userInput == "Positive",])
    my_list <<- rbind(my_list, unlist(search_set))
    click("view") # Dynamic refresh
    rerun$analytics <- TRUE # Analytics refresh
  })
  
  
  # Negative Button
  observeEvent(input$neg, {
    store <- searchIt(my_data)
    
    if(input$filter) {
      # On
      my_data[store == TRUE & my_data$userInput == "", c("userInput")] <<- "Negative"
    } else {
      # Off
      my_data[store == TRUE & my_data$userInput != "" & my_data$userInput != "Negative", c("flag")] <<- "✅ " 
      my_data[store == TRUE, c("userInput")] <<- "Negative"
    }
    
    search_set$out <- "Negative"
    search_set$count <- nrow(my_data[store == TRUE & my_data$userInput == "Negative",])
    my_list <<- rbind(my_list, unlist(search_set))
    click("view") # Dynamic refresh
    rerun$analytics <- TRUE # Analytics refresh
  })
  
  
  # Ignore Button
  observeEvent(input$ig, {
    p_len <- nrow(my_data)
    store <- searchIt(my_data)
    
    if(input$filter) {
      my_data <<- my_data[store == FALSE | my_data$userInput != "",]
    } else {
      my_data <<- my_data[store == FALSE,]
    }
    
    my_data_nrow$total <- nrow(my_data)
    showNotification(sprintf("Ignored %d rows.", p_len - my_data_nrow$total), type = "message", closeButton = TRUE)
    rerun$analytics <- TRUE # Analytics refresh
  })
  
  
  # Clear Button
  observeEvent(input$clear, {
    store <- searchIt(my_data)
    
    my_data[store == TRUE, c("userInput")] <<- ""
    
    search_set$out <- ""
    search_set$count <- nrow(my_data[store == TRUE & my_data$userInput == "",])
    my_list <<- rbind(my_list, unlist(search_set))
    click("view") # Dynamic refresh
    rerun$analytics <- TRUE # Analytics refresh
  })
  
  
  # Questionable Button
  observeEvent(input$question, {
    store <- searchIt(my_data)
    
    if(input$filter) {
      # UserInput is empty, so no effect to flag
      my_data[store == TRUE & my_data$userInput == "", c("userInput")] <<- "?"
    } else {
      # Off
      my_data[store == TRUE, c("userInput")] <<- "?"
    }
    
    search_set$out <- "?"
    search_set$count <- nrow(my_data[store == TRUE & my_data$userInput == "?",])
    my_list <<- rbind(my_list, unlist(search_set))
    click("view") # Dynamic refresh
    rerun$analytics <- TRUE # Analytics refresh
  })
  
  
  # Suggest for Pre
  observeEvent(input$sgt1, {
    predictWord <- predictIt(input$no1, input$aspectText)
    updateTextInput(session, "preText", value = paste(predictWord))
    
    predictPremod <- predictIt(1, predictWord)
    updateTextInput(session, "premodText", value = paste(predictPremod))
  })
  
  
  # Suggest for Post
  observeEvent(input$sgt2, {
    predictWord <- predictIt(input$no2, input$aspectText, pre=FALSE)
    updateTextInput(session, "postText", value = paste(predictWord))
    
    predictPostmod <- predictIt(1, predictWord)
    updateTextInput(session, "postmodText", value = paste(predictPostmod))
  })
  
  
  # Suggest for Aspect
  observeEvent(input$sgt3, {
    premodSearch <- input$premodText
    preSearch <- input$preText
    postSearch <- input$postText
    postmodSearch <- input$postmodText
    
    all_sentences <- grepl(paste0(premodSearch,"(.*?)",preSearch,"(.*?)",postSearch,"(.*?)",postmodSearch),  my_data[my_data$userInput == "", ]$value, ignore.case = TRUE, perl = TRUE)
    sentences <- my_data[all_sentences == TRUE, c("value")]
    
    value_list <- c()
    for(sentence in sentences) {
      substr <- str_extract(string = sentence, pattern = paste0("(?<=",preSearch,").*(?=",postSearch,")"))
      value_list <- c(value_list, substr)
    }
    
    top_word <- ""
    if(length(value_list) >= 1) {
      dft_data <- frqE(value_list, TRUE)
      colnames(dft_data) <- list("Word", "Frequency")
      dft_data <- filter(dft_data, !Word %in% irrelevant_words)
      setorderv(dft_data, c('Frequency'), c(-1))
      top_word <- dft_data[[1]][1]
    }
    
    updateTextInput(session, "aspectText", value = paste(top_word))
    
  })
  
  
  # Save Labeled data
  observeEvent(input$save, {
    # Take my_data and save to csv
    filename <- paste0(format(Sys.time(), "AdaptiveLearning_UserResult-%m-%d-%Y.csv"))
    my_data = dcast(my_data, index_hl + impression ~ variable, fun.aggregate = function (x) paste0(x,collapse=" "), value.var = "userInput")
    write.csv(my_data, filename)
    showNotification(sprintf("Saved file as %s", filename))
  })
  
  
  # Save Summary
  observeEvent(input$save_summary, {
    # Take my_list and save to csv
    filename <- paste0(format(Sys.time(), "AdaptiveLearning_UserSummary-%m-%d-%Y.csv"))
    write.csv(my_list, filename)
    showNotification(sprintf("Saved file as %s", filename))
  })
  
  
  # Load Summary
  observeEvent(input$load_summary, {
    inFile <- input$load_summary
    
    if (is.null(inFile))
      return(NULL)
    
    content<-read.csv(inFile$datapath)
    content[is.na(content)] <- ""
    
    for(i in 1:nrow(content)) {
      entry <- content[i,]
      my_list <<- rbind(my_list, unlist(entry))
      store <- searchIt(my_data, entry)
      my_data[store == TRUE, c("userInput")] <<- entry$out
    }
  })
  
  
  # Number of rows returned after search
  observe(output$numberRows <- renderText({
    sprintf("Rows: %s", my_data_nrow$total)
  }))
  
  # Number of rows returned after search
  observe(output$dataTableRows <- renderText({
    sprintf("%s entries", search_nrow$entries)
  }))
  
  # Display for Other Tabs
  observeEvent(input$tabs, {
    if(input$tabs == "User Summary") {
      output$summary <- renderDataTable({
        as.data.frame(my_list)
      }, options=list(searching = FALSE ))
    }
    else if(input$tabs == "Aspect Frequency") {
      output$analytics <- renderDataTable({
        if (rerun$analytics) {
          dft_data <- frqE(my_data[my_data$userInput == "", ])
          
          colnames(dft_data) <- list("Word", "Frequency")
          dft_data <- filter(dft_data, !Word %in% irrelevant_words)
          setorderv(dft_data, c('Frequency'), c(-1))
          
          rerun$analytics <- FALSE # Reset refresh
          rerun$my_data <- dft_data # Cache data
        }
        rerun$my_data # Return output
      }, options=list(searching = FALSE))
    }
    else if(input$tabs == "Graphs") {
      # PA Main
      output$pa_main <- renderPlot({
        req(rerun$my_data)
        d <- rerun$my_data[1:10,]
        d$Word <- factor(d$Word, levels = d$Word) # Preserve order
        ggplot(d, aes(x = c(1:10), y = Frequency)) + xlab("Iterations") +
          geom_point() + geom_text(aes(label=Word), position=position_jitter(width=.5,height=.5)) + ggtitle("PA Vocabulary Expansion") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(color="black", size=14, face="bold.italic"))
      })
      # SA Main
      output$sa_main <- renderPlot({
        req(rerun$my_data)
        d <- findIt(rerun$my_data[1,1])
        d$Word <- factor(d$Word, levels = d$Word) # Preserve order
        ggplot(d, aes(x = c(1:10), y = Frequency)) + xlab("Iterations") +
          geom_point() + geom_text(aes(label=Word), position=position_jitter(width=.5,height=.5)) + ggtitle("SA Vocabulary Expansion") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(color="black", size=14, face="bold.italic"))
      })
      # PA' Main
      output$pa_prime_main <- renderPlot({
        req(rerun$my_data)
        d <- findIt(rerun$my_data[1,1])
        d$Word <- factor(d$Word, levels = d$Word) # Preserve order
        ggplot(d, aes(x = c(1:10), y = Frequency)) + xlab("Iterations") +
          geom_point() + geom_text(aes(label=Word), position=position_jitter(width=.5,height=.5)) + ggtitle("PA' Vocabulary Expansion") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(color="black", size=14, face="bold.italic"))
      })
      
      #SA' Main
      output$sa_prime_main <- renderPlot({
        req(rerun$my_data)
        d <- findIt(rerun$my_data[1,1])
        e <- findIt(d[1,1])
        e$Word <- factor(e$Word, levels = e$Word) # Preserve order
        ggplot(e, aes(x = c(1:10), y = Frequency)) + xlab("Iterations") +
          geom_point() + geom_text(aes(label=Word), position=position_jitter(width=.5,height=.5)) + ggtitle("SA' Vocabulary Expansion") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(color="black", size=14, face="bold.italic"))
      })
      
    }
  })
  
  
  # ==================================Search Inputs======================================================================
  
  text_reactive <- eventReactive(input$view, {
    print("Search View")
    results <- list()
    results$premod <- input$premodText
    results$pre <- input$preText
    results$aspect <- input$aspectText
    results$post <- input$postText
    results$postmod <- input$postmodText
    results$chrlmt <- input$chrlmt
    results$excludePre <- input$excludePre
    results$excludePost <- input$excludePost
    results # Return value
  })
  
  # =================================Functions==========================================================================
  
  findIt <- function(word) {
    d <- predictIt(0, word, pre=FALSE, plot=TRUE)
    d <- d[2:11,]
  }
  
  # Search Function for View
  searchIt <- function(my_data, search = NA) {
    if(class(search) != 'data.frame'){
      search <- text_reactive()
    }
    search_set <<- search
    premodSearch <- search$premod
    preSearch <- search$pre
    aspectSearch <- stri_join_list(lapply(strsplit(search$aspect, ","), trimws), sep = "|")
    postSearch <- search$post
    postmodSearch <- search$postmod
    chrLimit <- search$chrlmt
    excludePre <- search$excludePre
    excludePost <- search$excludePost
    
    if (is.null(chrLimit) || chrLimit < 1) {
      chrLimit <- Inf
    } else {
      chrLimit <- strtoi(chrLimit)
    }
    
    store <- grepl(paste0(premodSearch,"(.*?)",preSearch,"(.*?)",aspectSearch,"(.*?)",postSearch,"(.*?)",postmodSearch), my_data$value, ignore.case = TRUE, perl = TRUE)
    if (excludePre != "" || excludePost != "") {
      store <- store & !grepl(paste0(excludePre,"(.*?)",aspectSearch,"(.*?)",excludePost), my_data$value, ignore.case = TRUE, perl = TRUE)
    }
    lmt <- lapply(my_data$value, nchar) < chrLimit # Apply character limit if necessary
    
    Map('*', store, lmt) # Return search
  }
  
  
  # Suggest Word Function
  predictIt <- function(num, wrd, pre=TRUE, plot=FALSE) {
    # Handle comma in word (aspect)
    wrd <- stri_join_list(lapply(strsplit(wrd, ","), trimws), sep = "|")
    
    if(pre) {
      regexp <- paste0("(?:\\w+\\s+){1,",num,"}(?=\\b",wrd,"\\b)")
    } else if(plot) {
      regexp <- paste0("(?:\\w+\\s+){1,3}(?=\\b",wrd,"\\b)\\s*(?:\\S+\\b\\s*){1,3}")
    } else {
      regexp <- paste0("(?<=\\b",wrd,"\\b)\\s*(?:\\S+\\b\\s*){0,",num,"}")
    }
    ptr <- str_match(tolower(my_data[my_data$userInput == "", ]$value), regexp)
    aptr <- ptr[ptr != "" & !is.na(ptr) ,]
    if (length(aptr) == 0) {
      showNotification(sprintf("No Suggestions."))
      return("")
    }
    aptr_filter <- lapply(aptr, str_extract_all, boundary("word"))
    aptr.freq <- table(unlist(aptr_filter))
    
    rst <- cbind.data.frame(names(aptr.freq), as.integer(aptr.freq))
    colnames(rst) <- list("Word", "Frequency")
    rst <- filter(rst, !Word %in% irrelevant_words)
    rst_sort <- rst[order(-rst[,2]),] # Sort by frequency
    if (plot) {
      return(rst_sort)
    }
    rst_sort[1,1] # Return word with highest frequency: ex. hypermetabolic
  }
  
  
  # Find Frequency for Unique Words
  frqE <- function(my_data, special=FALSE) {
    if(special) {
      words_list_of_lists <- lapply(my_data, str_extract_all, boundary("word")) 
    } else {
      words_list_of_lists <- lapply(my_data$value, str_extract_all, boundary("word"))
    }
    
    # Remove duplicate words in each row
    removeDuplicates <- function(wrd_list) {
      unique(tolower(unlist(wrd_list)))
    }
    
    filtered_words_list_of_lists <- lapply(words_list_of_lists, removeDuplicates)
    
    word_table <- table(unlist(filtered_words_list_of_lists))
    frq_count <- cbind.data.frame(names(word_table), as.integer(word_table))
    frq_count[str_count(names(word_table)) > 1, ]
  }
  
  # =========================================Output Display==================================================================
  
  # Display for First Tab
  output$contents <- renderDataTable({
    file <- req(input$file1)
    
    if (doOne$time) {
      my_data <- read.csv(file$datapath)
      my_list <<- c()
      search_set <<- c()
      s <- as.data.frame(str_split_fixed(my_data$impression, ifelse(is.na(str_extract(my_data$impression, "(?!1\\.\\d)(1\\..*)"))==FALSE, "(?!\\d\\.\\d|\\d\\. \\d\\.)(?=\\d\\.)(?<![A-Z-0-9]|\\d\\.)", "(?<!\\d\\.)(?<=\\.)"), n= Inf))
      my_data <- cbind(my_data, s)
      my_data <- reshape2::melt(my_data, measure.vars=grep("^V", colnames(my_data)), na.rm = T)
      
      my_data$value = str_trim(my_data$value)
      my_data = my_data[!(my_data$value == ""),]
      my_data = my_data[!duplicated(my_data[c("index_hl", "value")]),]
      my_data = my_data[c("index_hl", "impression", "value")]
      
      ##NEED TO MODIFY VIA IGNORE## my_data = my_data[!(duplicated(my_data$value) | duplicated(my_data$value, fromLast = TRUE)), ]
      
      my_data$userInput <- ""
      my_data$flag <- ""
      my_data <<- my_data[order(my_data$index_hl),]
      my_data_nrow$total <- nrow(my_data)
      
      enable("pos")
      enable("neg")
      enable("ig")
      enable("clear")
      enable("question")
      enable("save")
      enable("view")
      enable("sgt1")
      enable("sgt2")
      enable("sgt3")
      doOne$time <- FALSE
      click("view") # Force to show datatable after choosing file
    }
    
    ret_store <- searchIt(my_data)
    
    updateProgressBar(
      session = session,
      id = "percentageLabeled",
      value = (sum(my_data$userInput > 0)/my_data_nrow$total * 100)
    )
    
    if(input$filter) {
      # on
      filter_search <- my_data[ret_store == TRUE & my_data$userInput == "",]
      search_nrow$entries <- nrow(filter_search)
      return(filter_search)
    } else {
      # off
      search_nrow$entries <- nrow(my_data[ret_store == TRUE,])
    }
    
    return(my_data[ret_store == TRUE,])
    
  }, options=list(searching = FALSE, autoWidth=TRUE, columnDefs = list( list(width='45%', targets=c(2)), list(width='20%', targets=c(1)) ))
  )
  
}