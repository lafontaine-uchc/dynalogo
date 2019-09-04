seq_processing <- function(temp_data,seq_len){
    #removes sequences which are either too long or too short
    #browser()
    #temp_data$new_sequence<-gsub("-","",as.character(temp_data$Sequence))
    #browser()
    temp_data$length<-nchar(temp_data$Sequence)
    temp_data<-temp_data[temp_data$length==input$sequence_length,]
    temp_data    
}
add_missing_AA_rows<- function(PSSM){
    character_library <- AA_STANDARD
    missing_rows_index <- !(character_library %in% rownames(PSSM))
    if (sum(missing_rows_index >0)){
        missing_row_characters<-character_library[missing_rows_index]
        missing_rows = matrix(rep(0, length(missing_row_characters) * ncol(PSSM)),
                              nrow=length(missing_row_characters),
                              dimnames = list(missing_row_characters))
        PSSM<-rbind(PSSM, missing_rows)
        PSSM<-PSSM[order(row.names(PSSM)), ]
    }
    PSSM
}
create_PPM<- function(PFM){
    PPM<-apply(PFM,2, function(pos_count){pos_count/sum(pos_count)})
    PPM
}

remove_non_AA_rows <- function(PSSM){
    character_library <- AA_STANDARD
    PSSM<-PSSM[character_library,]
    PSSM
}
logo_data<-reactive({
    #seq_processing(filtered_data(),input$sequence_length)
    data_sorted()
})
background_data <- reactive({
    #seq_processing(get_avgs(),input$sequence_length)
    get_avgs()
})
background_rates<- reactive({
#    background_data()$Sequence
    string_set <- Biostrings::AAStringSet(background_data()$Sequence)
    PSSM <- Biostrings::consensusMatrix(string_set, baseOnly=TRUE, as.prob = FALSE)
    PSSM <- add_missing_AA_rows(PSSM)
    PSSM <- remove_non_AA_rows(PSSM)
    PSSM <- create_PPM(PSSM)
})
foreground_rates<- reactive({
    #    background_data()$Sequence
    string_set <- Biostrings::AAStringSet(logo_data()$Sequence)
    PSSM <- Biostrings::consensusMatrix(string_set, baseOnly=TRUE, as.prob = FALSE)
    PSSM <- add_missing_AA_rows(PSSM)
    PSSM <- remove_non_AA_rows(PSSM)
    PSSM <- create_PPM(PSSM)
})
KL_logo_heights<- reactive({
    if(input$custom_background == "Experimental"){
        w <- log2(foreground_rates()/background_rates())
    }
    if(input$custom_background == "Natural Occurrance"){
        values<-c(0.0755236,0.0515842,0.0453131,0.0530344,0.0169811,0.0402483,
                  0.0632002,0.0684442,0.0224067,0.0573156,0.0934327,0.0594192,
                  0.0235696,0.0407819,0.0492775,0.0722465,0.0574747,0.0125173,
                  0.0319968,0.0652477)
        nams<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
        gen_bg<-matrix(rep(x = values,ncol(foreground_rates())),nrow = 20,dimnames = list(nams))
        w <- log2(foreground_rates()/gen_bg)
    }
    if(input$custom_background == "Even Distribution"){
        gen_bg2 = matrix(rep(1/20,
                        nrow(foreground_rates()) * ncol(foreground_rates())),
                        nrow=nrow(foreground_rates()),
                        dimnames = list(rownames(foreground_rates())))
        w <- log2(foreground_rates()/gen_bg2)
    }

    w[foreground_rates()==0] = -99.999
    pw <- foreground_rates()*w
    bh<-apply(pw, 2, sum)
    bbh<-matrix(bh,nrow=20,ncol=length(bh),byrow=TRUE)
    height = bbh*foreground_rates()*sign(w)
    height
})
gglogo<-reactive({
    logo = ggseqlogo(KL_logo_heights(), method='custom', seq_type='aa') + 
        geom_hline(yintercept=0, linetype = "dashed") +
        scale_y_continuous(name="Bits", breaks = c(-4,-3,-2,-1,0,1,2,3,4), limits = c(-5,5))
    logo
})
output$test_logo<- renderPlot({
    gglogo()
})

callModule(downloadFigure,"logo",gglogo)
