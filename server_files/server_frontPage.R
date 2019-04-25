output$version_info<-downloadHandler(
    filename<- "./temp/sessionInfo.txt",
    content<- function(file){
        writeLines(capture.output(sessionInfo()), "./temp/sessionInfo.txt")
        file.copy("./temp/sessionInfo.txt",file)
    },
    contentType = "txt/plain"
)
output$splashImage <- renderImage({
    list(src = "./SplashImage.jpg",
         contentType = "image/jpg",
         alt = "Splash"
    )
},deleteFile = FALSE)
output$logoGif <- renderImage({
    list(src = "./resized.gif",
         contentType = "image/gif",
         alt = "logoGif"
    )
},deleteFile = FALSE)
output$manual_download<-downloadHandler(
    filename<- "manual.pdf",
    content<- function(file){
        file.copy("manual.pdf",file)
    },
    contentType = "application/pdf"
)
output$demo_data_download<-downloadHandler(
    filename<- "demo_data.zip",
    content<- function(file){
        file.copy("demo_data.zip",file)
    },
    contentType = "application/zip"
)