# library(shiny)
library(ggplot2)
library(lattice)
# library(ggExtra)
library(dplyr)
library(Rmisc)
library(DT)
library(reshape2)
library(eeptools)
library(platetools)
# library(stringi)
library(circlize)
library(readr)
library(viridis)
library(viridisLite)
library(RColorBrewer)
library(phenoScreen)

function(input, output) {
  # MANUAL DATA PAST - PLATES
  inData<-reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      # return(NULL)
      textdata <- input$platedata_datainput
      console.raw(textdata)
    } else {
      textdata <- read_file(inFile$datapath)
      console.raw(textdata)
    }
    
    if (input$separator == 2) {
        pasted_data <- read.table(header = FALSE, sep = "\t", text = textdata, 
          fileEncoding = "latin1")
      } else if (input$separator == 1) {
        pasted_data <- read.table(header = FALSE, sep = ",", text = textdata, 
          fileEncoding = "latin1")
      } else {
        pasted_data <- textdata
      }
      
    return (pasted_data)
    # } else {
    #   NULL
    # }
  })

    observe({  
      if (input$platedata_submitclick > 0) {
        data1<-inData()
        if(grepl("p", data1, fixed=TRUE)){
            createPlate_List()
          } else {
            writeto.table.platedata(data1)
          }
        
        # writeto.table.platedata(pasted_data)
      } else {
        NULL
      }
    })

    observe({  
      if(!is.null(input$dataset)){
        data<-get_plates(input$dataset)
        data<-as.vector(data)
        # data<-data[-1]
        # console.testdata(data)
        writeto.table.platedata(data)
      } else {
        NULL
      }
      })
    
    output$rawdata3 <- renderText({
      
      if (is.null(input$plot1_brush$x)) 
        return("") else {
        vdata <- as.numaric(unlist(strsplit(pasted_data, ",", fixed = TRUE)))
        df <- data.frame(well = num_to_well(1:384, plate = 384), vals = vdata)
        lvls <- levels(df$well)
        name <- lvls[round(input$plot1_brush$x)]
        HTML("You've selected <code>", name, "</code>")
      }
    })
    
   observe({  
    if (input$loadsample > 0) {
      pasted_data <- "0.041, 0.341, 0.304, 0.442, 0.220, 0.325, 0.315, 0.297, 0.077, 0.104, 0.413, 0.239, 0.316, 0.289, 0.043, 0.295, 0.320, 0.283, 0.041, 0.334, 0.310, 0.040, 0.412, 0.346
0.058, 0.182, 0.041, 0.161, 0.324, 0.273, 0.040, 0.450, 0.299, 0.148, 0.040, 0.337, 0.041, 0.098, 0.390, 0.058, 0.319, 0.041, 0.050, 0.186, 0.158, 0.187, 0.044, 0.065
0.040, 0.072, 0.286, 0.040, 0.292, 0.113, 0.374, 0.040, 0.268, 0.271, 0.264, 0.334, 0.060, 0.311, 0.319, 0.134, 0.067, 0.058, 0.291, 0.348, 0.413, 0.301, 0.307, 0.352
0.373, 0.041, 0.193, 0.041, 0.275, 0.092, 0.268, 0.280, 0.144, 0.436, 0.303, 0.299, 0.239, 0.042, 0.043, 0.227, 0.198, 0.136, 0.400, 0.057, 0.040, 0.213, 0.419, 0.312
0.295, 0.467, 0.264, 0.142, 0.056, 0.397, 0.327, 0.181, 0.284, 0.407, 0.187, 0.396, 0.064, 0.310, 0.056, 0.046, 0.283, 0.264, 0.288, 0.197, 0.403, 0.282, 0.082, 0.467
0.062, 0.040, 0.222, 0.056, 0.393, 0.145, 0.405, 0.464, 0.042, 0.276, 0.422, 0.046, 0.185, 0.208, 0.326, 0.214, 0.352, 0.281, 0.323, 0.463, 0.159, 0.040, 0.039, 0.496
0.281, 0.262, 0.351, 0.267, 0.040, 0.277, 0.291, 0.408, 0.051, 0.040, 0.289, 0.328, 0.343, 0.168, 0.300, 0.415, 0.231, 0.050, 0.235, 0.275, 0.491, 0.238, 0.065, 0.409
0.353, 0.168, 0.263, 0.259, 0.335, 0.051, 0.041, 0.228, 0.304, 0.265, 0.262, 0.279, 0.388, 0.360, 0.319, 0.320, 0.061, 0.349, 0.421, 0.162, 0.106, 0.299, 0.382, 0.040
0.274, 0.449, 0.165, 0.287, 0.263, 0.283, 0.256, 0.364, 0.148, 0.355, 0.427, 0.170, 0.040, 0.396, 0.256, 0.279, 0.295, 0.367, 0.261, 0.040, 0.087, 0.316, 0.374, 0.168
0.358, 0.240, 0.258, 0.456, 0.111, 0.295, 0.465, 0.343, 0.257, 0.327, 0.287, 0.091, 0.098, 0.399, 0.263, 0.041, 0.288, 0.239, 0.041, 0.145, 0.314, 0.400, 0.059, 0.374
0.329, 0.291, 0.404, 0.277, 0.291, 0.269, 0.289, 0.266, 0.240, 0.096, 0.256, 0.219, 0.043, 0.184, 0.320, 0.040, 0.213, 0.391, 0.324, 0.041, 0.393, 0.040, 0.467, 0.309
0.289, 0.204, 0.270, 0.305, 0.271, 0.284, 0.334, 0.360, 0.151, 0.040, 0.097, 0.203, 0.371, 0.040, 0.466, 0.259, 0.268, 0.371, 0.276, 0.253, 0.271, 0.276, 0.371, 0.041
0.431, 0.040, 0.459, 0.305, 0.197, 0.375, 0.040, 0.454, 0.362, 0.262, 0.273, 0.374, 0.255, 0.251, 0.407, 0.182, 0.116, 0.259, 0.285, 0.401, 0.077, 0.045, 0.373, 0.174
0.410, 0.275, 0.281, 0.189, 0.306, 0.041, 0.276, 0.228, 0.040, 0.378, 0.276, 0.241, 0.298, 0.039, 0.283, 0.447, 0.397, 0.274, 0.305, 0.104, 0.362, 0.359, 0.040, 0.416
0.139, 0.046, 0.310, 0.428, 0.349, 0.359, 0.186, 0.040, 0.072, 0.040, 0.362, 0.259, 0.039, 0.239, 0.295, 0.265, 0.255, 0.043, 0.039, 0.090, 0.404, 0.273, 0.103, 0.040
0.045, 0.368, 0.265, 0.040, 0.040, 0.044, 0.305, 0.040, 0.142, 0.092, 0.267, 0.416, 0.328, 0.434, 0.327, 0.042, 0.042, 0.098, 0.370, 0.111, 0.041, 0.288, 0.295, 0.042"
      writeto.table.platedata(pasted_data)
    } else {
      NULL
    }
  })
  
  writeto.table.platedata <- function(data) {
    # output$platetable<-DT::renderDataTable(data, server = FALSE)
    vdata <- pasted_plate_data(data)
    plate_stats(vdata)
    createheat_map(vdata)
    
    
    # console.testdata(vdata)
  }
  
  # create_plate_list<-function(data){
  # output$choose_dataset <- renderUI({})
  # data <- multiplates

  get_plates<-function(plt){
    data1<-inData()
    data1<-gsub("\\n", ",", data1)
    console.other(data1)
    multip1 <- unlist(strsplit(data1, "p"))
    data <- multip1[lapply(multip1, nchar) > 1]
      if (length(data) > 1) {
          list_val <- NULL
          plateData<- NULL
          for (i in 1:length(data)) {
            list <- unlist(strsplit(data[i], ",", fixed = TRUE))
            idata <- as.list(strsplit(data[i], ","))
            plateData[paste0("Plate-", list[1])]<-idata

            # 
            list_val <- c(list_val, paste0("Plate-", list[1]))
            # console.raw(list[1])
            list <- NULL
          }
          console.raw(plateData)
          finalList <- as.list(as.character(list_val))
          return(plateData[[plt]])

          # console.testdata(finalList)
        } else {
          console.raw("Maybe you dont have Plate names (p1,p2 ...) or No buttons Clicked or Data Pasted")
        }
  }
  
  createPlate_List<-function(){
  output$choose_dataset <- renderUI({
    # colunts <- unlist(strsplit(pasted_data,',', fixed = TRUE))
    data1<-inData()
    multip1 <- unlist(strsplit(data1, "p"))
    data <- multip1[lapply(multip1, nchar) > 1]
    # console.testdata(data)
      if (length(data) > 1) {
        list_val <- NULL
        for (i in 1:length(data)) {
          list <- unlist(strsplit(data[i], ",", fixed = TRUE))
          

          # 
          list_val <- c(list_val, paste0("Plate-", list[1]))
          # console.raw(list[1])
          list <- NULL
        }
        finalList <- as.list(as.character(list_val))
        # return(plateData)
        selectInput("dataset", "Select Data Plate", finalList)

        # console.testdata(finalList)
      } else {
        console.raw("No buttons Clicked or Data Pasted")
        NULL
      }
    
  })
}
  # }
  
  # set.seed(122)
  # histdata <- rnorm(500)
  
  # mtcars2 = mtcars[, c('hp', 'mpg')]
  # output$platetable<-DT::renderDataTable(mtcars2, server = FALSE)
  
  console.raw <- function(data) {
    output$rawdata3 <- renderPrint({
      data
    })
  }
  
  console.data <- function(data) {
    output$rawdata2 <- renderPrint({
      data
    })
  }
  
  console.other <- function(data) {
    output$rawdata1 <- renderPrint({
      data
    })
  }
  
  console.testdata <- function(data) {
    output$rawdata4 <- renderPrint({
      data
    })
  }

  console.mainpage <- function(data) {
    output$rawdatamain <- renderPrint({
      data
    })
  }
  
  dfmidVals <- function(df) {
    m_nums <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
      20, 21, 22, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 
      42, 43, 44, 45, 46, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 
      64, 65, 66, 67, 68, 69, 70, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 
      86, 87, 88, 89, 90, 91, 92, 93, 94, 99, 100, 101, 102, 103, 104, 105, 
      106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 123, 
      124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 
      138, 139, 140, 141, 142, 147, 148, 149, 150, 151, 152, 153, 154, 155, 
      156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 171, 172, 173, 
      174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 
      188, 189, 190, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 
      206, 207, 208, 209, 210, 211, 212, 213, 214, 219, 220, 221, 222, 223, 
      224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 
      238, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 
      256, 257, 258, 259, 260, 261, 262, 267, 268, 269, 270, 271, 272, 273, 
      274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 291, 
      292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 
      306, 307, 308, 309, 310, 315, 316, 317, 318, 319, 320, 321, 322, 323, 
      324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 339, 340, 341, 
      342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 
      356, 357, 358, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 
      374, 375, 376, 377, 378, 379, 380, 381, 382)
    midDF <- df[m_nums, ]
    midDF$type <- "vals"
    return(midDF)
  }
  
  dfcontrolVals <- function(df) {
    m_nums <- c(1, 25, 97, 121, 193, 217, 289, 313, 23, 47, 119, 143, 215, 239, 
      311, 335, 2, 26, 98, 122, 194, 218, 290, 314, 24, 48, 120, 144, 216, 
      240, 312, 336, 49, 73, 145, 169, 241, 265, 337, 361, 71, 95, 167, 191, 
      263, 287, 359, 383, 50, 74, 146, 170, 242, 266, 338, 362, 72, 96, 168, 
      192, 264, 288, 360, 384)
    contDF <- df[m_nums, ]
    contDF$type <- "cont"
    return(contDF)
  }


  dfmidVals96 <- function(df) {
    m_nums <- c(2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,26,27,28,29,30,31,32,33,34,35,38,39,40,41,42,43,44,45,46,47,50,51,52,53,54,55,56,57,58,59,62,63,64,65,66,67,68,69,70,71,74,75,76,77,78,79,80,81,82,83,86,87,88,89,90,91,92,93,94,95)
    midDF <- df[m_nums, ]
    midDF$type <- "vals"
    return(midDF)
  }
  
  dfcontrolVals96 <- function(df) {
    m_nums <- c(1,25,49,73,24,48,72,96,13,25,49,73,12,36,60,8)
    contDF <- df[m_nums, ]
    contDF$type <- "cont"
    return(contDF)
  }
  
  # PASTED PLate data
  pasted_plate_data <- function(pdata) {
    f9<-as.vector(as.numeric(unlist(pdata)))
    p8<-gsub("\\n", ",", f9)
    p7<-gsub(",$", "", p8)
    # f8<-strsplit(as.character(ndata$well),'',fixed=TRUE)))
    # strsplit(as.character(ndata$well),'',fixed=TRUE)))
    f0 <-p7
    f2<-trimws(f0 ,which = c("both"))
    pasted_data <- f2
    console.testdata(pasted_data)
    return(pasted_data)
  }

  cleandata<-function(data){

  }
  
  plotRowcolerror <- function(df) {
    
    ndata <- rbind(dfcontrolVals(df), dfmidVals(df))
    foo <- data.frame(do.call("rbind", strsplit(as.character(ndata$well), "", 
      fixed = TRUE)))
    pcoli <- paste0(foo$X2, foo$X3)
    pvalue <- foo$vals
    prow <- foo$X1
    nprow <- as.integer(foo$X1)
    pcol <- as.integer(pcoli)
    tpcol <- paste("C", as.integer(pcoli))
    newframe <- cbind(ndata, prow, pcol, nprow, tpcol)
    console.other(newframe)
    
    output$confplot3 <- renderPlot({
      g <- ggplot(ndata, aes(y = vals, x = prow))
      g + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", 
        dotsize = 0.5, fill = "red") + theme(axis.text.x = element_text(angle = 65, 
        vjust = 0.6)) + labs(title = "Box plot + Dot plot", subtitle = "Each dot represents 1 row in source data", 
        caption = "Source: Single Plate", x = "Well Location", y = "Value")
      
    })
    
    output$confplot4 <- renderPlot({
      g <- ggplot(ndata, aes(y = vals, x = tpcol))
      g + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", 
        dotsize = 0.5, fill = "red") + theme(axis.text.x = element_text(angle = 65, 
        vjust = 0.6)) + labs(title = "Box plot + Dot plot", subtitle = "Each dot represents 1 row in source data", 
        caption = "Source: Single Plate", x = "Well Location", y = "Value")
      
    })
    
  }
  
  plotNormalize <- function(df) {
    
    ndata <- rbind(dfcontrolVals(df), dfmidVals(df))
    # foo <- data.frame(do.call('rbind',
    # strsplit(as.character(ndata$well),'',fixed=TRUE)))
    # pcoli<-paste0(foo$X2,foo$X3); pvalue<-foo$vals prow<-foo$X1
    # nprow<-as.integer(foo$X1) pcol<-as.integer(pcoli)
    # tpcol<-paste('C',as.integer(pcoli))
    # newframe<-cbind(ndata,prow,pcol,nprow,tpcol) console.other(newframe)
    
    
    # rowsum <- aggregate(newframe, by=list(newframe$nprow), FUN=mean)
    # console.other(rowsum)
    
    # rowsumdf <- rbind(head(rowsum), tail(rowsum)) console.other(rowsumdf)
    
    output$confplot1 <- renderPlot({
      ggplot(ndata, aes(y = vals, x = well), main = "Raw Data Plot", xlab = "Plate Well (Rows: left -> Right)", 
        ylab = "value") + geom_point(aes(color = type), size = 3.5) + theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
        theme(panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) + 
        theme(axis.line = element_line(colour = "gray80"))
      
    })
    
  }
  
  plate_stats <- function(data) {
    
    # vdata <- as.numeric(unlist(strsplit(data, ",", fixed = TRUE)))
    datai<-get_plates(input$dataset)
    # datai<-as.vector(datai)
    
    datai <- pasted_plate_data(datai)
    vdata <-datai[-1]
    # console.testdata(vdata)
    # vdata <- as.numeric(unlist(strsplit(vdata, ",", fixed = TRUE)))
    vdata<-as.vector(as.numeric(vdata))
    console.data(vdata)
    if(input$pty==96){
        df <- data.frame(well = num_to_well(1:96, plate = 96), vals = vdata)
      } else if(input$pty==384){
        df <- data.frame(well = num_to_well(1:384, plate = 384), vals = vdata)
      } else {
        df<-NULL
      }
    pmean <- mean(df$vals)
    psd <- sd(df$vals)
    pmin <- min(df$vals)
    pmax <- max(df$vals)
    # create dotplot console.other(dfmidVals(df))
    output$platestats <- renderUI({
      HTML(paste("<span class=\"tit-1\">Highest Value</span>: <span class=\"res-1\">", 
        pmax, "</span><br/>
<span class=\"tit-1\">Lowest Value</span>: <span class=\"res-1\">", 
        pmin, "</span><br/>
<span class=\"tit-1\">Plate SD</span>: <span class=\"res-1\">", 
        psd, "</span><br/>
<span class=\"tit-1\">Well Mean</span>: <span class=\"res-1\">", 
        pmean, "</span><br/>
<span class=\"tit-1\">Control Mean (col 1,2,23,24)</span>: <span class=\"res-1\">M</span><br/>"))
    })
  }
  
  createheat_map <- function(data) {
    
    datai<-get_plates(input$dataset)
    # datai<-as.vector(datai)
    
    datai <- pasted_plate_data(datai)
    vdata <-datai[-1]
    # console.testdata(vdata)
    # vdata <- as.numeric(unlist(strsplit(vdata, ",", fixed = TRUE)))
    vdata<-as.vector(as.numeric(vdata))
    console.data(vdata)
    
    if(input$pty==96){
        df <- data.frame(well = num_to_well(1:96, plate = 96), vals = vdata)
        siz<-16
      } else if(input$pty==384){
        df <- data.frame(well = num_to_well(1:384, plate = 384), vals = vdata)
        siz<-10
      } else {
        df<-NULL
        siz<-NULL
      }
    
    # create dotplot console.other(dfmidVals(df))
    createdatapoints_plot(df)
    createfreq_plot(df)
    createdatasorted_plot(df)
    plotPercentHits(df)
    
    output$platetable <- DT::renderDataTable(df, server = FALSE)
    console.raw(df)
    
    # selectedpoints<-brushedPoints(df, input$plot1_brush)
    # output$platetable<-DT::renderDataTable(selectedpoints, server = FALSE)
    # console.raw(input$plot1_brush)
    
    if (input$tab == "confidence") {
      plotRowcolerror(df)
    }
    
    if (input$tab == "normalize") {
      plotNormalize(df)
    }
    
    pt <- input$platetheme
    output$plotplate <- renderPlot({
      plttype<-input$pty
      if (pt == 1) {
        raw_map(data = df$vals, well = df$well, plate = plttype) + ggtitle("Raw Plate - Light") + 
          geom_point(aes_string(fill = "values"), colour = "gray40", shape = 21, 
          size = siz) + scale_fill_distiller("value", palette = input$palette)
        
      } else if (pt == 2) {
        raw_map(data = df$vals, well = df$well, plate = plttype) + theme_dark() + 
          ggtitle("Raw Plate - Dark") + geom_point(aes_string(fill = "values"), 
          colour = "gray40", shape = 21, size = siz) + scale_fill_distiller("value", 
          palette = input$palette)
        
      } else if (pt == 3) {
        z_map(data = df$vals, well = df$well, plate = plttype) + ggtitle("Edge effect plate") + 
          geom_point(aes_string(fill = "values"), colour = "gray40", shape = 21, 
          size = siz) + scale_fill_distiller("value", palette = input$palette)
        
      } else if (pt == 4) {
        b_map(data = df$vals, well = df$well, plate = plttype) + ggtitle("b-score : Median polish plate effects") + 
          geom_point(aes_string(fill = "values"), colour = "gray40", shape = 21, 
          size = siz) + scale_fill_distiller("value", palette = input$palette)
      } else if (pt == 5) {
        hit_map(data = df$vals, well = df$well, plate = plttype) + ggtitle(expression(atop("Hit Map", 
          atop(italic("The function defaults to categorising wells as 'hits' or 'negative hits' for those values abve or below 2 standard deviations of the plate average."), 
          "")))) + theme(plot.subtitle = element_text(size = 12)) + geom_point(aes_string(fill = "values"), 
          colour = "gray40", shape = 21, size = siz)
        
      } else if (pt == 6) {
        hit_map(data = df$vals, well = df$well, plate = plttype, threshold = input$threshold) + 
          ggtitle(paste("Hits - Selected SD ", input$threshold)) + geom_point(aes_string(fill = "values"), 
          colour = "gray40", shape = 21, size = siz)
        
      } else if (pt == 7) {
        hit_map(data = df$vals, well = df$well, plate = plttype, threshold = input$threshold) + 
          ggtitle(paste("Hits - B-Score Smooth & Selected SD ", input$threshold)) + 
          theme(plot.subtitle = element_text(size = 14)) + geom_point(aes_string(fill = "values"), 
          colour = "gray40", shape = 21, size = siz)
        
      } else {
        NULL
        
      }
    })
  }

  trim <- function (x) { gsub("^\\s+|\\s+$", "", x) }
  
  createfreq_plot <- function(data) {
    output$plotfeq <- renderPlot({
      hist(data$vals, main = "frequency Plot - data distribution.", xlab = "Data Values", 
        col = "#75AFA9", border = "#75AFA9", las = 1)
      
      
    })
  }
  
  createdatapoints_plot <- function(data) {
    # ndata <- data[order(data$vals),]
    console.raw(data)
    output$plotdatapoints <- renderPlot({
      xyplot(data$vals ~ data$well, data, grid = TRUE, main = "Raw Data Plot", 
        type = c("p", "smooth"), pch = 19, lwd = 1, col = "#75AFA9", xlab = "Plate Well (Rows: left -> Right)", 
        ylab = "value", scales = list(x = list(at = NULL)))
    })
  }
  
  createdatasorted_plot <- function(data) {
    # ndata <- data[order(data$vals),]
    
    output$sorted_plot <- renderPlot({
      data$well <- factor(data$well, levels = data$well[order(data$vals)])
      # //Plot USING GGPLOT ggplot(data, aes(x=well, y=vals)) + geom_point(shape=1)
      xyplot(data$vals ~ data$well, data, grid = TRUE, main = "Sorted Data Plot", 
        type = c("p", "smooth"), pch = 19, col = "#75AFA9", lwd = 1, xlab = "Plate Well (Rows: left -> Right)", 
        ylab = "value", scales = list(x = list(at = NULL)))
    })
  }
  
  
  
  # output$brush_info <- renderPrint({ selectedpoints<-brushedPoints(mtcars2,
  # input$plot1_brush) output$platetable<-DT::renderDataTable(df, server = FALSE)
  # })

  plotPercentHits <- function(df) {
    #http://www.r-graph-gallery.com/226-plot-types-for-circular-plot/
    if(input$pty==96){
      ndata <- rbind(dfcontrolVals96(df), dfmidVals96(df))
    } else if(input$pty==384){
      ndata <- rbind(dfcontrolVals(df), dfmidVals(df))
    } else {
      ndata<-NULL
    }
    foo <- data.frame(do.call("rbind", strsplit(as.character(ndata$well), "", 
      fixed = TRUE)))
    pcoli <- paste0(foo$X2, foo$X3)
    pvalue <- foo$vals
    prow <- foo$X1
    nprow <- as.integer(foo$X1)
    pcol <- as.integer(pcoli)
    tpcol <- paste("C", as.integer(pcoli))
    # percent <-(ndata$vals/sum(ndata$vals))*100
    pmin <- min(df$vals)
    pmax <- max(df$vals)
    pval<-((ndata$vals-pmin)/(pmax-pmin))*100
    pgroup<- cut(pval,breaks=c(0.0,10,20,30,40,50,60,70,80,90,100.1), labels=c("<10%","11-20%","21-30%","31-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-100%"), right=FALSE)
    y<-c(1:nrow(ndata))
    newframe <- cbind(ndata, y, prow, pcol, nprow, tpcol, pval, pgroup)
    
    console.mainpage(newframe)
    # data<-newframe

    output$percentplot <- renderPlot({
      data<-newframe
      
      if(input$pty==96){
        circos.par("track.height" = 0.6)
          # Initialize chart
        circos.initialize(factors = data$pgroup, x = data$pval )
        circos.trackPlotRegion(factors = data$pgroup, y=data$y, panel.fun = function(pval, y) {
            circos.axis(labels.cex=1.1, labels.font=1, lwd=0.8, h="top", direction="outside")
            })
        circos.trackPoints(data$pgroup, data$pval, data$y, col = rgb(0.1,0.5,0.8,0.5), pch=20)
      } else if(input$pty==384){
        circos.par("track.height" = 0.6)
          # Initialize chart
        circos.initialize(factors = data$pgroup, x = data$pval )
        circos.trackPlotRegion(factors = data$pgroup, y=data$y, panel.fun = function(pval, y) {
            circos.axis(labels.cex=1.1, labels.font=1, lwd=0.8, h="top", direction="outside")
            })
        circos.trackPoints(data$pgroup, data$pval, data$y, col = rgb(0.1,0.5,0.8,0.5), pch=20)
      } else {
        
      }
    })
    
    # output$percentplot <- renderPlot({

    #   # custom general parameters:
    #   par(mar = c(1, 1, 1, 1), bg=rgb(0.4,0.1,0.7,0.05) ) 
    #   circos.par("track.height" = 0.6)

    #   # Step1: Initialize
    #   circos.initialize(factors = newframe$pgroup, x = newframe$vals)
    #   # Step2: Build regions. 
    #   circos.trackPlotRegion(factors = newframe$pgroup, y = newframe$y, panel.fun = function(x, y) {
    #       circos.axis(
    #           h="top",                   # x axis on the inner or outer part of the track?
    #           labels=TRUE,               # show the labels of the axis?
    #           major.tick=TRUE,           # show ticks?
    #           labels.cex=0.5,            # labels size (higher=bigger)
    #           labels.font=1,             # labels font (1, 2, 3 , 4)
    #           direction="outside",       # ticks point to the outside or inside of the circle ?
    #           minor.ticks=4,             # Number of minor (=small) ticks
    #           major.tick.percentage=0.1, # The size of the ticks in percentage of the track height
    #           lwd=2                      # thickness of ticks and x axis.
    #           )
    #       })
    #   # Step 3: Add points
    #   circos.trackPoints(newframe$pgroup, newframe$vals, newframe$y, col = rgb(0.1,0.5,0.8,0.3), pch = 20, cex = 2)


      # g <- ggplot(ndata, aes(y = vals, x = prow))
      # g + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", 
      #   dotsize = 0.5, fill = "red") + theme(axis.text.x = element_text(angle = 65, 
      #   vjust = 0.6)) + labs(title = "Box plot + Dot plot", subtitle = "Each dot represents 1 row in source data", 
      #   caption = "Source: Single Plate", x = "Well Location", y = "Value")
      
    #})
    
  }
  
}


