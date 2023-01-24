library(shiny)
library(magick)

setwd("C:/Users/pcmckann/desktop/myapp"->wd)
#setwd("C:\Users\pcmckann\desktop\myapp"->wd)
www=paste0(wd,'/www')

lf=list.files()

weblonpics=list.files(path=paste0(www,'/weblon'),pattern='Weblon')
weblonnames=gsub('.png','',weblonpics)
weblonnamesshort=gsub('Weblon Coast Line Plus ','',weblonnames)
sunbrellapics=list.files(path=paste0(www,'/sunbrella'),pattern='Sunbrella')
sunbrellanames=gsub('.png','',sunbrellapics)
sunbrellanamesshort=gsub('Sunbrella Seamark ','',weblonnames)
swatchfiles=list.files('./www/swatches')
rooffiles=list.files('./www/roof')
wallfiles=list.files('./www/walls')
skirtfiles=list.files('./www/skirt')


#addResourcePath(prefix = "imgResources", directoryPath = "www")

# Define UI ----
ui <- fluidPage(
 
   # titlePanel(
   #   div(" Place your Yurtzzle on the perfect spot"
   #            ,style="height:200px;background-color: grey;font:"),
   # ),
  titlePanel(title=span('    Place your Yurtzzle on the perfect spot'
                ,style='color:grey; font-size:28px;font-family:Arial;margin-left:80px')
            
            ),

  fluidRow(
    column(width=3, 
           fluidRow(
             selectInput("roof", label = h4("  Select Roof Color"), 
                         choices = weblonnamesshort,
                         selected = 1,
                         width="80%")
             ,style= "background-color: grey;margin-left:6px;font-family: Arial;font-size: 14px"
           )
    )
    ,column(width=1,
           fluidRow(
           imageOutput("roof")
           ,style="height:80px;background-color:grey"
           )
    )
    ,column(width=3,
           fluidRow(
             selectInput("walls", label = h4("  Select Wall Color"), 
                                choices = c(weblonnamesshort,sunbrellanamesshort),
                                selected = 2,
                                width="80%")
             ,style= "background-color: LightGrey"
             )
    )
    ,column(width=1,
            fluidRow(
             imageOutput("walls")
             ,style="height:80px;background-color: LightGrey"
             )
    )
    ,column(width=3,
           fluidRow(
            selectInput("skirt", label = h4("  Select Skirt Color"), 
                                choices = c(weblonnamesshort,sunbrellanamesshort),
                                selected = 1,
                                width="80%")
            ,style= "background-color: grey"
            )
    )
    ,column(width=1,
            fluidRow(
                     imageOutput("skirt")
               ,style= "height:80px;background-color: grey"
            )
    )
          # fluidRow(
          #   div("")
          #),
  ),
  
  fluidRow(
    column(width=3,
          fluidRow(
            fileInput("upload", h4("Upload Background Image"), accept = c('image/png', 'image/jpeg'))
            ,style= "background-color: LightGrey;margin-left:6px"
          ),
          fluidRow(
            sliderInput("yurtsize",h4("Adjust Yurtzzle Size"),20,1200,500
            ,ticks=F)
            ,style= "background-color: grey;margin-left:6px"
          ),
          fluidRow(
            sliderInput("yurtright",h4("Move Yurtzzle Right and Left"),0,800,0
            ,ticks=F)
            ,style= "background-color: LightGrey;margin-left:6px"
          ),
          fluidRow(
            sliderInput("yurtdown",h4("Move Yurtzzle Down and Up"),0,800,0
            ,ticks=F)
            ,style= "background-color: grey;margin-left:6px"
          )
    	    

      )
    
    # column(width=1, 
    #        
    #        fluidRow(
    #          imageOutput("roof")
    #          ,style="height:80px;width:80px"
    #        ),
    #       
    #        fluidRow(
    #          imageOutput("walls")
    #          ,style="height:80px;width:80px"
    #        ),
    #       
    #        fluidRow(
    #          imageOutput("skirt")
    #          ,style= "height:80px;width:80px"
    #        )
    #        
    # ),
    
    ,column(
      width=8,imageOutput("dressedYurt")
      ,style=("margin-left:10px;margin-top:10px")
    )
  )
)


    

    # mainPanel(
    #   # imageOutput("roof") ,
    #   # imageOutput("walls"),
    #   # imageOutput("skirt"),
    #   # 
      #imageOutput("dressedYurt")




#the problem is converting the character variable into the filename of the png file--into the datapath


# Define server logic ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2)#allows large files to be uploaded (30MB)
  # 
   roofswatchfile=reactive({
          normalizePath(file.path('./www/Swatches',swatchfiles[grepl(input$roof,swatchfiles)]))
   })
  # roofswatchfile=reactive({
  #     image_border(
  #       image_read(normalizePath(file.path('./www/Swatches',swatchfiles[grepl(input$roof,swatchfiles)]))
  #       )
  #       "black","4x4")
                   
  wallswatchfile=reactive({
     normalizePath(file.path('./www/Swatches',swatchfiles[grepl(input$walls,swatchfiles)]))
   })
   skirtswatchfile=reactive({
     normalizePath(file.path('./www/Swatches',swatchfiles[grepl(input$skirt,swatchfiles)]))
   })

	output$roof=renderImage({
	  list(src=roofswatchfile(),height=80,width=80)
	  #filename= normalizePath(file.path('./www/Swatches',swatchfiles[grepl(input$roof,swatchfiles)]))
	  #list(src=filename,height=100,width=100)
	},deleteFile=FALSE)
	output$walls=renderImage({
    list(src=wallswatchfile(),height=80,width=80)
	},deleteFile=FALSE)
	output$skirt=renderImage({
    list(src=skirtswatchfile(),height=80,width=80)
	},deleteFile=FALSE)
	
	
	# Background Image
	bgLoc <- reactiveVal("https://raw.githubusercontent.com/ThinkR-open/collage/master/inst/tigrou/tigrou.jpg")
	## convert the img location to an img value
	bgVal <- reactive({
	  image_convert(image_read(bgLoc())|>image_scale("1000"), "jpeg")
	})
	
	# When uploading new image
	observeEvent(input$upload, {
	  if (length(input$upload$datapath)) {
	    ## set the image location
	    bgLoc(input$upload$datapath)
	  }
	})
	

	updatedbgLoc <- reactive({
	  ## retrieve the imageVal
	  #image <- imageVal()
	  
	  
	  # Numeric operators
	  bgfileloc <- image_write(bgVal(), format = 'jpg')
	  
	  ## return only the tmp file location
	  bgfileloc
	})
	
	# # A plot of fixed size
	# output$img <- renderImage(
	#   {
	#     # Return a list
	#     list(src = updatedImageLoc(), contentType = "image/jpeg")
	#   }, 
	#   ## DO NOT DELETE THE FILE!
	#   deleteFile = FALSE
	# )
	  
	
  tempfilelocation=reactive({
    
	  bgfn=updatedbgLoc()#normalizePath(file.path("./www/background/Republic.jpg"))
	  rfn=normalizePath(file.path('./www/roof',rooffiles[grepl(input$roof,rooffiles)]))
	  wfn=normalizePath(file.path('./www/walls',wallfiles[grepl(input$walls,wallfiles)]))
	  sfn=normalizePath(file.path('./www/skirt',skirtfiles[grepl(input$skirt,skirtfiles)]))
	  bg=image_border(image_read(path=bgfn),"black","10x10")|>image_scale("800") #|> image_resize('1000%')
	
	  #now to superimpose different roof, walls, and skirt
	  r=image_read(path=rfn)|>image_scale("800") #|>image_transparent('grey', fuzz = 25)
	  w=image_read(path=wfn)|>image_scale("800") #|>image_transparent('grey', fuzz = 25)
	  #doorcolor='orange';doorx="+555+400";w2=image_fill(w,doorcolor,point=doorx,fuzz=20)
	  s=image_read(path=sfn)|>image_scale("800") #|>image_transparent('grey', fuzz = 25)
	  #y=image_read(path=file.path("./www/door/door Yurtzzle Logo Window.png"))|>image_scale("500")
	  # 
	# 
	   dy=image_trim(image_flatten(c(image_background(s,"none"),w,r)))|>image_scale(input$yurtsize)
	   
	   dy2=image_composite(bg,dy
	                       ,gravity='northwest'
	                       ,offset=paste0(
	                                #ifelse(input$yurtright>=0,'+',''),
	                         '+',input$yurtright,'+',input$yurtdown) )
	   
	   tmpfile=image_write(dy2,"dressedYurt tmp.jpg",format='jpg')
	   
	  tmpfile
  })
	  
	 output$dressedYurt=renderImage({
	   list(src=tempfilelocation())
	    },deleteFile=F)

}



# Run the app ----
shinyApp(ui = ui, server = server)













