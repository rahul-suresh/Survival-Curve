library(shiny)
library(colourpicker)
library(Hmisc)
library(markdown)
library(rhandsontable)
library(dplyr)
library(magrittr)

ggplotColours <- function(n=6, h=c(0, 360)+15){
	if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
	hcl(h = (seq(h[1], h[2], length = n)), c = 150, l = 60)
}

ui <- function(request){
	navbarPage(
		"Survival Curve", theme = "bootstrap.css",
		tabPanel(
			"Data",
			sidebarLayout(
				sidebarPanel(
					p("This app will generate a Suvival curve based on the data entered in the spreadsheet provided. The sample data present at the start is in the format compatible with the app.
						for now try to limit the number of groups to 2. Once you have entered the data and made sure to review any issues in the data Review tab.")
					),
				mainPanel(
					tabsetPanel(type="tabs",
											tabPanel ("Data",
																rHandsontableOutput("mytable")
																
											),
											tabPanel("Review Data",
															 dataTableOutput('table')
											),
											tabPanel("Data Summary",
															 htmlOutput("summary")
											)
					)
				)
			)
			),
		tabPanel(
			"Plot",
			sidebarLayout(
				sidebarPanel(
					tabsetPanel(
						tabPanel(
							"Main",
							sliderInput("graphwidth", "Graph width :", min= 0, max= 1000, value= 600, step= 25),
							textAreaInput ("Title", "Title Text:", value = "Survival curve"),
							sliderInput("linewidth", "Line width:", 1, 10, 3),
							uiOutput("colourinput"),
							actionButton("reset", "Reset default colours", icon = icon("undo")),
							uiOutput("Graphlabels"),
							sliderInput("Titletextsize","Title Text Size:", min = 0, max=5, value=1.5, step = 0.01)
						),
						tabPanel(
							"Axis",
							sliderInput("Xaxislimits", "X axis limits:",min = 0, max = 200, value = c(0,140), step= 10),
							sliderInput("majorticks", "Major ticks:",min = 1, max = 50, value = 20, step= 1),
							sliderInput("axiswidth", "Axis width:",min = 0, max = 10, value = 1, step= 0.1),
							sliderInput("Xaxisoffset", "X Axis Offset %:",min = 0, max = 9.9, value = 3, step= 0.1),
							sliderInput("Yaxisoffset", "Y Axis Offset %:",min = 0, max = 9.9, value = 0, step= 0.1),
							textInput (inputId = "Xtitle", "X Axis Title",value = "Days"),
							textInput(inputId = "Ytitle", "Y Axis Title", value = "Survival Probability"),
							sliderInput("Axistitletextsize","Axis Title Text Size:", min = 0, max=2, value=1.4, step = 0.01),
							sliderInput("Axislabeltextsize","Axis Label Text Size:", min = 0, max=2, value=1, step = 0.01),
							selectInput("fontlab", label = h3("Axis Title Text Style"),choices = list("Regular" = 1, "Bold" = 2, "Italics" = 3, "Bold Italic"=4), selected = 2),
							selectInput("fontaxis", label = h3("Axis Labels Text Style"),choices = list("Regular" = 1, "Bold" = 2, "Italics" = 3, "Bold Italic"=4), selected = 2),
							sliderInput("Xaxisdistance","X Axis Title Position:", min = 0, max=5, value=3, step = 0.1),
							sliderInput("Yaxisdistance","Y Axis Title Position:", min = 0, max=5, value=3, step = 0.1)
						),
						tabPanel(
							"Legend",
							sliderInput("legendxposition", "Legend X position", min=0, max=100, value=75, step = 1),
							sliderInput("legendyposition", "Legend Y Position", min=0, max=100, value=100, step = 1),
							sliderInput("spacinglegend", "Spacing:", min=1, max=5, value=1.5, step = 0.1),
							sliderInput("Legendtextsize","Legend Text Size:", min = 0, max=2, value=1.1, step = 0.01)
						)
					)
				),
				tags$div(title="Right click and Save image as.. to download this image as png.",
				absolutePanel(
					top=50, right=50,
					uiOutput("ui_plot"),
					fixed = TRUE ,
					draggable=TRUE
					)
				)
			)
		),
		tabPanel(
			"Download",
			sidebarLayout(
				sidebarPanel(
					#left=20,width=400,draggable = TRUE,
					tabsetPanel(
						tabPanel("Download High-Res",
										 selectInput("fformat", "File type", choices=c("PNG","TIFF","PDF"), selected = NULL, multiple = FALSE, selectize = TRUE),
										 downloadButton('highres_download', 'Download Plot')
						)
					)
				),
				absolutePanel(
					right=20, width = 400, draggable = TRUE,
					wellPanel(
						HTML(
							markdownToHTML(
								fragment.only=TRUE,
								text=c("You can bookmark the current graphing parameters by bookmarking the state of the app. I have not figured out how to save the Graph Title yet in the book mark.
											 So for now just remember it.............  OK fine, Ill look into it if you can bribe me with a cup of coffee.")))),
					bookmarkButton()
				)
		)
		)
	)
}

server <- function(input, output, session) {
	
	library(rms)
	library(survival)
	
	#initial values
	
		ID<-c(rep("",200))
		Group<-c("Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group1","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2","Group2",rep("",168))
		Time<-as.numeric(c(61,87,87,87,87,87,73,87,63,73,67,106,52,139,59,55,61,87,87,75,57,87,87,75,68,55,14,67,24,49,108,19,rep("",168)))
		Status<-c(1,0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,rep("",168))
		dat1<-data.frame(ID,Group,Time,Status, stringsAsFactors = FALSE)
		
	#make data reactive again
		
		values <- reactiveValues(data=dat1)
		
		observe({
			if(!is.null(input$mytable))
				values$data <- hot_to_r(input$mytable)
		})
		
	#Spreadsheet for User-interface/ data upload
		
		output$mytable <- renderRHandsontable({
			rhandsontable(values$data, width = 450, height="600px")%>%
				hot_cols(colWidths = 100) %>%
				hot_rows(rowHeights = 25)%>%
				hot_cell(1, 1, "Column ID is for your preferance if you want an identifier for each of your observation. This data will not be used for making the survival curve")
		})
		
	# Review the data

		output$table <- renderDataTable(na.omit(values$data))
		
	#Data Summary
		
		output$summary <- renderUI({ 
			hmm<-data.frame(values$data, stringsAsFactors = FALSE)
			hmm<-na.omit(hmm)
			groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
			numberofgroups<- paste("Number of Groups:", length(unique(factor(hmm$Group))))
			totalnumber<- paste("Total Number of Observations:", length(hmm$Time))
			numberingroup<-NULL
			for(i in 1:length(unique(factor(hmm$Group)))){
				numberingroup<-paste(numberingroup,'<br/>',"Total number of observations in ",groupnumber$Group[i]," :", groupnumber$n[i])
			}
			HTML(paste(numberofgroups, totalnumber, numberingroup, sep = '<br/>'))
		})
		
		# make Colour values depend on the number of groups in the data
		
		colourdata <- reactive({
			hmm<-data.frame(values$data, stringsAsFactors = FALSE)
			hmm<-na.omit(hmm)
			groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
			numberofgroups<- paste("Number of Groups:", length(unique(factor(hmm$Group))))
			totalnumber<- paste("Total Number of Observations:", length(hmm$Time))
			listofcolors<-ggplotColours(length(unique(factor(hmm$Group))))
			lev<-sort(unique(groupnumber$Group))
			lapply(seq_along(lev), function(i) {
				colourInput(inputId =  paste0("col", i), label =  paste0("Choose colour for ", lev[i]," :"), value = listofcolors[i])
				})
		})
		
		output$colourinput <- renderUI({colourdata()})
		
		# Reset default colours
		
		observeEvent(input$reset, {
			# Problem: dynamic number of widgets
			# - lapply, do.call
			hmm<-data.frame(values$data, stringsAsFactors = FALSE)
			hmm<-na.omit(hmm)
			groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
			
			lev<-sort(unique(groupnumber$Group))
			listofcolors <- ggplotColours(length(unique(factor(hmm$Group))))
			
			lapply(seq_along(lev), function(i) {
				do.call(what = "updateColourInput",
								args = list(
									session = session,
									inputId = paste0("col", i),
									value = listofcolors[i]
								)
				)
			})
		})
		
		# input the key to the legend text depending on the number of groups in data.
		
		Graphdata <- reactive({
			hmm<-data.frame(values$data, stringsAsFactors = FALSE)
			hmm<-na.omit(hmm)
			groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
			numberofgroups<- paste("Number of Groups:", length(unique(factor(hmm$Group))))
			totalnumber<- paste("Total Number of Observations:", length(hmm$Time))
			listofcolors<-ggplotColours(length(unique(factor(hmm$Group))))
			lev<-sort(unique(groupnumber$Group))
			lapply(seq_along(lev), function(i) {
				textInput(inputId = paste0("graphlabel", i), label =  paste0(lev[i]," description :"), value = lev[i])
			})
		})
		
		output$Graphlabels <- renderUI({Graphdata()})
		
		#Render Display Plot

		output$plot1 <- renderPlot({
			
		hmm<-data.frame(values$data, stringsAsFactors = FALSE)
		hmm<-na.omit(hmm)
		groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
		
		cols<-paste0("c(", paste0("input$col", 1:length(unique(groupnumber$Group)), collapse = ", "), ")")
		cols <- eval(parse(text = cols))
		
		legendtext<-paste0("c(", paste0("input$graphlabel", 1:length(unique(groupnumber$Group)), collapse = ", "), ")")
		legendtext <- eval(parse(text = legendtext))
		
		hmm$SurvObj <- with(hmm, Surv(Time, Status == 1)) #define a survval object
		km.by.group <- survfit(SurvObj ~ Group, data = hmm, conf.type = "log-log") # load the data onto a survfit object/ survival curve. Format is survfot object= survfit (x axis~grouping info, data source, confidance intervals)
	
		log.rank<-survdiff(Surv(Time, Status == 1)~Group, data = hmm, rho = 0) # log rank test
		Wilcox<-survdiff(Surv(Time, Status == 1)~Group, data = hmm, rho = 1) # Peto & Peto modification of the Gehan-Wilcoxon test
		
		pvaluelogrank<- 1 - pchisq(log.rank$chisq, 1)
		pvaluewilcox<- 1 - pchisq(Wilcox$chisq, 1)
		
		par(mar=c(5.1,7.1,4.1,2.1), xpd=FALSE)
		plot(km.by.group, mark.time = F,
				cex.main=input$Titletextsize,
			 ylim=c(0,1.03), 
			 xlim=c(input$Xaxislimits[1],input$Xaxislimits[2]),
			 axes=FALSE,       # remove axis and borders
			 yaxs="i", 
			 col= cols,
			 lwd = input$linewidth, 
			 main=input$Title
			 )
		title(
			ylab = input$Ytitle,
			mgp = c(input$Yaxisdistance, 1, 0),
			cex.lab=input$Axistitletextsize,
			font.lab=input$fontlab)
		title(
			xlab = input$Xtitle,
			mgp = c(input$Xaxisdistance, 1, 0),
			cex.lab=input$Axistitletextsize,
			font.lab=input$fontlab
			)
		axis(1,
			 at=seq(input$Xaxislimits[1],input$Xaxislimits[2], by=input$majorticks),
			 mgp=c(3,1,input$Xaxisoffset/10),
			 lty= 1, 
			 lwd = input$axiswidth,
			 cex.axis= input$Axislabeltextsize,
			 font.axis=input$fontaxis
			 ) # can add (lwd=0, lwd.ticks =1) if you only want to display tick marks. might be a nice style.
		axis(2,
			 at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1),
			 mgp=c(3,1,input$Yaxisoffset/10),
			 lty= 1,
			 lwd = input$axiswidth,
			 cex.axis= input$Axislabeltextsize,
			 font.axis=input$fontaxis
			 )
		par(xpd=T)
		legend(
			x=((input$legendxposition/100)* (input$Xaxislimits[2]-input$Xaxislimits[1]))+input$Xaxislimits[1],
			y=input$legendyposition/100,
			#inset=-.3,
			legend=legendtext,
			col= cols,
			y.intersp = input$spacinglegend,
			lty = 1,
			bg="transparent",
			lwd = input$linewidth,
			box.lty = 0,
			text.font=2,
			cex=input$Legendtextsize
			)
	
	#minor ticks at y axis

			axis(2,
			 at=seq(0,1,0.05),
			 labels=F,
			 mgp=c(3,1,input$Yaxisoffset/10),
			 lwd=0,
			 lwd.ticks =input$axiswidth,
			 lty= 1,
			 las=1,
			 tcl=-.25
		)
	
	 #Add grid lines
	#grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"))
		Corner_text <- function(text, location="topright"){
		legend(location,legend=text, bty ="n", pch=NA) 
		}
	
		Corner_text(paste("Logrank pvalue=", round(pvaluelogrank, digits=3) ,"\n Wilcox pvalue=",round(pvaluewilcox, digits=3)), location = "bottomleft")
		})

	#Calculates the graph width as a numeric else it is in the worng format from input.
	plot_width <- function() {
		# calculate values$facetCount
		values$facetCount <- as.numeric(input$graphwidth)
		return(values$facetCount)
	}
	
	# The plot made is finally packages to be shown here
	
	output$ui_plot<- renderUI({
		plotOutput("plot1",  width=plot_width(),height= 450)
	})
	
	#Prepare for High Res Download.
	
	store <- reactiveValues(dname="Survival Curves")
	
	fn_downloadname <- reactive({
		
		if(input$fformat=="PNG") filename <- paste0(store$dname,".png",sep="")
		if(input$fformat=="TIFF") filename <- paste0(store$dname,".tif",sep="")
		if(input$fformat=="PDF") filename <- paste0(store$dname,".pdf",sep="")
		return(filename)
	})
	
	# This option only makes/renders plot when the the user hits the download button. so can download the same graph multiple times.
	
	fn_download <- function()
	{
		# Make the damn plot
		
		hmm<-data.frame(values$data, stringsAsFactors = FALSE)
		hmm<-na.omit(hmm)
		groupnumber<-data.frame(hmm%>%group_by(Group)%>%tally(), stringsAsFactors = FALSE)
		
		cols<-paste0("c(", paste0("input$col", 1:length(unique(groupnumber$Group)), collapse = ", "), ")")
		cols <- eval(parse(text = cols))
		
		legendtext<-paste0("c(", paste0("input$graphlabel", 1:length(unique(groupnumber$Group)), collapse = ", "), ")")
		legendtext <- eval(parse(text = legendtext))
		
		
		hmm$SurvObj <- with(hmm, Surv(Time, Status == 1)) #define a survval object
		km.by.group <- survfit(SurvObj ~ Group, data = hmm, conf.type = "log-log") # load the data onto a survfit object/ survival curve. Format is survfot object= survfit (x axis~grouping info, data source, confidance intervals)
		
		log.rank<-survdiff(Surv(Time, Status == 1)~Group, data = hmm, rho = 0) # log rank test
		Wilcox<-survdiff(Surv(Time, Status == 1)~Group, data = hmm, rho = 1) # Peto & Peto modification of the Gehan-Wilcoxon test
		
		pvaluelogrank<- 1 - pchisq(log.rank$chisq, 1)
		pvaluewilcox<- 1 - pchisq(Wilcox$chisq, 1)
		
		#Specify the file type to render
		
		if(input$fformat=="PNG") png(fn_downloadname(), width=(input$graphwidth/450)*6, height=6, units = 'in', res=300)
		if(input$fformat=="TIFF") tiff(fn_downloadname(), width=(input$graphwidth/450)*6, height=6, units = 'in', res = 300,compression="lzw")
		if(input$fformat=="PDF") pdf(fn_downloadname(), width=(input$graphwidth/450)*6, height=6)
		
		par(mar=c(5.1,7.1,4.1,2.1), xpd=FALSE)
		plot(km.by.group, mark.time = F,
				 cex.main=input$Titletextsize,
				 ylim=c(0,1.03), 
				 xlim=c(input$Xaxislimits[1],input$Xaxislimits[2]),
				 axes=FALSE,       # remove axis and borders
				 yaxs="i", 
				 col= cols,
				 lwd = input$linewidth, 
				 main=input$Title
		)
		title(
			ylab = input$Ytitle,
			mgp = c(input$Yaxisdistance, 1, 0),
			cex.lab=input$Axistitletextsize,
			font.lab=input$fontlab)
		title(
			xlab = input$Xtitle,
			mgp = c(input$Xaxisdistance, 1, 0),
			cex.lab=input$Axistitletextsize,
			font.lab=input$fontlab
		)
		axis(1,
				 at=seq(input$Xaxislimits[1],input$Xaxislimits[2], by=input$majorticks),
				 mgp=c(3,1,input$Xaxisoffset/10),
				 lty= 1, 
				 lwd = input$axiswidth,
				 cex.axis= input$Axislabeltextsize,
				 font.axis=input$fontaxis
		) # can add (lwd=0, lwd.ticks =1) if you only want to display tick marks. might be a nice style.
		axis(2,
				 at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1),
				 mgp=c(3,1,input$Yaxisoffset/10),
				 lty= 1,
				 lwd = input$axiswidth,
				 cex.axis= input$Axislabeltextsize,
				 font.axis=input$fontaxis
		)
		par(xpd=T)
		legend(
			x=((input$legendxposition/100)* (input$Xaxislimits[2]-input$Xaxislimits[1]))+input$Xaxislimits[1],
			y=input$legendyposition/100,
			#inset=-.3,
			legend=legendtext,
			col= cols,
			y.intersp = input$spacinglegend,
			lty = 1,
			bg="transparent",
			lwd = input$linewidth,
			box.lty = 0,
			text.font=2,
			cex=input$Legendtextsize
		)
		
		#minor ticks at y axis
		
		axis(2,
				 at=seq(0,1,0.05),
				 labels=F,
				 mgp=c(3,1,input$Yaxisoffset/10),
				 lwd=0,
				 lwd.ticks =input$axiswidth,
				 lty= 1,
				 las=1,
				 tcl=-.25
		)
		
		#Add grid lines
		#grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"))
		Corner_text <- function(text, location="topright"){
			legend(location,legend=text, bty ="n", pch=NA) 
		}
		
		Corner_text(paste("Logrank pvalue=", round(pvaluelogrank, digits=3) ,"\n Wilcox pvalue=",round(pvaluewilcox, digits=3)), location = "bottomleft")

		dev.off()
	}
	
	# download handler
	output$highres_download <- downloadHandler(
		filename = fn_downloadname,
		content = function(file) {
			fn_download()
			file.copy(fn_downloadname(), file, overwrite=T)
		}
	)
	
	
#	session$onSessionEnded(function() { #end R as soon as you close the browser
#		stopApp()													# problem is if you run it on R studio, the entire R program and R studio shuts down.
#		q("no") 													# it aslo shuts down when you hit refresh in the browser
#	})																	# best used if you are running vbs file directly or in a portable operation.
}

#enableBookmarking(store = "server")
shinyApp(ui, server, enableBookmarking="url")
