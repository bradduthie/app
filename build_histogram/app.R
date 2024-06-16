library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Building a histogram"),
    
    hr(),
    h4("Use the arrows at the bottom to increase the sample size of sparrows. As you increaes the sample size, you will see a new sparrow added in the right panel with its measured length at the top right. Notice how this adds a measurement to the histogram on the left, and think about the distribution of measured lengths that is being built."),
    hr(),
    
    plotOutput("distPlot"),
    
    fluidRow(
        column(4, offset = 1,
               numericInput("N1", label = "Sample size of birds", width="100%",
                            min = 0, max = 500, value = 0, step = 1), 
        ),
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # set.seed(100);
    
    mbox <- function(x0, x1, y0, y1){
        xx <- seq(from=x0, to=x1, length.out = 100);
        yy <- seq(from=y0, to=y1, length.out = 100);
        xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
        yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
        return(list(x=xd, y=yd));
    }
    
    datall <- read.csv("Bumpus_data.csv", header = TRUE);
    dat    <- datall;
    totlen <- as.numeric(dat[,3]);
    totwei <- as.numeric(dat[,5]);
    totbir <- length(totlen);
    
    xloc  <- runif(totbir);
    yloc  <- runif(totbir);
    
    output$distPlot <- renderPlot({
        N1     <- input$N1;
        minlen <- min(totlen);
        maxlen <- max(totlen);
        par(mar = c(5, 5, 1, 1), lwd = 3, mfrow = c(1, 2));
        hist(x = totlen, main = "", ylab = "Number of sparrows",
             xlab = "Sparrow total length (mm)", cex.lab = 1.5, lwd = 4,
             xaxt = "n", col = "white", xlim = c(minlen, maxlen), 
             ylim = c(0, 19), breaks = minlen:maxlen,
             border = "white", add = FALSE, cex.axis = 1.5);
        if(N1 > 0){
            hist(x = totlen[1:N1], main = "", ylab = "Number of sparrows",
                 xlab = "Sparrow total length (mm)", cex.lab = 1.5, lwd = 4,
                 xaxt = "n", col = "blue", xlim = c(minlen, maxlen), 
                 cex.axis = 1.5, ylim = c(0, 20), breaks = minlen:maxlen, 
                 add = TRUE);
            axis(side = 1, at = minlen:maxlen, cex.axis = 1.5, lwd = 3, 
                 line = -0.75);
            size  <- 0.02 * (totlen + 10*(totlen - mean(totlen)));
            par(mar = c(0, 0, 0, 0), lwd = 1, xaxt = "n", yaxt = "n");
            plot(x = xloc[1:N1], yloc[1:N1], pch = "-", xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", cex = 4*size[1:N1], bg = "grey", col = "black");
            points(x = xloc[1:N1], yloc[1:N1]-0.02, pch = "^", xlim = c(0, 1), 
                   ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                   xlab = "", cex = 1.5*size[1:N1], col = "orange"); 
            points(x = xloc[1:N1], yloc[1:N1], pch = 25, xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", cex = size[1:N1], bg = "blue");
            lenmeas <- paste("Measured Length = ", totlen[N1], " (mm)");
            text(x = 0.6, y = 0.99, labels = lenmeas, col = "blue", cex = 1.8,
                 bg = "grey");
        }else{
            axis(side = 1, at = minlen:maxlen, cex.axis = 1.5, lwd = 3, 
                 line = -0.75);
            par(mar = c(0, 0, 0, 0), lwd = 1, xaxt = "n", yaxt = "n");
            plot(x = xloc[1:N1], yloc[1:N1], pch = 25, xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", type = "n", bg = "blue");
        }
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
