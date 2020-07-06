library(tidyverse)
library(data.table)
library(DT)
#library(class) # for kNN classification algorithm 
library(BBmisc) # for normalization
library(gmodels) # for model evaluation
library(plotly) # for interactive visualization
library(knitr)
library(kableExtra)
library(shiny)
library(rgl)
library(plot3Drgl)
library(shinyRGL)
library(pander)
library(splines)
library(visreg)
library(grid)
library(gridExtra)
library(stargazer)
library(corrplot)
library(psych)
library(fastDummies)
library(IRdisplay)

load("../../data/RData/house_prices.RData")

dt_house <- datatable(
  house_prices,
  filter = "top",
  options = list(pageLength = 10)
)

dt_house

house_prices[,summary(.SD)]

vars <- names(house_prices)
vars # variables

var_1 <- vars[1] # selected variable

var_3 <- 5 # number of bins

var_1
var_3

house_sub <- house_prices %>% dplyr::filter()

p1 <- ggplot(house_sub, aes(x = house_sub[[var_1]])) +
        geom_histogram(bins = var_3, aes(label=..count..)) +
        labs(x = var_1)
p1a <- ggplotly(p1, tooltip = "label")

p2 <- ggplot(house_sub, aes(x = 0, y = house_sub[[var_1]])) +
        geom_boxplot() +
        theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        coord_flip() +
        labs(x = "", y = var_1)
  
subplot(p1a, p2, nrows = 1)

var_11 <- vars[1] # first variable selected
var_12 <- vars[2] # second variable selected
var_10 <- 5 # number of bins

var_11
var_12
var_10

house_sub <- house_prices

x2 <- house_sub[[var_11]]
y2 <- house_sub[[var_12]]
labx <- var_11
laby <- var_12
bins <- var_10

##  Create cuts:
x_c <- cut(x2, as.integer(bins))
y_c <- cut(y2, as.integer(bins))

##  Calculate joint counts at cut levels:
z1 <- table(x_c, y_c)

## Calculate break points
breaks <- function(vec) (max(vec) - min(vec)) / bins * 1:bins + min(vec)

xb <- breaks(x2)
yb <- breaks(y2)

##  Plot as a 3D histogram:
hist3D1 <- plot3Drgl::hist3Drgl(x = xb,
                              y = yb,
                              z = z1,
                              axes = T,
                              label = T,
                              xlab = labx,
                              ylab = laby,
                              zlab = "frequencies",
                              nticks = bins,
                              ticktype = "detailed"
                              )
#hist3D1
scene <- scene3d()
#rgl::rglwidget()
rgl::rglwidget(scene)

var_21 <- vars[1] # selected variable
var_21

p1 <-  ggplot(house_prices, mapping = aes(x = house_prices[[var_21]])
) +
        geom_density(adjust = 3) +
        guides(color=guide_legend(title=NULL)) +
        labs(x = var_21)
        
ggplotly(p1, tooltip = "density")

var_4 <- vars[1] # first variable selected
var_5 <- vars[2] # second variable selected

var_4
var_5

p <-  ggplot(house_prices, mapping = aes(x = house_prices[[var_4]],
                                 y = house_prices[[var_5]]),
) +
        geom_point(mapping = aes(text = paste(var_4, ": ", house_prices[[var_4]], "\n",
                                              var_5, ": ", house_prices[[var_5]]))) + 
        geom_smooth() +
        guides(color=guide_legend(title=NULL)) +
        labs(x = var_4, y = var_5)
        
ggplotly(p, tooltip = c("text"))


var_31 <- vars[1] # first variable selected
var_32 <- vars[2] # second variable selected
var_33 <- vars[3] # third variable selected

var_31
var_32
var_33

plot_ly(x = house_prices[[var_31]],
      y = house_prices[[var_32]],
      z = house_prices[[var_33]],
      type = "scatter3d") %>%
plotly::layout(scene = list(
  xaxis = list(title = var_31),
  yaxis = list(title = var_32),
  zaxis = list(title = var_33)))

indepvars <- names(house_prices) %>% setdiff("Price")

formula1 <- reformulate(indepvars, "Price")

house_model <- lm(formula1, data = house_prices)

vars <- setdiff(names(house_prices), "Price")
sampsize <- house_prices[,.N]

var_41 <- vars[1] # first variable selected
var_42 <- vars[2] # second variable selected
var_43 <- sampsize # samplesize

var_41
var_42
var_43

house_prices2 <- house_prices[sample(.N, var_43)]
model <- lm(formula1, data = house_prices2)
house_model2 <- list(datax = house_prices2, model = model)
modelx <- house_model2
house_model2

capture.output(stargazer(house_model2$model,
                      report = "vctps*",
                      ci=TRUE,
                      ci.level=0.90,
                      single.row=TRUE,
                      digits = 2,
                     #multicolumn = T,
                     #single.row = F,
                     type = "html")) %>%
paste(collapse = "\n") %>%
IRdisplay::display_html()

fit <- modelx$model
fit$data <- modelx$datax
persp <- visreg2d(fit,
                  var_41,
                  var_42,
                  plot.type = "persp", gg = T)
persp

fit <- modelx$model
fit$data <- modelx$datax
persp <- visreg2d(fit,
                  var_41,
                  var_42,
                  plot.type = "image", gg = T)
persp

fit <- modelx$model
fit$data <- modelx$datax

plots <- lapply(vars, function(x) visreg(fit, x, gg = T))
do.call(grid.arrange, c(plots, nrow = 1))  

vars <- setdiff(names(house_prices), "Price")
sampsize <- house_prices[,.N]

var_53 <- sampsize # sample size
var_53

house_prices3 <- house_prices[sample(.N, var_53)]
house_model3 <- lapply(vars, function(x){
    formula2 <- reformulate(x, "Price")
    lm(formula2, data = house_prices3)
    }
  )

house_model3

capture.output(do.call(stargazer, c(house_model3,
                                    report = "vctps*",
                                    ci=TRUE,
                                    ci.level=0.90,
                                    single.row=TRUE,
                                    digits = 2,
                                    type = "html"))) %>%
paste(collapse = "\n") %>%
IRdisplay::display_html()

fit <- house_model3

plots <- lapply(fit, function(x) visreg(x, gg = T))
do.call(grid.arrange, c(plots, nrow = 1))  

house_prices %>% cor() %>%

corrplot::corrplot.mixed(upper = "ellipse",
                         lower = "number",
                         tl.pos = "lt",
                         number.cex = .5,
                         lower.col = "black",
                         tl.cex = 0.7)

house_prices %>% psych::pairs.panels()


