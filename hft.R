## thanks https://github.com/mbostock/d3/wiki/Chord-Layout
## extend Ben Hunter's implementation of chord diagram in rCharts
## http://mostlyconjecture.com/2014/05/03/chord-diagrams-with-rcharts/

## hft data from
## Baron, Matthew and Brogaard, Jonathan and Kirilenko, Andrei A.,
## Risk and Return in High Frequency Trading
## (May 5, 2014)
## Available at SSRN: http://ssrn.com/abstract=2433118

require(rCharts)
require(reshape2)

#tradeFlowDollar <- readClipboard()
load("hftData.RData")

cleanup <- function(d, expectCol = 10){
  df <- data.frame(do.call(rbind,lapply(
    strsplit( d, " " )
    ,function(x) {
      x = gsub(  #eliminate $ signs from the numbers
        x = x
        ,pattern = "[\\$\\,\r]"
        ,replacement = ""
      )
      if (length(x) == expectCol){  #since space delimited combine row names that are two words
        x[1] <- paste0(x[1],x[2])
        x <- x[-2]
      }
      return(x)
    }
  )),stringsAsFactors = F)
  #copy paste loses superscript so replace ? with lower case superscript
  df[1:3,1] <- paste0("HFT",c("a","m","p"))
  #name rows
  rownames(df) <- c(df[,1])
  #name columns
  colnames(df) <- c("source", df[,1])
  #make numeric
  df[,-1] <- lapply( df[,-1], as.numeric )
  
  return(df)
}

tradeFlow.df <- cleanup(tradeFlow)
tradeFlowDollar.df <- cleanup(tradeFlowDollar,expectCol=11)[,-(9:11)]

#this step is not necessary for the chord diagram
#for something other than chord diagram often data format will be more like this
#melted with source, target, and value
tradeFlow.melt <- melt(
  tradeFlow.df[-nrow(tradeFlow.df),-ncol(tradeFlow.df)]
  ,id.vars = 1
  ,variable.name = "target"
  ,value.name = "profit"
)

tradeFlowDollar.melt <- melt(
  tradeFlowDollar.df
  ,id.vars = 1
  ,variable.name = "target"
  ,value.name = "profit"
)

#make negative 0 to work with network diagrams
tradeFlow.df[-1] <- lapply(tradeFlow.df[,-1],function(x){ifelse(x<0,0,x)})
tradeFlowDollar.df[-1] <- lapply(tradeFlowDollar.df[,-1],function(x){ifelse(x<0,0,x)})
#make negative positive to work with network diagrams
#tradeFlow.df[,-1] <- abs(tradeFlow.df[,-1])


### rCharts custom chord diagram courtesy of Ben Hunter
### http://mostlyconjecture.com/2014/05/03/chord-diagrams-with-rcharts/
### all attribution should go there

ChordDiagram = setRefClass('ChordDiagram', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper()
    LIB <<- get_lib("http://timelyportfolio.github.io/rCharts_chord")
    lib <<- "chord_diagram"
    templates$script <<- '
    <script type="text/javascript">
    function draw{{chartId}}(){
      var params = {{{ chartParams }}}
      var chart = {{{ chordD }}}
      
      d3.select("#" + params.id) 
        .datum({"data":{{{data}}}, "matrix":{{{matrix}}} })
        .call(chart)
        return chart;
      };
      
      //$(document).ready(function(){
        draw{{chartId}}()
    //});
    
    </script>'
  },
  getPayload = function(chartId){
    chordD = toChain(params[!(names(params) %in% c('dom', 'data', 'matrix'))], "d3.chordDiagram()")
    chartParams = RJSONIO:::toJSON(params)
    list(chordD = chordD, chartParams = chartParams, data=toJSONArray(params[['data']]),
         matrix=toJSONArray(params[['matrix']]), chartId = chartId, lib = basename(lib), liburl = LIB$url
    )
  }
))

options(viewer=NULL)
ch <- ChordDiagram$new()
ch$params$data = lapply(
  1:nrow(tradeFlow.df)
  ,function(x){
    list(
      name = tradeFlow.df[,1][x]
      ,color = RColorBrewer::brewer.pal(9, name = "Blues")[x]
    )
  }
)
ch$params$matrix <- unname(as.matrix(tradeFlow.df[-nrow(tradeFlow.df),-1]))
ch$params$height <- 700
ch$params$width <- 700
ch$params$id <- "chorddiagram"
ch$params$removeSmall = T
ch$params$titleText = "#!
function(d){
    return d.source.name + ' takes ' + d3.format(',.2f')(d.d.source.value) + '/per trade from ' + d.target.name
}!#"
ch
#ch$save("hft.html",cdn=T)


ch2 <- ChordDiagram$new()
ch2$params$data = lapply(
  1:nrow(tradeFlowDollar.df)
  ,function(x){
    list(
      name = tradeFlowDollar.df[,1][x]
      ,color = RColorBrewer::brewer.pal(9, name = "Blues")[x]
    )
  }
)
ch2$params$matrix <- unname(as.matrix(tradeFlowDollar.df[-nrow(tradeFlow.df),-1]))
ch2$params$height <- 700
ch2$params$width <- 700
ch2$params$id <- "chorddiagram"
ch2$params$removeSmall = T
ch2$params$titleText = "#!
function(d){
    return d.source.name + ' takes $' + d3.format(',.f')(d.d.source.value) + ' from '  +d.target.name
}!#"
ch2
ch2$save("hft_dollar.html",cdn=T)


require(ggplot2)
ggplot(data=tradeFlow.melt,aes(x=target,y=profit)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~source, ncol=1) + coord_flip()
ggplot(data=tradeFlowDollar.melt,aes(x=target,y=profit)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~source, ncol=1) + coord_flip()
