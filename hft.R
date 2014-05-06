## thanks https://github.com/mbostock/d3/wiki/Chord-Layout
## extend Ben Hunter's implementation of chord diagram in rCharts
## http://mostlyconjecture.com/2014/05/03/chord-diagrams-with-rcharts/



require(reshape2)

#tradeFlow <- readClipboard()
load.data("")

tradeFlow.df <- data.frame(do.call(rbind,lapply(
  strsplit( tradeFlow, " " )
  ,function(x) {
    x = gsub(  #eliminate $ signs from the numbers
      x = x
      ,pattern = "\\$"
      ,replacement = ""
    )
    if (length(x) == 10){  #since space delimited combine row names that are two words
      x[1] <- paste0(x[1],x[2])
      x <- x[-2]
    }
    return(x)
  }
)),stringsAsFactors = F)
#copy paste loses superscript so replace ? with lower case superscript
tradeFlow.df[1:3,1] <- paste0("HFT",c("a","m","p"))
#name rows
rownames(tradeFlow.df) <- c(tradeFlow.df[,1])
#name columns
colnames(tradeFlow.df) <- c("source", tradeFlow.df[,1])
#make numeric
tradeFlow.df[,-1] <- lapply( tradeFlow.df[,-1], as.numeric )

#make negative 0 to work with network diagrams
tradeFlow.df[-1] <- lapply(tradeFlow.df[,-1],function(x){ifelse(x<0,0,x)})
#make negative positive to work with network diagrams
#tradeFlow.df[,-1] <- abs(tradeFlow.df[,-1])



#this step is not necessary for the chord diagram
#for something other than chord diagram often data format will be more like this
#melted with source, target, and value
tradeFlow.melt <- melt(
  tradeFlow.df[-nrow(tradeFlow.df),-ncol(tradeFlow.df)]
  ,id.vars = 1
  ,variable.name = "target"
  ,value.name = "weight"
)


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
ch
