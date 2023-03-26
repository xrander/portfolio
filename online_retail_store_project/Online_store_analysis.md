---
title: "Online Retail Store Analysis"
author: "Olamide Adu"
date: "2023-03-25"
output:
   prettydoc::html_pretty:
    theme: Architect
    toc: true
    toc_float: false
    highlight: github
    code_theme: zenburn
    keep_md: true
---

# Online Retail Store Analysis

## Overview

The data used for this project was provided by TATA through theforage.com virtual experience platform. This is an analysis of a retail store data to give actionable insights that would assist business decision-making.

**The Data**

-   InvoiceNO: invoice code for purchase made

-   StockCode: the code of the object purchased when in stock

-   Description: the description of item purchased

-   Quantity: the amount of the product purchased

-   InvoiceDate: the year,month, date, hour, minute and seconds of item purchase

-   Unitprice: the cost for one of the product

-   CustomerID: a number representing and tagging a particular customer

-   Country: the country where purchase was made.


```r
# Importing libraries
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   1.0.1 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(rnaturalearth)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.2.3
```

```
## 
## Attaching package: 'plotly'
## 
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following object is masked from 'package:graphics':
## 
##     layout
```

Next, I imported the data which is in xlsx format. The file is fairly large and can be downloaded at '<https://github.com/xrander/online_retail_store_project/blob/master/Online%20Retail.xlsx>'. The file is already available on my local computer and further analysis are made based on this


```r
# Setting working directory

setwd("C:/Users/aduol/Documents/Personal/Online_retail_store/online_retail_store_project/")

# to replicate this project, use your device directory here, in the format setwd('C:/users/myworkingdirectory') 

#importing data
online_store <- read_excel('Online Retail.xlsx')

#saving the file in csv format to reduce processing time by R
write.csv(online_store,
    "C:/Users/aduol/Documents/Personal/Online_retail_store/online_retail_store_project/online_store.csv",
    row.names = F)

#loading data again
online_store <- read.csv('online_store.csv')
```

Data types do change after converting 'xlsx' to 'csv', it is therefore necessary to investigate the data before attempting any questions. This can also provide some insight to the data which aids analysis


```r
head(online_store) #to get a quick preview of the table
```

```
##   InvoiceNo StockCode                         Description Quantity
## 1    536365    85123A  WHITE HANGING HEART T-LIGHT HOLDER        6
## 2    536365     71053                 WHITE METAL LANTERN        6
## 3    536365    84406B      CREAM CUPID HEARTS COAT HANGER        8
## 4    536365    84029G KNITTED UNION FLAG HOT WATER BOTTLE        6
## 5    536365    84029E      RED WOOLLY HOTTIE WHITE HEART.        6
## 6    536365     22752        SET 7 BABUSHKA NESTING BOXES        2
##           InvoiceDate UnitPrice CustomerID        Country
## 1 2010-12-01 08:26:00      2.55      17850 United Kingdom
## 2 2010-12-01 08:26:00      3.39      17850 United Kingdom
## 3 2010-12-01 08:26:00      2.75      17850 United Kingdom
## 4 2010-12-01 08:26:00      3.39      17850 United Kingdom
## 5 2010-12-01 08:26:00      3.39      17850 United Kingdom
## 6 2010-12-01 08:26:00      7.65      17850 United Kingdom
```

```r
summary(online_store)
```

```
##   InvoiceNo          StockCode         Description           Quantity        
##  Length:541909      Length:541909      Length:541909      Min.   :-80995.00  
##  Class :character   Class :character   Class :character   1st Qu.:     1.00  
##  Mode  :character   Mode  :character   Mode  :character   Median :     3.00  
##                                                           Mean   :     9.55  
##                                                           3rd Qu.:    10.00  
##                                                           Max.   : 80995.00  
##                                                                              
##  InvoiceDate          UnitPrice           CustomerID       Country         
##  Length:541909      Min.   :-11062.06   Min.   :12346    Length:541909     
##  Class :character   1st Qu.:     1.25   1st Qu.:13953    Class :character  
##  Mode  :character   Median :     2.08   Median :15152    Mode  :character  
##                     Mean   :     4.61   Mean   :15288                      
##                     3rd Qu.:     4.13   3rd Qu.:16791                      
##                     Max.   : 38970.00   Max.   :18287                      
##                                         NA's   :135080
```

```r
str(online_store)
```

```
## 'data.frame':	541909 obs. of  8 variables:
##  $ InvoiceNo  : chr  "536365" "536365" "536365" "536365" ...
##  $ StockCode  : chr  "85123A" "71053" "84406B" "84029G" ...
##  $ Description: chr  "WHITE HANGING HEART T-LIGHT HOLDER" "WHITE METAL LANTERN" "CREAM CUPID HEARTS COAT HANGER" "KNITTED UNION FLAG HOT WATER BOTTLE" ...
##  $ Quantity   : int  6 6 8 6 6 2 6 6 6 32 ...
##  $ InvoiceDate: chr  "2010-12-01 08:26:00" "2010-12-01 08:26:00" "2010-12-01 08:26:00" "2010-12-01 08:26:00" ...
##  $ UnitPrice  : num  2.55 3.39 2.75 3.39 3.39 7.65 4.25 1.85 1.85 1.69 ...
##  $ CustomerID : int  17850 17850 17850 17850 17850 17850 17850 17850 17850 13047 ...
##  $ Country    : chr  "United Kingdom" "United Kingdom" "United Kingdom" "United Kingdom" ...
```

```r
online_store$CustomerID <- as.factor(online_store$CustomerID)# changing data types

# The POSIXct data type for InvoiceDate have been changed to character when file was converted.

online_store <- online_store %>%
  mutate(InvoiceDate = as.POSIXct(InvoiceDate, format = "%Y-%m-%d %H:%M:%S")) # Changing character type to Posixct
```

## Questions

1.  Which region is generating the highest revenue and which region is generating the lowest?


```r
# We need to estimate the revenue generated first before estimating revenue per region
online_store <- online_store %>%
    mutate(Revenue = UnitPrice * Quantity)

regional_sales <- online_store %>%
  select(Country, Revenue, CustomerID) %>%
  filter(Country != 'United Kingdom') %>%
  group_by(Country) %>%
  summarize(total_revenue = sum(Revenue),
            average_revenue = mean(Revenue) #Estimates of the revenue per region
            )
```

The United Kingdom is having more than twice the revenue of the second place country and is excluded from the analysis of this question moving forward


```r
# Estimating the 10 highest earning regions
top_10 <- regional_sales %>% top_n(10, wt = total_revenue)


top_countries <- ggplot(top_10,
                        aes(reorder(Country, total_revenue),
                            total_revenue/10000))+
  geom_bar(aes(fill = Country),
           stat = 'identity')+
  labs(title = 'Top Revenue Generating regions Excluding the UK',
       x = 'Country',
       y = 'Revenue generated in tens of thousands')+
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))
# Estimating the 10 lowest revenue generating regions
bottom_10 <- regional_sales %>% top_n(-10, total_revenue)

least_countries <- ggplot(bottom_10,
                        aes(reorder(Country, total_revenue),
                            total_revenue))+
  geom_bar(aes(fill = Country),
           stat = 'identity')+
  labs(title = 'Least Revenue Generating regions',
       x = 'Country',
       y ='Revenue generated') +
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))

lc <- ggplotly(least_countries)
tc <- ggplotly(top_countries)

lc
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-64001649c107b61af548" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-64001649c107b61af548">{"x":{"data":[{"orientation":"v","width":0.9,"base":0,"x":[2],"y":[548.4],"text":"reorder(Country, total_revenue): Bahrain<br />total_revenue:  548.40<br />Country: Bahrain","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Bahrain","legendgroup":"Bahrain","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[5],"y":[1143.6],"text":"reorder(Country, total_revenue): Brazil<br />total_revenue: 1143.60<br />Country: Brazil","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(216,144,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Brazil","legendgroup":"Brazil","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[3],"y":[707.72],"text":"reorder(Country, total_revenue): Czech Republic<br />total_revenue:  707.72<br />Country: Czech Republic","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Czech Republic","legendgroup":"Czech Republic","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[6],"y":[1291.75],"text":"reorder(Country, total_revenue): European Community<br />total_revenue: 1291.75<br />Country: European Community","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(57,182,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"European Community","legendgroup":"European Community","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[8],"y":[1693.88],"text":"reorder(Country, total_revenue): Lebanon<br />total_revenue: 1693.88<br />Country: Lebanon","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Lebanon","legendgroup":"Lebanon","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[7],"y":[1661.06],"text":"reorder(Country, total_revenue): Lithuania<br />total_revenue: 1661.06<br />Country: Lithuania","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Lithuania","legendgroup":"Lithuania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[4],"y":[1002.31],"text":"reorder(Country, total_revenue): RSA<br />total_revenue: 1002.31<br />Country: RSA","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"RSA","legendgroup":"RSA","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[1],"y":[131.17],"text":"reorder(Country, total_revenue): Saudi Arabia<br />total_revenue:  131.17<br />Country: Saudi Arabia","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(149,144,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Saudi Arabia","legendgroup":"Saudi Arabia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[10],"y":[1902.28],"text":"reorder(Country, total_revenue): United Arab Emirates<br />total_revenue: 1902.28<br />Country: United Arab Emirates","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"United Arab Emirates","legendgroup":"United Arab Emirates","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[9],"y":[1730.92],"text":"reorder(Country, total_revenue): USA<br />total_revenue: 1730.92<br />Country: USA","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,98,188,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"USA","legendgroup":"USA","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"<b> Least Revenue Generating regions <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Saudi Arabia","Bahrain","Czech Republic","RSA","Brazil","European Community","Lithuania","Lebanon","USA","United Arab Emirates"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Saudi Arabia","Bahrain","Czech Republic","RSA","Brazil","European Community","Lithuania","Lebanon","USA","United Arab Emirates"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"<b> Country <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-95.114,1997.394],"tickmode":"array","ticktext":["0","500","1000","1500"],"tickvals":[0,500,1000,1500],"categoryorder":"array","categoryarray":["0","500","1000","1500"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"<b> Revenue generated <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"Country","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"55a86a413d78":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"55a86a413d78","visdat":{"55a86a413d78":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
tc
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-df7e5d1365ebc8d1d5c5" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-df7e5d1365ebc8d1d5c5">{"x":{"data":[{"orientation":"v","width":0.9,"base":0,"x":[6],"y":[13.707727],"text":"reorder(Country, total_revenue): Australia<br />total_revenue/10000: 13.707727<br />Country: Australia","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Australia","legendgroup":"Australia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[3],"y":[4.091096],"text":"reorder(Country, total_revenue): Belgium<br />total_revenue/10000:  4.091096<br />Country: Belgium","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(216,144,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Belgium","legendgroup":"Belgium","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[9],"y":[26.327682],"text":"reorder(Country, total_revenue): EIRE<br />total_revenue/10000: 26.327682<br />Country: EIRE","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"EIRE","legendgroup":"EIRE","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[7],"y":[19.74039],"text":"reorder(Country, total_revenue): France<br />total_revenue/10000: 19.740390<br />Country: France","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(57,182,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"France","legendgroup":"France","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[8],"y":[22.169821],"text":"reorder(Country, total_revenue): Germany<br />total_revenue/10000: 22.169821<br />Country: Germany","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Germany","legendgroup":"Germany","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[1],"y":[3.534062],"text":"reorder(Country, total_revenue): Japan<br />total_revenue/10000:  3.534062<br />Country: Japan","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Japan","legendgroup":"Japan","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[10],"y":[28.466154],"text":"reorder(Country, total_revenue): Netherlands<br />total_revenue/10000: 28.466154<br />Country: Netherlands","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Netherlands","legendgroup":"Netherlands","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[4],"y":[5.477458],"text":"reorder(Country, total_revenue): Spain<br />total_revenue/10000:  5.477458<br />Country: Spain","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(149,144,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Spain","legendgroup":"Spain","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[2],"y":[3.659591],"text":"reorder(Country, total_revenue): Sweden<br />total_revenue/10000:  3.659591<br />Country: Sweden","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Sweden","legendgroup":"Sweden","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[5],"y":[5.638535],"text":"reorder(Country, total_revenue): Switzerland<br />total_revenue/10000:  5.638535<br />Country: Switzerland","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,98,188,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Switzerland","legendgroup":"Switzerland","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"<b> Top Revenue Generating regions Excluding the UK <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Japan","Sweden","Belgium","Spain","Switzerland","Australia","France","Germany","EIRE","Netherlands"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Japan","Sweden","Belgium","Spain","Switzerland","Australia","France","Germany","EIRE","Netherlands"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"<b> Country <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.4233077,29.8894617],"tickmode":"array","ticktext":["0","10","20"],"tickvals":[0,10,20],"categoryorder":"array","categoryarray":["0","10","20"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"<b> Revenue generated in tens of thousands <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"Country","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"55a8828af2":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"55a8828af2","visdat":{"55a8828af2":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

2.  What is the monthly trend of revenue, which months have faced the biggest increase/decrease?


```r
# For this analysis we need to extract the months and date from InvoiceDate
online_store <- online_store %>%
  mutate(month_num = as.integer(format(InvoiceDate, format ='%m')),
         year = factor(format(InvoiceDate, format = '%Y'),
                       levels = c(2010, 2011)),
         month = factor(month.abb[month_num],
                        levels = c('Jan', 'Feb', 'Mar',
                                   'Apr', 'May', 'Jun',
                                   'Jul', 'Aug', 'Sep',
                                   'Oct', 'Nov', 'Dec')))


monthly_revenue <- online_store %>%
  select(month, year, Revenue) %>%
  group_by(year, month) %>%
  summarize(total_revenue = sum(Revenue),
            average_revenue = mean(Revenue),) %>%
  mutate(percent_change = (total_revenue - lag(total_revenue))/lag(total_revenue) * 100,
         month_num = as.integer(month))
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
mnth_chn <- ggplot(monthly_revenue, aes(month, percent_change))+
  geom_bar(aes(fill = month),
           na.rm = T,
           stat = 'identity')

ggplotly(mnth_chn)
```

```
## Warning: Removed 2 rows containing missing values (`position_stack()`).
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-0b1b26c0ba7308e7fce9" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0b1b26c0ba7308e7fce9">{"x":{"data":[{"orientation":"v","width":0.9,"base":-11.0602823648689,"x":[2],"y":[11.0602823648689],"text":"month: Feb<br />percent_change: 11.0602824<br />month: Feb","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Feb","legendgroup":"Feb","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[3],"y":[37.1849665900465],"text":"month: Mar<br />percent_change: 37.1849666<br />month: Mar","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(219,142,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Mar","legendgroup":"Mar","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":-27.8163495012814,"x":[4],"y":[27.8163495012814],"text":"month: Apr<br />percent_change: 27.8163495<br />month: Apr","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(174,162,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Apr","legendgroup":"Apr","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[5],"y":[46.6591781021751],"text":"month: May<br />percent_change: 46.6591781<br />month: May","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(100,178,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"May","legendgroup":"May","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":-4.45304822114491,"x":[6],"y":[4.45304822114491],"text":"month: Jun<br />percent_change:  4.4530482<br />month: Jun","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,189,92,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Jun","legendgroup":"Jun","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":-1.42131101040289,"x":[7],"y":[1.42131101040289],"text":"month: Jul<br />percent_change:  1.4213110<br />month: Jul","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,193,167,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Jul","legendgroup":"Jul","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[8],"y":[0.202612472493781],"text":"month: Aug<br />percent_change:  0.2026125<br />month: Aug","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,186,222,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Aug","legendgroup":"Aug","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[9],"y":[49.3652751270137],"text":"month: Sep<br />percent_change: 49.3652751<br />month: Sep","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,166,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Sep","legendgroup":"Sep","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[10],"y":[5.00320361837245],"text":"month: Oct<br />percent_change:  5.0032036<br />month: Oct","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(179,133,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Oct","legendgroup":"Oct","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[11],"y":[36.5228237960333],"text":"month: Nov<br />percent_change: 36.5228238<br />month: Nov","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(239,103,235,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Nov","legendgroup":"Nov","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":-70.3323991260513,"x":[12],"y":[70.3323991260513],"text":"month: Dec<br />percent_change: 70.3323991<br />month: Dec","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,99,182,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Dec","legendgroup":"Dec","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":43.1050228310502},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,12.6],"tickmode":"array","ticktext":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"month","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-76.3172828387045,55.3501588396669],"tickmode":"array","ticktext":["-75","-50","-25","0","25","50"],"tickvals":[-75,-50,-25,0,25,50],"categoryorder":"array","categoryarray":["-75","-50","-25","0","25","50"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"percent_change","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"month","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"55a85b3f74b4":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"55a85b3f74b4","visdat":{"55a85b3f74b4":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

3.  Which months generated the most revenue? Is there a seasonality in sales?


```r
mnth_in <- ggplot(monthly_revenue, aes(month, total_revenue)) +
  geom_bar(aes(fill = year),stat = 'identity') +
  scale_fill_manual(values = c('lightgreen', 'darkgreen'))

ggplotly(mnth_in)
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f1c0e1d60e9da3f53a9e" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f1c0e1d60e9da3f53a9e">{"x":{"data":[{"orientation":"v","width":0.899999999999999,"base":433668.01,"x":[12],"y":[748957.02],"text":"month: Dec<br />total_revenue:  748957.0<br />year: 2010","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(144,238,144,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"2010","legendgroup":"2010","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0,0,0],"x":[1,2,3,4,5,6,7,8,9,10,11,12],"y":[560000.26,498062.65,683267.08,493207.121,723333.51,691123.12,681300.111,682680.51,1019687.622,1070704.67,1461756.25,433668.01],"text":["month: Jan<br />total_revenue:  560000.3<br />year: 2011","month: Feb<br />total_revenue:  498062.6<br />year: 2011","month: Mar<br />total_revenue:  683267.1<br />year: 2011","month: Apr<br />total_revenue:  493207.1<br />year: 2011","month: May<br />total_revenue:  723333.5<br />year: 2011","month: Jun<br />total_revenue:  691123.1<br />year: 2011","month: Jul<br />total_revenue:  681300.1<br />year: 2011","month: Aug<br />total_revenue:  682680.5<br />year: 2011","month: Sep<br />total_revenue: 1019687.6<br />year: 2011","month: Oct<br />total_revenue: 1070704.7<br />year: 2011","month: Nov<br />total_revenue: 1461756.2<br />year: 2011","month: Dec<br />total_revenue:  433668.0<br />year: 2011"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,100,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"2011","legendgroup":"2011","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":66.4840182648402},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,12.6],"tickmode":"array","ticktext":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"month","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-73087.8125,1534844.0625],"tickmode":"array","ticktext":["0","500000","1000000","1500000"],"tickvals":[0,500000,1000000,1500000],"categoryorder":"array","categoryarray":["0","500000","1000000","1500000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"total_revenue","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"year","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"55a824f45eba":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"55a824f45eba","visdat":{"55a824f45eba":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

4.  Who are the top customers and how much do they contribute to the total revenue? Is the business dependent on these customers or is the customer base diversified


```r
cp <- online_store %>%
  select(CustomerID, Revenue) %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarize(number_of_purchase = length(CustomerID),
            total_purchase = sum(Revenue)) # This returns all the revenue generated from each customers and the number of times they made a purchase

# To get the top 10 customers that spent the most
top_10_cp <- cp %>% top_n(10, wt = total_purchase)

tp_10_customer<- top_10_cp %>%
  ggplot(aes(reorder(CustomerID, total_purchase/10000), y = total_purchase/10000, fill = CustomerID)) +
  geom_bar(stat =  'identity')+
  labs(title = 'Top spending customers',
       x = 'Customer ID',
       y = 'Amount spent in ten thousands')+
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'))

ggplotly(tp_10_customer)
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-e14b039c7f4a061e96a5" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-e14b039c7f4a061e96a5">{"x":{"data":[{"orientation":"v","width":0.9,"base":0,"x":[6],"y":[12.372545],"text":"reorder(CustomerID, total_purchase/10000): 12415<br />total_purchase/10000: 12.372545<br />CustomerID: 12415","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"12415","legendgroup":"12415","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[2],"y":[6.26531],"text":"reorder(CustomerID, total_purchase/10000): 13694<br />total_purchase/10000:  6.265310<br />CustomerID: 13694","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(216,144,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"13694","legendgroup":"13694","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[5],"y":[11.338414],"text":"reorder(CustomerID, total_purchase/10000): 14156<br />total_purchase/10000: 11.338414<br />CustomerID: 14156","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"14156","legendgroup":"14156","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[10],"y":[27.948902],"text":"reorder(CustomerID, total_purchase/10000): 14646<br />total_purchase/10000: 27.948902<br />CustomerID: 14646","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(57,182,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"14646","legendgroup":"14646","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[7],"y":[13.257262],"text":"reorder(CustomerID, total_purchase/10000): 14911<br />total_purchase/10000: 13.257262<br />CustomerID: 14911","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"14911","legendgroup":"14911","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[1],"y":[5.941934],"text":"reorder(CustomerID, total_purchase/10000): 15311<br />total_purchase/10000:  5.941934<br />CustomerID: 15311","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"15311","legendgroup":"15311","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[3],"y":[6.589208],"text":"reorder(CustomerID, total_purchase/10000): 16684<br />total_purchase/10000:  6.589208<br />CustomerID: 16684","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"16684","legendgroup":"16684","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[8],"y":[18.748217],"text":"reorder(CustomerID, total_purchase/10000): 17450<br />total_purchase/10000: 18.748217<br />CustomerID: 17450","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(149,144,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"17450","legendgroup":"17450","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[4],"y":[8.812538],"text":"reorder(CustomerID, total_purchase/10000): 17511<br />total_purchase/10000:  8.812538<br />CustomerID: 17511","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"17511","legendgroup":"17511","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[9],"y":[25.643849],"text":"reorder(CustomerID, total_purchase/10000): 18102<br />total_purchase/10000: 25.643849<br />CustomerID: 18102","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,98,188,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"18102","legendgroup":"18102","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"<b> Top spending customers <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["15311","13694","16684","17511","14156","12415","14911","17450","18102","14646"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["15311","13694","16684","17511","14156","12415","14911","17450","18102","14646"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"<b> Customer ID <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.3974451,29.3463471],"tickmode":"array","ticktext":["0","10","20"],"tickvals":[0,10,20],"categoryorder":"array","categoryarray":["0","10","20"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"<b> Amount spent in ten thousands <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"CustomerID","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"55a837ab3365":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"55a837ab3365","visdat":{"55a837ab3365":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

The identity of the top customers


```r
top_10_cp$CustomerID
```

```
##  [1] 12415 13694 14156 14646 14911 15311 16684 17450 17511 18102
## 4372 Levels: 12346 12347 12348 12349 12350 12352 12353 12354 12355 ... 18287
```

The contribution of the top customers to the total revenue


```r
# Proportion of top customers in the total purchase
sum(top_10_cp$total_purchase)/sum(online_store$Revenue) * 100
```

```
## [1] 14.04613
```

14% of the total revenue is by the top customers and this implies that the business is not diversified as losing this customers which is less than 0.0022873 will lead to a 14% drop in revenue. Ideally, we would want to have a larger number of customers contributing smaller amounts to our revenue, so that we are not overly reliant on any one customer or group of customers.

5.  Amount generated from return customers


```r
# Amount generated from customers with more than one order
online_store %>% select(CustomerID, Revenue) %>%
  filter(length(CustomerID)>1) %>%
  summarize(revenue = sum(Revenue))
```

```
##   revenue
## 1 9747748
```
