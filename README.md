This small project uses mostly `shiny` and `gpplot2` packages for `R` to visualize Belgian inflation data as a modest web application. Data is imported (and so heavily dependent on its format) from an Excel/CSV file provided by Belgian SPF Economie.

[Project hosting](http://spark.rstudio.com/rytis/inflation) is kindly provided by RStudio.

**UPDATE**: On February 11, 2014 the format of the Excel file changed with an update to the index. Source files have not been updated yet as I hope to eliminate the need for Excel file all together and sync data directly from the SPF data server. I am waiting for their response.

** UPDATE 2**: As per answer of SPF Economie, the only way to get the data now is a manual download of Excel/CSV files. Even though they use an OLAP server for data storage, there is apparently no possibility for now to have [XMLA](https://en.wikipedia.org/wiki/XML_for_Analysis) activated. Similar services may be available as of beSTAT 2.0 but no dates are known.

Since the new index will have regular updates to the consumption bundle, I suspect that Excel may also change quite often which makes the previous method cumbersome and easily broken...
