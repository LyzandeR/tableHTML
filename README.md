# tableHTML

This is a package for creating easily CSS-ible HTML tables, which are compatible with R shiny.

# Installation

To install the package you need to have the `devtools` package installed. To install devtools type in your console: `install.packages('devtools')`.

Then to install tableHTML run the following on your console:

```R
devtools::install_github('lyzander/tableHTML')
```

# Usage

The package so far creates an easily CSS-ible HTML table without any CSS. By typing on your console:

```R
library(tableHTML)
tableHTML(mtcars)
```

an HTML table will appear on viewer in R studio and will look like this:

![simple_table](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/simple_table.PNG)

You can select whether to include row names by setting the rownames argument to FALSE:

```R
tableHTML(mtcars, rownames = FALSE)
```

![simple_table_no_rownames](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/table_no_rownames.PNG)

The class argument sets the class name for the table. The default will be of the form table_dataframeName. For example, for mtcars the HTML table's class would be table_mtcars. This makes it easy to create a css value for it in case there are multiple tables.

```R
mytable <- tableHTML(mtcars)
str(mytable)
Classes 'tableHTML', 'html', 'character'  atomic [1:1] <table class=table_mtcars border=1 style="border-collapse: collapse;">
<tr>
  <th id=header_1> </th>
  <th id=header_2>mpg</th>
  <th id=header_3>cyl</th>
truncated...
```
And also:

```R
mytable <- tableHTML(mtcars, class = 'myclass')
str(mytable)
Classes 'tableHTML', 'html', 'character'  atomic [1:1] <table class=myclass border=1 style="border-collapse: collapse;">
<tr>
  <th id=header_1> </th>
  <th id=header_2>mpg</th>
  <th id=header_3>cyl</th>
truncated...
```

In case you would like to add second headers that span multiple columns you can do it by providing a list in the second_header argument. The first element will contain the column spans (i.e. a numeric atomic vector) whereas the second element will contain the names (i.e. a character atomic vector).

As an example:

```R
tableHTML(mtcars, second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
```

![table_second_header](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/table_second_headers.PNG)

The widths argument specifies the columns' width in pixels. It needs to have the same length as the number of columns (if rownames is TRUE, rownames are also considered a column) As an example:

```R
 tableHTML(mtcars, widths = rep(100, 12), second_header = list(c(3,4,5), c('col1', 'col2', 'col3')))
```
![table_widths]()


Now that we know about how to use the function's arguments let's see how we can use tableHTML with shiny. The implementation is really simple since internally tableHTML uses `htmltools::HTML` to convert the table into an HTML object. Simply using `renderUI` and `uiOutput` will be enough:

```R
shinyApp(
 ui = fluidPage(
   fluidRow(
    #leave some spacing
    br(),
    column(width = 1),
    uiOutput("mytable"))
 ), 
 server = function(input, output) {
  output$mytable <- renderUI( 
   tableHTML(mtcars)
 )}
)
```

This will yield a very simple app that looks like this:

![shiny_table](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/Shiny_table.PNG)

Now that our very simple table has been created we can modify it using CSS. tableHTML creates ids for columns, headers and overheaders and gives the table a specific class name and it was mentioned previously.

The rules are like this:

* Table: Will get the class from the class argument in the function. The final class will be of the form table_dataframeName.

* Columns: Will get an id which will be the same as the column name

* Headers: Will get an id of the form header_headerIndex. For example the first header will have the id header_1, the second header will have header_2 and so on.

* Second_Header: Will get an id of the form overheader_secondHeaderIndex. For example the first second_header will have the id overheader_1, the second header will have overheader_2 and so on.

Let's see this in practice. We create a CSS file e.g. mycss.css where we will write our css code. Then we load this in R shiny in the following way. We have saved a ui.R file and server.R file in the same directory. In that directory we have a www/ folder where we save our .css file. In our case that is named mycss.css. We include this file in R using `includeCSS`:

```R
#ui.R
shinyUI(
 fluidPage(
  fluidRow(
   #leave some spacing
   br(),
   column(width = 1),
   includeCSS('www/mycss.css'),
   uiOutput("mytable"))
 )
)

#server.R
shinyServer(
function(input, output) {
   output$mytable <- renderUI( 
    tableHTML(mtcars, second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')))
   )}
)
```

Note: You can find out about good practices [here](http://shiny.rstudio.com/articles/css.html).

In order to add a vertical column line we can use the column ids created by tableHTML. To add a vertical red line at mpg column, our css file would look like this:

```CSS
#mpg {
	border-left: 5px solid red;
}
``` 

Our HTML table will look like this:

![shiny_vertical](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/shiny_vertical_line.PNG)

In order to change the background color of one of the headers (e.g. the second) our css would look like this:

```CSS
#mpg{
	border-left: 5px solid red;
}

#header_2 {
	background-color: green;
}
``` 
![shiny_back](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/shiny_header_color.PNG)


And in order to align the first of the second_headers in the center our css would look like:

```CSS
#mpg{
	border-left: 5px solid red;
}

#header_2 {
	background-color: green;
}

#overheader_1 {
	text-align: center;
}
``` 

![shiny_overheader](https://github.com/LyzandeR/tableHTML/blob/master/readme_files/shiny_overheader_center.PNG)

# Further Development

This package is still in its infancy and is still under development. The next release will implement CSS functions in order to add some CSS features from within the R functions.