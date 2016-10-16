
setwd('path/webscraping')
library("rvest")
library("lubridate")


###########webscraping from yelp page####################
laz_yelp_data1 <- c()
laz_yelp_data2 <- c()
laz_yelp_data <- c()

laz_yelp_scrap <- function(path)
{
  laz_yelp_path = path
  laz_yelp_html = read_html(laz_yelp_path)
  laz_yelp_tag = html_nodes(x = laz_yelp_html, css = "p")
  laz_yelp_text = html_text(laz_yelp_tag)
  laz_yelp_data = c()
  for (i in  0:length(laz_yelp_text))
  {
    laz_yelp_data[i] =  laz_yelp_text[i]
  }
  return (laz_yelp_data)
}

laz_yelp_data1 <- laz_yelp_scrap('https://www.yelp.com/biz/laz-fly-economy-parking-windsor-locks-3')
laz_yelp_data2 <- laz_yelp_scrap('https://www.yelp.com/biz/laz-fly-economy-parking-windsor-locks-3?start=20')
laz_yelp_data = c(laz_yelp_data1[-c(1,2)],laz_yelp_data2[-c(1,2)])

write(laz_yelp_data, file = "laz_yelp_data.txt",  
      ncolumns = if(is.character(laz_yelp_data)) 1 else 5,
      append = FALSE, sep = " ")


##########webscraping from groupon######################
laz_groupon_scrap <- function(path)
{
  laz_groupon_path = path
  laz_groupon_html = read_html(laz_groupon_path)
  laz_groupon_tag = html_nodes(x = laz_groupon_html, css = ".tip-description")
  laz_groupon_text = html_text(laz_groupon_tag)
  laz_groupon_data = c()
  for (i in  0:length(laz_groupon_text))
  {
    laz_groupon_data[i] =  laz_groupon_text[i]
  }
  return (laz_groupon_data)
}

laz_groupon_data_1 = laz_groupon_scrap('https://www.groupon.com/biz/windsor-locks-ct/laz-fly-airport-parking-1')
laz_groupon_data_2 = laz_groupon_scrap('https://www.groupon.com/biz/windsor-locks-ct/laz-fly-airport-parking-1?tips_page=2')
laz_groupon_data_3 = laz_groupon_scrap('https://www.groupon.com/biz/windsor-locks-ct/laz-fly-airport-parking-1?tips_page=3')
laz_groupon_data_4 = laz_groupon_scrap('https://www.groupon.com/biz/windsor-locks-ct/laz-fly-airport-parking-1?tips_page=4')
laz_groupon_data <- c(laz_groupon_data_1,laz_groupon_data_2,laz_groupon_data_3,laz_groupon_data_4)

write(laz_groupon_data, file = "laz_groupon_data.txt",  
      ncolumns = if(is.character(laz_groupon_data)) 1 else 5,
      append = FALSE, sep = " ")


