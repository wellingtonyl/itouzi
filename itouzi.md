R语言编写爬虫抓取P2P平台爱投资的债券市场信息
================

缘起
----

前段时间（2016年10月），有网友在网贷天眼公开爆料，称P2P平台爱投资发布的涉及内蒙古赤峰市7家企业的借款项目造假，且涉及自融。消息传出，人心惶惶，平台投资者纷纷抛售手中的债券。由于该平台的标的均为中长期标的，要想回收投资必须将债券在该平台上的债券市场挂牌出售，有下家接盘才行。一时间，爱投资的债券市场上挂牌债权数量飙升，债券收益率也水涨船高，从事发前的12%左右蹿升至15%以上，个别甚至达到20%。彼时作为曾在该平台投资过的投资者，我曾密切关注爱投资债券市场的动态。只是当时还不会爬虫，无法将数据记录下来。

如今上述事件已经过去将近三个月了，事情可以说是不了了之。爱投资的债券市场又恢复了平静。那么会不会有下一次爆料呢？这个P2P平台会不会再次陷入动荡？能不能写一个动态记录债券市场信息的程序呢监视市场的情况呢？在有了爬虫和数据挖掘技术后，这一切都不困难了。

爬虫的编写
----------

R语言获取网页的包主要有rvest和RCurl两个。爱投资的债券市场信息显示在json格式的网页中，用rvest包无法抓取，这次轮到RCurl出马了。事实上这个网站的数据抓取非常简单，不需要登陆，只需调用getURL()函数即可。翻页功能通过for循环修改提交的网址实现。首先读取所有网页并储存在变量pages中，每个网页都是json格式的字符串。接下来比较困难的是提取网页中的有用信息，并组织成结构化的数据。这里用到了大量正则表达式，尤其是向前和向后预查，做这个项目顺便复习了正则。提取出来的字段有债券收益率，剩余时间，剩余金额，项目网址，借款开始日期，还款日期，借款方和担保方。其中得到的数字是形如“123,456.78”的字符串，需要自定义函数将其转化为数字。

下面是爬虫程序。

``` r
#library(rvest)
library(RCurl)         # 网页抓取
library(magrittr)      # 管道运算符
library(stringr)       # 字符处理，正则

date <- Sys.Date()               # 获取日期
hour <- str_sub(date(), 12, 13)  # 获取小时

### functions
## 把形如“123,456.78”的字符串转化为数字
char2num <- function(str) {
  k=-1                         # 取数位置，倒过来取
  i=-2                         # 10的多少次方
  n=0
  while (k>=-nchar(str)) {
    if (-k%%4!=3 ) {
      n <- n+ 10^(i)*as.integer(str_sub(str, k, k))
      k=k-1
      i=i+1
    }
    else k=k-1
  }
  return(n)
}

## 抓取itouzi债券市场所有网页，并记录抓取时间
itouzi_debt_pages <- function() {
  max_page <- getURL("https://www.itouzi.com/dinvest/ajax/debtList?page=1", encoding="UTF-8") %>%
    str_extract("(?<=\"nn\":)[0-9]{1,3}") %>% as.numeric()               # 获取页码
  pages <- c()
  for (i in 1: max_page) {
    temp_page <- getURL(paste0("https://www.itouzi.com/dinvest/ajax/debtList?page=", i), encoding="UTF-8") %>%
      paste0(Sys.time())                     # 加入时间戳
    pages %<>% c(temp_page)
    Sys.sleep(2)                             # 每抓一页休息两秒，防止被服务器封ip
  }
  return(pages)
}

## 把一个网页转化为数据表
## 字段包括利率intst，剩余时间（天）remain_time，剩余金额（元）remain_amount，项目网址detail_url，
## 借款开始时间begin_time，还款日期end_time，借款方borrow_name，担保方guar_info和信息抓取时间time_stamp。
page2df <- function(page) {
  data.frame(intst = str_extract_all(page, "(?<=real_apr_b.{3})\\d{1,2}\\.\\d{2}") %>%
               unlist() %>%
               sapply(char2num),
             remain_time = str_extract_all(page, "(?<=remain_time\":)[0-9]+") %>% 
               unlist %>%
               as.numeric(),
             remain_amount = str_extract_all(page, "(?<=remainingAmount\":\")([0-9]{1,3},){0,3}([0-9]{1,3}\\.[0-9]{2})") %>%
               unlist %>%
               sapply(char2num),
             detail_url = str_extract_all(page, "(?<=detailUrl\":\")/dinvest/debt/[a-zA-Z]+\\?id=[0-9a-z]{48}") %>%
               unlist %>%
               paste0("https://www.itouzi.com", .),
             begin_time = str_extract_all(page, "(?<=beginTime\":\")[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
               unlist() %>%
               as.Date(),
             end_time = str_extract_all(page, "(?<=endTime\":\")[0-9]{4}-[0-9]{2}-[0-9]{2}")%>%
               unlist() %>%
               as.Date(),
             borrow_name = str_extract_all(page, "(?<=\"name\":\").{4,40}(?=\",\"style_cn)") %>%
               unlist(),
             guar_info = str_extract_all(page, "(?<=\"name\":\").{4,40}(?=.{6}((type)|(pager)))") %>%
               unlist(),
             time_stamp = str_sub(page, -19, -1) %>% as.POSIXct(),      
             stringsAsFactors = F
  )
}

## 把所有网页转化为数据表，合并
itouzi_debt_results <- function(pages) {
  pages <- pages[which(nchar(pages)>200)]  # 除去空网页
  pages %>% lapply(page2df) %>%
    Reduce(rbind, .)                       # 合并所有表格
}
```

获得的数据
----------

下面展示了抓取的数据的前几行。

``` r
head(results)
```

    ##    intst remain_time remain_amount
    ## 1: 12.86         310          2200
    ## 2: 12.81         242          9000
    ## 3: 12.80         312          8800
    ## 4: 12.75         324          5000
    ## 5: 12.73         254         85000
    ## 6: 12.72         525          2800
    ##                                                                                                detail_url
    ## 1: https://www.itouzi.com/dinvest/debt/shengxinDetail?id=74545961675653545a454c374370546949506c6e75513d3d
    ## 2:         https://www.itouzi.com/dinvest/debt/detail?id=2b67416450367054464b796d3634416c62355a7239673d3d
    ## 3: https://www.itouzi.com/dinvest/debt/shengxinDetail?id=68452b6f77412f6154714f655275496169794c484b513d3d
    ## 4: https://www.itouzi.com/dinvest/debt/shengxinDetail?id=666f4646616f4f7161544d67463869702f2b463434673d3d
    ## 5:         https://www.itouzi.com/dinvest/debt/detail?id=763938754a6c31776e6a58647a4a514c6b312f5037673d3d
    ## 6:         https://www.itouzi.com/dinvest/debt/detail?id=6c526d7a53544f557353334e3766355159774e6c2f413d3d
    ##    begin_time   end_time                                    borrow_name
    ## 1: 2016-11-01 2017-11-01                           省心计划 I-201611016
    ## 2: 2016-08-25 2017-08-25                 珠宝商贸企业补充流动资金(三期)
    ## 3: 2016-11-03 2017-11-03                           省心计划 I-201611031
    ## 4: 2016-11-15 2017-11-15                           省心计划 I-201611152
    ## 5: 2016-09-06 2017-09-06                 生物科技企业补充流动资金(一期)
    ## 6: 2016-06-03 2018-06-04 金属制品生产企业设备回租应收租金转让项目(二期)
    ##                           guar_info          time_stamp
    ## 1: 中安融金（深圳）商业保理有限公司 2016-12-26 12:10:02
    ## 2:           鑫融基投资担保有限公司 2016-12-26 12:10:02
    ## 3: 中安融金（深圳）商业保理有限公司 2016-12-26 12:10:02
    ## 4:     微弘商业保理（深圳）有限公司 2016-12-26 12:10:02
    ## 5:           鑫融基投资担保有限公司 2016-12-26 12:10:02
    ## 6:     沣腾国际融资租赁有限责任公司 2016-12-26 12:10:02

定期自动抓取
------------

债券市场的情况是动态变化的，因此需要定时获取数据。这里我用到了.bat文件和Windows的计划任务。

R的脚本是可以通过命令行命令来执行的。首先需要将R的主程序所在的bin文件夹的路径加入到环境变量中。然后可以用如下命令执行R脚本（去掉\#）。更多的命令和参数详见W.N.Venables和D.M.Smith的"An Introduction to R——Notes on R: A Programming Enviroment for Data Analysis and Graphics" version 3.3.2 (2016-10-31) Appendix B。

``` r
#r CMD BATCH --no-restore --no-save C:\folder\name.R
```

将上述命令写入.bat文件，并加入计划任务中，设定每小时执行一次，获取的数据写入.csv文件中或导入数据库，即可定时获取爱投资债券市场的信息。
