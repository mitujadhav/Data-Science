library(RODBC)

# initialize the connection
ch <- odbcConnect("markdown")

# run the query
df = sqlQuery(ch, paste("select columns[0] as PARAM2 FROM dfs.`C:\\Users\\madhumitaj\\Desktop\\event2.tsv`;"))


read.parquet(sqlContext, "D:/tmp/merged_events/0_0_0.parquet")

parquetFile(sqlContext, ...)


library(RODBC)
#Local Machine
# initialize the connection
ch <- odbcConnect("MapR ODBC Driver for Drill DSN")
# run the query
df = sqlQuery(ch, paste("SELECT * FROM `dfs`.`tmp`.`./merged_events`"))
odbcClose(ch)


# For Remote Machine
ch <- odbcConnect("remote")
df = sqlQuery(ch, paste("SELECT * FROM `dfs`.`tmp`.`./abc`"))











