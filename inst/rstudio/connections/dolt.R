# Connect to a Dolt database
conn <- doltr::dolt_pane(
  doltr::dolt(
    dir = "${0:Directory (or 'remote')=doltdb}",
    dbname = "${1:Database Name=doltdb}",
    username = "${2:User=root}",
    password = "${3:Password}",
    port = ${4:Port=3306},
    host = "${5:Host=127.0.0.1}",
    cache_connection = ${6:Cache Connection=TRUE}
  ))
