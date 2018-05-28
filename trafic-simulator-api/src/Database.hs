
module Database 
  ( createTableTrafficSignal,
    selectAllTrafficSignals,
    insertTrafficSignal
  ) where


import Database.HDBC
import Database.HDBC.Sqlite3

connect :: IO Connection
connect = connectSqlite3 "TrafficSignalDB.db"

--createTableSignal :: IO Connection
createTableTrafficSignal = do
    conn <- connect
    run conn "CREATE TABLE TrafficSignal (id INTEGER PRIMARY KEY, localizacao1 VARCHAR(100), localizacao2 VARCHAR(100), funcionamento VARCHAR(10), utilizacao VARCHAR(20), sinalsonoro VARCHAR(1), sinalizadorciclista VARCHAR(1), Latitude NUMERIC(20), Longitude NUMERIC(20))" []
    commit conn

{-
selectAllTrafficSignals :: IO ()
selectAllTrafficSignals = do
    conn <- connect
    result <- quickQuery' conn "SELECT * FROM TrafficSignal" []
    mapM_ print result
-}

selectAllTrafficSignals :: IO ()
selectAllTrafficSignals = do
    conn <- connect
    stmt <- prepare conn "SELECT * FROM TrafficSignal"
    execute stmt []
    results <- fetchAllRowsAL stmt
    commit conn
    mapM_ print results


getLastId :: [[SqlValue]] -> Integer
getLastId [SqlNull:xs] = 0
getLastId [x:xs] = fromSql x

selectLastId = do
    conn <- connect
    result <- quickQuery' conn "SELECT MAX (id) FROM TrafficSignal" []
    disconnect conn
    return (getLastId result)

stmtInsertTrafficSignal :: Connection -> IO Statement
stmtInsertTrafficSignal conn = prepare conn "INSERT INTO TrafficSignal VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertTrafficSignalTest :: IO ()
insertTrafficSignalTest = do
    conn <- connect
    stmt <- stmtInsertTrafficSignal conn
    currentID <- selectLastId
    result <- execute stmt [toSql (currentID + 1), toSql "Boa Viagem Av.", toSql "Em frente ao parque Dona Lindú", toSql "E/Git", toSql "Ocasional", toSql 'N', toSql 'N', toSql (-8.142323::Double), toSql (-34.903798::Double)]
    commit conn
    disconnect conn

insertTrafficSignal :: String -> String -> String -> String -> Char -> Char -> Double -> Double -> IO ()
insertTrafficSignal localizacao1 localizacao2 funcionamento utilizacao sinalsonoro sinalizadorciclista latitude longitude = do
    conn <- connect
    stmt <- stmtInsertTrafficSignal conn
    currentID <- selectLastId
    result <- execute stmt [toSql (currentID + 1), toSql localizacao1, toSql localizacao2, toSql funcionamento, toSql utilizacao, toSql sinalsonoro, toSql sinalizadorciclista, toSql latitude, toSql longitude]
    commit conn
    disconnect conn