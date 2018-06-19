
module Database 
  ( createTableTrafficSignal,
    selectAllTrafficSignals,
    insertTrafficSignal,
    insertDataFromCSV,
    getTrafficSignalById,
    getAllTrafficSignals
  ) where


import Database.HDBC
import Database.HDBC.Sqlite3
import CSVParsing
import Text.ParserCombinators.Parsec
import Model.TrafficSignal

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


trafficSignalFromSql [id, localizacao1, localizacao2, func, utl, sinalsonoro, sinalizadorciclista, latitude, longitude] =
    TrafficSignal {
    trafficId =  fromSql id, 
    localizacao1 = fromSql localizacao1, 
    localizacao2 = fromSql localizacao2,
    funcionamento = fromSql func,
    utilizacao = fromSql utl,
    sinalSonoro = fromSql sinalsonoro,
    sinalizadorCiclista = fromSql sinalizadorciclista,
    latitude = fromSql latitude,
    longitude = fromSql longitude
}


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

insertDataFromCSV :: IO ()
insertDataFromCSV = do
    l <- readDataFromCSV
    insertAllLines (parseData l)

insertAllLines :: Either ParseError [[String]] -> IO ()
insertAllLines (Right []) = return ()
insertAllLines (Right (x:xs)) = do 
    insertTrafficSignal (elemIndex x 1) (elemIndex x 2) (elemIndex x 3) (elemIndex x 4) (toChar (elemIndex x 5)) (toChar (elemIndex x 6)) (read (elemIndex x 7)::Double) (read (elemIndex x 8)::Double) 
    insertAllLines (Right xs)


getTrafficSignalById :: Int -> IO TrafficSignal
getTrafficSignalById id = do
    conn <- connect
    result <- quickQuery' conn "SELECT * FROM TrafficSignal WHERE id == ? " [toSql (id::Int)]
    return $ trafficSignalFromSql $ head result

getAllTrafficSignals :: IO [TrafficSignal]
getAllTrafficSignals = do
    conn <- connect
    results <- quickQuery' conn "SELECT * FROM TrafficSignal" []
    return $ map trafficSignalFromSql results

updateTrafficSignalStatus :: IO ()
updateTrafficSignalStatus = do
    con <- connect
    stmt <- prepare conn "update TrafficSignal set status = (status + 1) % 3 lastUpdate = now() where now() - lastUpdate > "
    result <- execute stmt
    commit conn
    disconnect conn