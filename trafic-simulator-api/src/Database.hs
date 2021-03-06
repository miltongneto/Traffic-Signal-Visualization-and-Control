
module Database 
  ( createTableTrafficSignal,
    selectAllTrafficSignals,
    insertTrafficSignal,
    insertDataFromCSV,
    getTrafficSignalById,
    getAllTrafficSignals,
    updateTrafficSignalStatus,
    updateTrafficSignalById,
    forceChangeStatus
  ) where


import Database.HDBC
import Database.HDBC.Sqlite3
import CSVParsing
import Text.ParserCombinators.Parsec
import Model.TrafficSignal
import Data.Time

connect :: IO Connection
connect = connectSqlite3 "TrafficSignalDB.db"

--createTableSignal :: IO Connection
createTableTrafficSignal = do
    conn <- connect
    run conn "DROP TABLE TrafficSignal" []
    run conn "CREATE TABLE TrafficSignal (id INTEGER PRIMARY KEY, localizacao1 VARCHAR(100), localizacao2 VARCHAR(100), funcionamento VARCHAR(10), utilizacao VARCHAR(20), sinalsonoro VARCHAR(1), sinalizadorciclista VARCHAR(1), Latitude NUMERIC(20), Longitude NUMERIC(20), timeToClose INTEGER, timeToOpen INTEGER, status INTEGER, lastUpdate DATETIME)" []
    commit conn

{-
selectAllTrafficSignals :: IO ()
selectAllTrafficSignals = do
    conn <- connect
    result <- quickQuery' conn "SELECT * FROM TrafficSignal" []
    mapM_ print result
-}


trafficSignalFromSql [id, localizacao1, localizacao2, func, utl, sinalsonoro, sinalizadorciclista, latitude, longitude, timeToClose, timeToOpen, status, lastUpdate] =
    TrafficSignal {
    trafficId =  fromSql id, 
    localizacao1 = fromSql localizacao1, 
    localizacao2 = fromSql localizacao2,
    funcionamento = fromSql func,
    utilizacao = fromSql utl,
    sinalSonoro = fromSql sinalsonoro,
    sinalizadorCiclista = fromSql sinalizadorciclista,
    latitude = fromSql latitude,
    longitude = fromSql longitude,
    timeToClose = fromSql timeToClose,
    timeToOpen = fromSql timeToOpen,
    status = fromSql status,
    lastUpdate = fromSql lastUpdate
}


selectAllTrafficSignals :: IO ()
selectAllTrafficSignals = do
    conn <- connect
    stmt <- prepare conn "SELECT * FROM TrafficSignal"
    execute stmt []
    results <- fetchAllRowsAL stmt
    commit conn
    mapM_ print results

getTime = TimeOfDay 10 30 30
getDay = fromGregorian 2018 6 15 

getData = LocalTime getDay getTime

getLastId :: [[SqlValue]] -> Integer
getLastId [SqlNull:xs] = 0
getLastId [x:xs] = fromSql x

selectLastId = do
    conn <- connect
    result <- quickQuery' conn "SELECT MAX (id) FROM TrafficSignal" []
    disconnect conn
    return (getLastId result)

stmtInsertTrafficSignal :: Connection -> IO Statement
stmtInsertTrafficSignal conn = prepare conn "INSERT INTO TrafficSignal VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

stmtUpdateTrafficSignal :: Connection -> IO Statement
stmtUpdateTrafficSignal conn = prepare conn "UPDATE TrafficSignal SET localizacao1 = ?, localizacao2 = ?, funcionamento = ?, utilizacao = ?, sinalsonoro = ?, sinalizadorciclista = ?, Latitude = ?, Longitude = ?, timeToClose = ?, timeToOpen = ? WHERE id == ?"

stmtForceStatusChange :: Connection -> IO Statement
stmtForceStatusChange conn = prepare conn "UPDATE TrafficSignal SET status = (status + 1) % 3, lastUpdate = datetime('now') WHERE id == ?"

stmtUpdateAllStatus :: Connection -> IO Statement
stmtUpdateAllStatus conn = prepare conn "update TrafficSignal set status = (status + 1) % 3, lastUpdate = datetime('now') where Cast ((julianday('now') - julianday(lastUpdate)) * 24 * 60 * 60 As Integer) > CASE WHEN status = 0 then timeToClose ELSE CASE WHEN status = 1 then 1 ELSE timeToOpen END END"

insertTrafficSignalTest :: IO ()
insertTrafficSignalTest = do
    conn <- connect
    stmt <- stmtInsertTrafficSignal conn
    currentID <- selectLastId
    result <- execute stmt [toSql (currentID + 1), toSql "Boa Viagem Av.", toSql "Em frente ao parque Dona Lindú", toSql "E/Git", toSql "Ocasional", toSql 'N', toSql 'N', toSql (-8.142323::Double), toSql (-34.903798::Double)]
    commit conn
    disconnect conn

insertTrafficSignal :: String -> String -> String -> String -> Char -> Char -> Double -> Double -> Int -> Int -> Int -> LocalTime -> IO ()
insertTrafficSignal localizacao1 localizacao2 funcionamento utilizacao sinalsonoro sinalizadorciclista latitude longitude timeToClose timeToOpen status localtime = do
    conn <- connect
    stmt <- stmtInsertTrafficSignal conn
    currentID <- selectLastId
    result <- execute stmt [
        toSql (currentID + 1), 
        toSql localizacao1, 
        toSql localizacao2, 
        toSql funcionamento, 
        toSql utilizacao, 
        toSql sinalsonoro, 
        toSql sinalizadorciclista, 
        toSql latitude, 
        toSql longitude, 
        toSql timeToClose, 
        toSql timeToOpen, 
        toSql status, 
        toSql localtime]
    commit conn
    disconnect conn

insertDataFromCSV :: IO ()
insertDataFromCSV = do
    l <- readDataFromCSV
    insertAllLines (parseData l)

insertAllLines :: Either ParseError [[String]] -> IO ()
insertAllLines (Right []) = return ()
insertAllLines (Right (x:xs)) = do 
    insertTrafficSignal (elemIndex x 1) (elemIndex x 2) (elemIndex x 3) (elemIndex x 4) (toChar (elemIndex x 5)) (toChar (elemIndex x 6)) (read (elemIndex x 7)::Double) (read (elemIndex x 8)::Double) 15 10 0 getData
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
    conn <- connect
    stmt <- stmtUpdateAllStatus conn
    result <- execute stmt []
    commit conn
    disconnect conn

updateTrafficSignalById :: Int -> TrafficSignal -> IO ()
updateTrafficSignalById id trafficSignal = do
    conn <- connect
    stmt <- stmtUpdateTrafficSignal conn
    result <- execute stmt [
        toSql $ localizacao1 trafficSignal, 
        toSql $ localizacao2 trafficSignal, 
        toSql $ funcionamento trafficSignal, 
        toSql $ utilizacao trafficSignal, 
        toSql $ sinalSonoro trafficSignal, 
        toSql $ sinalizadorCiclista trafficSignal, 
        toSql $ latitude trafficSignal, 
        toSql $ longitude trafficSignal, 
        toSql $ timeToClose trafficSignal, 
        toSql $ timeToOpen trafficSignal,
        toSql id]
    commit conn
    disconnect conn

forceChangeStatus ::  Int -> IO ()
forceChangeStatus id = do
    conn <- connect
    stmt <- stmtForceStatusChange conn
    execute stmt [toSql id]
    commit conn
    disconnect conn
