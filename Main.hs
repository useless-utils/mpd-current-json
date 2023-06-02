import Network.MPD

main = do
  s <- withMPD status
  case s of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right s' -> do
      printState (stState s')
      printTime s'

printTime :: Status -> IO ()
printTime s =
  case stTime s of
    Just (elapsed, total) -> putStrLn $ "Time: \"" ++ show elapsed ++ "/" ++ show total ++ "\""
    Nothing -> putStrLn "Time information not available"

printState s =
  case s of
    Playing -> p "State: Playing"
    Paused -> p "State: Paused"
    Stopped -> p "State: Stopped"
    where
      p = putStrLn
