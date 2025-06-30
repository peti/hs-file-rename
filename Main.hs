import System.Directory
import System.FilePath
import System.IO
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad (filterM)

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  actions <- processDirectory currentDir
  if null actions
    then putStrLn "No files to rename."
    else do
      putStrLn "Will rename:"
      mapM_ (\(old, new) -> putStrLn ("  " ++ old ++ " -> " ++ new)) actions
      putStr "Proceed? (y/n): "
      hFlush stdout
      answer <- getLine
      if answer == "y"
        then do
          mapM_ (\(old, new) -> renameFile old new) actions
          putStrLn "Files renamed."
        else
          putStrLn "Operation cancelled."


processDirectory :: FilePath -> IO [(FilePath, FilePath)]
processDirectory path = do
  entries <- listDirectory path
  let fullPaths = map (path </>) entries

  files <- filterM doesFileExist fullPaths
  dirs  <- filterM doesDirectoryExist fullPaths

  let dirName = takeFileName path
  let renamed = getRenamed files dirName
  let usedNumbers = getUsedNumbers renamed dirName
  let startNumber = if null usedNumbers then 0 else maximum usedNumbers + 1

  let toRename = getNonRenamed files dirName
  let renamesHere = makeNewNames toRename dirName startNumber

  subRenames <- mapM processDirectory dirs
  return (renamesHere ++ concat subRenames)


isRenamed :: FilePath -> String -> Bool
isRenamed file prefix =
  let name = dropExtension (takeFileName file)
      expectedStart = prefix ++ "_"
  in case stripPrefix expectedStart name of
       Just rest -> all isDigit rest
       Nothing -> False

getRenamed :: [FilePath] -> String -> [FilePath]
getRenamed files prefix = [f | f <- files, isRenamed f prefix]


getNonRenamed :: [FilePath] -> String -> [FilePath]
getNonRenamed files prefix = [f | f <- files, not (isRenamed f prefix)]


getUsedNumbers :: [FilePath] -> String -> [Int]
getUsedNumbers [] _ = []
getUsedNumbers (f:fs) prefix =
  let name = dropExtension (takeFileName f)
      prefixWithUnderscore = prefix ++ "_"
  in case stripPrefix prefixWithUnderscore name of
       Just numStr -> case readMaybe numStr of
                        Just n  -> n : getUsedNumbers fs prefix
                        Nothing -> getUsedNumbers fs prefix
       Nothing -> getUsedNumbers fs prefix


makeNewNames :: [FilePath] -> String -> Int -> [(FilePath, FilePath)]
makeNewNames [] _ _ = []
makeNewNames (f:fs) prefix n =
  let ext = takeExtension f
      dir = takeDirectory f
      newName = prefix ++ "_" ++ show n ++ ext
      fullNew = dir </> newName
  in (f, fullNew) : makeNewNames fs prefix (n + 1)


stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
  | x == y    = stripPrefix xs ys
  | otherwise = Nothing
stripPrefix _ _ = Nothing