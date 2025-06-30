import System.Directory
import System.FilePath
import System.IO
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad ( filterM )
import Data.List ( partition )

newtype Path = Path { getPath :: FilePath } deriving (Show)

newtype Prefix = Prefix { getPrefix :: String } deriving (Show)

main :: IO ()
main = do
  currentDir <- Path <$> getCurrentDirectory
  actions <- processDirectory currentDir
  if null actions
    then putStrLn "No files to rename."
    else do
      putStrLn "Will rename:"
      mapM_ (\(old, new) -> putStrLn ("  " ++ getPath old ++ " -> " ++ getPath new)) actions
      putStr "Proceed? (y/n): "
      hFlush stdout
      answer <- getLine
      if answer == "y"
        then do
          mapM_ (\(old, new) -> renameFile (getPath old) (getPath new)) actions
          putStrLn "Files renamed."
        else
          putStrLn "Operation cancelled."


processDirectory :: Path -> IO [(Path, Path)]
processDirectory path = do
  entries <- map Path <$> listDirectory (getPath path)
  let
    fullPaths :: [Path]
    fullPaths = [ Path (getPath path </> p) | Path p <- entries ]

  files <- filterM (doesFileExist . getPath) fullPaths
  dirs  <- filterM (doesDirectoryExist . getPath) fullPaths

  let dirName = Prefix (takeFileName (getPath path))
  let (renamed,toRename) = partition (isRenamed (dirName)) files
  let usedNumbers = getUsedNumbers renamed (dirName)
  let startNumber = if null usedNumbers then 0 else maximum usedNumbers + 1
  let renamesHere = makeNewNames toRename dirName startNumber

  subRenames <- mapM processDirectory dirs
  return (renamesHere ++ concat subRenames)


isRenamed :: Prefix -> Path -> Bool
isRenamed prefix file =
  let name = dropExtension (takeFileName (getPath file))
      expectedStart = getPrefix prefix ++ "_"
  in case stripPrefix expectedStart name of
       Just rest -> all isDigit rest
       Nothing -> False

getUsedNumbers :: [Path] -> Prefix -> [Int]
getUsedNumbers [] _ = []
getUsedNumbers (f:fs) prefix =
  let name = dropExtension (takeFileName (getPath f))
      prefixWithUnderscore = getPrefix prefix ++ "_"
  in case stripPrefix prefixWithUnderscore name of
       Just numStr -> case readMaybe numStr of
                        Just n  -> n : getUsedNumbers fs prefix
                        Nothing -> getUsedNumbers fs prefix
       Nothing -> getUsedNumbers fs prefix


makeNewNames :: [Path] -> Prefix -> Int -> [(Path, Path)]
makeNewNames [] _ _ = []
makeNewNames (f:fs) prefix n =
  let ext = takeExtension (getPath f)
      dir = takeDirectory (getPath f)
      newName = getPrefix prefix ++ "_" ++ show n ++ ext
      fullNew = Path (dir </> newName)
  in (f, fullNew) : makeNewNames fs prefix (n + 1)


stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
  | x == y    = stripPrefix xs ys
  | otherwise = Nothing
stripPrefix _ _ = Nothing
