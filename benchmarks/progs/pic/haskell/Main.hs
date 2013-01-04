-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain
import System.IO(hPutStr,stderr)
import System.Environment(getArgs,getProgName)

usage = do
  progname <- getProgName
  putStrLn ("usage:  " ++ progname ++ " num")


main = do
    cmdline <- getArgs
    case cmdline of
      [numstr] -> do
        let (nPart, rest) = (head (reads numstr)) :: (Int, String)
        putStrLn (pic nPart)
      _ -> do
        usage

