{-# OPTIONS_GHC -Wall #-}
module Todo.Cli (run) where

import Data.List (sortBy, isInfixOf)
import Data.Char (toLower, isUpper)
import Data.Ord (comparing)
import System.IO
import qualified System.IO.Strict as S
import Options.Applicative
import Todo

type TaskArg = Task
type RepoFilePath = String

data Command = CmdAdd TaskArg RepoFilePath
             | CmdList RepoFilePath
             | CmdQuery RepoFilePath [QueryArg] (Maybe SortArg)

data QueryArg = QueryTodo                           -- [-C | --no-complete] means not complete
              | QueryDone                           -- [-c | --complete] means complete
              | QueryMinPriority (Maybe Priority)   -- [-m MIN | --min=MIN] no priority is lowest priority
              | QueryMaxPriority (Maybe Priority)   -- [-M MAX | --max=MAX] no priority is lowest priority
              | QueryContentLike Content            -- [ --like=TEXT] find tasks with TEXT in their content
              deriving Show

data SortArg = SortDateAsc      -- [ --sort-date-asc ]
             | SortDateDesc     -- [ --sort-date-desc ]
             | SortPriorityAsc  -- [ --sort-priority-asc ]
             | SortPriorityDesc -- [ --sort-priority-desc ]
             deriving Show

run :: IO ()
run = do
    cmd <- execParser $ info (helper <*> parseCommand) idm
    case cmd of
        CmdList p -> do
            repo <- withFile p ReadMode S.hGetContents
            let ts = readTasks repo
            putStr $ showTasks ts
        CmdAdd t p -> do
            oldRepo <- withFile p ReadMode S.hGetContents
            let newRepo = t : readTasks oldRepo
            withFile p WriteMode $ flip hPutStr (showTasks newRepo)
        CmdQuery p qs s -> do
            allTasks <- withFile p ReadMode (fmap readTasks . S.hGetContents)
            putStr . showTasks . sort s . foldr query allTasks $ qs

query :: QueryArg -> [Task] -> [Task]
query q = filter $ case q of
        QueryTodo -> not . isComplete
        QueryDone -> isComplete
        QueryMinPriority (Just m) -> minPriority m
        QueryMinPriority _ -> const True
        QueryMaxPriority (Just m) -> maxPriority m
        QueryMaxPriority _ -> const True
        QueryContentLike s -> isInfixOf s' . lower . content where
            s' = lower s
            lower = map toLower
            -- let's avoid universal pattern matching so if we add
            -- a new QueryArg constructor option the compiler can
            -- warn us about the non-exhaustive pattern matching

sort :: Maybe SortArg -> [Task] -> [Task]
sort (Just s) = sortBy $ case s of
        SortDateAsc -> asc maybeDate
        SortDateDesc -> desc maybeDate
        SortPriorityAsc -> asc priority
        SortPriorityDesc -> desc priority
 where
    asc :: Ord b => (a -> b) -> a -> a -> Ordering
    asc = comparing
    desc :: Ord b => (a -> b) -> a -> a -> Ordering
    desc f = flip $ comparing f
sort _ = id

parseCommand :: Parser Command
parseCommand = subparser $ cmdAdd <> cmdList <> cmdQuery

cmdAdd :: Mod CommandFields Command
cmdAdd = command "add" $ info (CmdAdd <$> fmap readTask taskInput <*> pathArgument)
    (briefDesc <> progDesc "Add TASK to REPO")
  where
    taskInput = strArgument $ metavar "TASK"

cmdList :: Mod CommandFields Command
cmdList = command "list" $ info (CmdList <$> pathArgument)
    (briefDesc <> progDesc "List all tasks from REPO")

pathArgument :: Parser String
pathArgument = strArgument (metavar "PATH")

cmdQuery :: Mod CommandFields Command
cmdQuery = command "query" $ info (CmdQuery <$> pathArgument <*> queryArgs <*> sortArg)
    (briefDesc <> progDesc "Query REPO according to specified filters")

queryArgs :: Parser [QueryArg]
queryArgs = many $
    (qDone <|> qTodo)
    <|> qMinPriority
    <|> qMaxPriority
    <|> qContentLike
  where
    qDone = flag' QueryDone (short 'c' <> long "complete" <> help "Complete tasks only")
    qTodo = flag' QueryTodo (short 'C' <> long "no-complete" <> help "Incomplete tasks only")
    qMinPriority = QueryMinPriority <$> bindPriority <$> priorityOption (short 'm' <> long "min-priority" <> metavar "MIN" <> help "tasks with priority at least MIN")
    qMaxPriority = QueryMaxPriority <$> bindPriority <$> priorityOption (short 'M' <> long "max-priority" <> metavar "MAX" <> help "tasks with priority at most MAX")
    qContentLike = QueryContentLike <$> strOption (short 'l' <> long "like" <> metavar "TEXT" <> help "task with content like TEXT")
    bindPriority :: Maybe Char -> Maybe Priority
    bindPriority mc = mc >>= mkPriority

priorityOption ::  Mod OptionFields (Maybe Char) -> Parser (Maybe Char)
priorityOption = option char where
    char [c] | isUpper c = return $ Just c
    char _ = fail "Invalid option (single uppercase character required)"

sortArg :: Parser (Maybe SortArg)
sortArg = optional $ dateAsc <|> dateDesc <|> priorityAsc <|> priorityDesc
  where
    dateAsc = build SortDateAsc "date" "asc"
    dateDesc = build SortDateDesc "date" "desc"
    priorityAsc = build SortPriorityAsc "priority" "asc"
    priorityDesc = build SortPriorityDesc "priority" "desc"
    build constr param order = flag' constr (long ("sort-" ++ param ++ "-" ++ order) <> help ("Sort tasks by " ++ param ++ " in " ++ order ++ "ending order"))
