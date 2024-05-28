module Log where
import Data.Char


-- tipos de mensagem
data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

-- instante de tempo 
type TimeStamp = Int

-- entrada no ficheiro de log
data LogEntry = LogMessage MessageType TimeStamp String
              | Unknown String
  deriving (Show, Eq)

-- Ã¡rvore binÃ¡ria para mensagens
data MessageTree = Empty
                 | Node LogEntry MessageTree MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogEntry
parseMessage xs | decide == "E "   = LogMessage (Error (read (k !! 1))) (read (k !! 2)) (dropWhile (\n -> not $ isAlpha(n)) less)
                | decide == "I "   = LogMessage (Info) (read ((k !! 1))) (dropWhile (\n -> not $ isAlpha(n)) less)
                | decide == "W "   = LogMessage (Warning) (read ((k !! 1))) (dropWhile (\n -> not $ isAlpha(n)) less)
                | otherwise         = Unknown xs
                where k = words xs
                      less = drop 2 xs
                      decide = take 2 xs

tempo ::LogEntry -> TimeStamp
tempo (LogMessage _ t _) = t

tipo :: LogEntry -> Bool
tipo (Unknown _) = True
tipo _ = False

insert' :: LogEntry -> MessageTree -> MessageTree
insert' ent Empty = (Node ent Empty Empty)
insert' ent (Node val esq dir) | atual <= novo      = Node val esq (insert' ent dir)
                               | atual > novo       = Node val (insert' ent esq) dir
                              where atual = tempo val
                                    novo = tempo ent

insert :: LogEntry -> MessageTree -> MessageTree
insert ent xs = if tipo ent then xs else insert' ent xs                     


build :: [LogEntry] -> MessageTree
build xs = foldr insert Empty xs

inOrder :: MessageTree -> [LogEntry]
inOrder Empty = []
inOrder (Node val esq dir) = inOrder esq ++ [val] ++ inOrder dir