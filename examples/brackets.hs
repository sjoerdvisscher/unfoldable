-- From https://byorgey.wordpress.com/2016/10/25/adventures-in-enumerating-balanced-brackets/
import Data.Unfolder
import Data.MemoTrie (memo)
import Control.Applicative

enumBrackets :: Unfolder f => f String
enumBrackets = enumBracketsTail 0

enumBracketsTail :: Unfolder f => Int -> f String
enumBracketsTail = enumBracketsTail'
  where
    -- Ensure memoization happens for a specific `f`
    enumBracketsTail' = memo enumBracketsTail''
    enumBracketsTail'' 0 = pure "" <|> choose [('(':) <$> enumBracketsTail' 1]
    enumBracketsTail'' c =
      choose [('(':) <$> enumBracketsTail' (c+1)]
      <|>
      ((')':) <$> enumBracketsTail' (c-1))


{-

>>> enumBrackets 3 :: [String]
["((()))","(()())","(())()","()(())","()()()"]
>>> getNth (enumBrackets 40) 16221270422764920820
"((((((((()((())()(()()()())(()))((()()()()(()((()())))((()())))))))()))()())()))"
>>> size (enumBrackets 100)
896519947090131496687170070074100632420837521538745909320

-}
