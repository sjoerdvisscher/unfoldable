-- From https://byorgey.wordpress.com/2016/10/25/adventures-in-enumerating-balanced-brackets/
import Data.Unfolder
import Data.MemoTrie (memo2)
import Control.Applicative

enumBrackets :: Unfolder f => Int -> f String
enumBrackets n = enumBracketsTail n 0

enumBracketsTail :: Unfolder f => Int -> Int -> f String
enumBracketsTail = enumBracketsTail'
  where
    -- Ensure memoization happens for a specific `f`
    enumBracketsTail' = memo2 enumBracketsTail''
    enumBracketsTail'' 0 c = pure (replicate c ')')
    enumBracketsTail'' n 0 = ('(':) <$> enumBracketsTail' (n-1) 1
    enumBracketsTail'' n c =
      ('(':) <$> enumBracketsTail' (n-1) (c+1)
      <|>
      ((')':) <$> enumBracketsTail' n (c-1))

{-

>>> enumBrackets 3 :: [String]
["((()))","(()())","(())()","()(())","()()()"]
>>> getNth (enumBrackets 40) 16221270422764920820
"((((((((()((())()(()()()())(()))((()()()()(()((()())))((()())))))))()))()())()))"
>>> size (enumBrackets 100)
896519947090131496687170070074100632420837521538745909320

-}
