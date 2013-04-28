
import Ex16.Parser
import Ex16.Lexer
import Data.Char

pat = Pat . words

parse_int :: String -> Maybe (String, String)
parse_int = f [] where
    f acc [] = Just (reverse acc, [])
    f acc (c:cs) = if isDigit c
        then f (c:acc) cs
        else Just (reverse acc, c:cs)

parser = build_parser [
    ("_int", TPCustom "0123456789" parse_int)
    ] [
    pat "( _ )" 1.0 ANon (),
    pat "_ + _" 2.0 ALeft (),
    pat "_ - _" 2.0 ALeft (),
    pat "_ * _" 3.0 ALeft (),
    pat "_int" 0.0 ANon ()
    ]

main = do
    let lexed = parse_lex (parser_lexer parser) "(2+3+4*5+(6+7)*8)"
    print $ lexed
    case lexed of
        Left err -> print err
        Right toks -> case parse_lexed parser toks of
            Left err -> print err
            Right pt -> putStrLn (dump_pt pt)
