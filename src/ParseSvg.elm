module ParseSvg exposing (Attr, SvgAst(..), convertSvgToElm)

import Char
import Parser exposing (..)


type alias Attr =
    ( String, String )


type SvgAst
    = SvgNode String (List Attr) (List SvgAst)
    | TextNode String


convertSvgToElm : String -> Result (List DeadEnd) SvgAst
convertSvgToElm svg =
    run svgDocumentP svg


svgDocumentP : Parser SvgAst
svgDocumentP =
    succeed identity
        |. whitespaceP
        |. oneOf [ backtrackable declarationP, succeed () ]
        |. oneOf [ backtrackable doctypeP, succeed () ]
        |. whitespaceP
        |= svgP


declarationP : Parser ()
declarationP =
    symbol "<?xml" |. inclusiveChompUntil "?>"


doctypeP : Parser ()
doctypeP =
    symbol "<!DOCTYPE" |. inclusiveChompUntil ">"


svgP : Parser SvgAst
svgP =
    let
        children attrName =
            oneOf
                [ backtrackable noChildrenP
                , childrenP attrName
                ]

        attrsAndChildren attrName =
            succeed (SvgNode attrName)
                |. whitespaceP
                |= attrsP
                |. whitespaceP
                |= children attrName

        name =
            succeed identity
                |= attrNameP
                |> Parser.andThen attrsAndChildren
    in
    succeed identity
        |. oneOf [ backtrackable commentP, succeed () ]
        |. spaces
        |. symbol "<"
        |. whitespaceP
        |= oneOf
            [ symbol "/" |> fail "Mismatched closing tag?"
            , name
            ]


childrenP : String -> Parser (List SvgAst)
childrenP attrName =
    succeed (++)
        |. symbol ">"
        |. whitespaceP
        |. oneOf [ backtrackable commentP, succeed () ]
        |. whitespaceP
        |= textChildrenP
        |. whitespaceP
        |= svgChildrenP
        |. whitespaceP
        |. oneOf [ backtrackable commentP, succeed () ]
        |. whitespaceP
        |. backtrackable (closingTagP attrName)


noChildrenP : Parser (List a)
noChildrenP =
    succeed [] |. symbol "/>"


attrNameP : Parser String
attrNameP =
    let
        isNameChar c =
            Char.isAlphaNum c || c == ':' || c == '-' || c == '_'
    in
    getChompedString <|
        succeed ()
            |. chompWhile isNameChar


quotedStringP : Parser String
quotedStringP =
    let
        isInnerChar c =
            c /= '"'
    in
    succeed identity
        |. quoteP
        |= ((succeed () |. chompWhile isInnerChar) |> getChompedString)
        |. quoteP


attrP : Parser Attr
attrP =
    succeed Tuple.pair
        |= attrNameP
        |. symbol "="
        |= quotedStringP


type alias LoopHelper a =
    List a -> Parser (Step (List a) (List a))


attrsP : Parser (List Attr)
attrsP =
    let
        helper : LoopHelper Attr
        helper revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= attrP
                    |. whitespaceP
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] helper


svgChildrenP : Parser (List SvgAst)
svgChildrenP =
    let
        appendHelper revSvgs svg txtNode =
            Loop (txtNode ++ (svg :: revSvgs))

        helper : LoopHelper SvgAst
        helper revStmts =
            oneOf
                [ backtrackable
                    (succeed (appendHelper revStmts)
                        |= svgP
                        |. whitespaceP
                        |= oneOf [ textChildrenP, succeed [] ]
                    )
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] helper


textChildrenP : Parser (List SvgAst)
textChildrenP =
    -- TODO: the way we strip whitespace from the front of TextNodes is weird
    succeed
        (\string ->
            if String.isEmpty string then
                []

            else
                [ TextNode string ]
        )
        |= textContentP


textContentP : Parser String
textContentP =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> c /= '>' && c /= '<')


commentP : Parser ()
commentP =
    succeed ()
        |. symbol "<!--"
        |. inclusiveChompUntil "-->"


closingTagP : String -> Parser ()
closingTagP attrName =
    symbol ("</" ++ attrName ++ ">")


inclusiveChompUntil s =
    chompUntil s |. symbol s


quoteP =
    symbol "\""


fail : String -> Parser a -> Parser b
fail why =
    Parser.andThen (always (problem why))


whitespaceP : Parser ()
whitespaceP =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
