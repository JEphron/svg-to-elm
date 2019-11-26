module Main exposing (main)

import Browser
import Char
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import Parser exposing (..)


type alias Model =
    { elmCode : String
    , svgCode : String
    }


type Msg
    = SvgCodeChange String
    | ConvertSvgToElm


init : () -> ( Model, Cmd Msg )
init _ =
    ( { elmCode = ""
      , svgCode = ""
      }
    , Cmd.none
    )


black =
    rgb 0 0 0


textAreaStyle : Style
textAreaStyle =
    Css.batch
        [ resize none
        , width (pct 100)
        , minHeight (px 200)
        , borderRadius (px 5)
        , border3 (px 1) solid black
        , padding (px 10)
        ]


codeArea attrs children =
    textarea (attrs ++ [ css [ textAreaStyle ] ]) children


justifyContent =
    Css.property "justify-content"


wrapper =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent "center"
            , alignItems center
            , padding (px 50)
            ]
        ]


split els =
    let
        halfLine =
            div
                [ css
                    [ borderBottom3 (px 1) solid (rgb 200 200 200)
                    , width (pct 100)
                    , margin2 (px 20) (px 20)
                    ]
                ]
                []
    in
    div [ css [ displayFlex, width (pct 100), margin2 (px 20) (px 20) ] ]
        ([ halfLine ] ++ els ++ [ halfLine ])


view : Model -> Browser.Document Msg
view model =
    { title = "SVG -> Elm"
    , body =
        List.map toUnstyled
            [ wrapper
                [ codeArea [ onInput SvgCodeChange ] []
                , split [ button [ onClick ConvertSvgToElm ] [ text "convert" ] ]
                , codeArea [ value model.elmCode ] []
                ]
            ]
    }


type alias Attr =
    ( String, String )


type SvgAst
    = SvgNode String (List Attr) (List SvgAst)


lAngleP =
    symbol "<"


rAngleP =
    symbol ">"


constP : String -> Parser ()
constP str =
    List.map
        (String.fromChar >> symbol)
        (String.toList str)
        |> List.reverse
        |> List.foldl (|.) (succeed ())


inclusiveChompUntil s =
    chompUntil s |. symbol s


declarationP : Parser ()
declarationP =
    constP "?xml" |. inclusiveChompUntil "?>"


attrNameP : Parser String
attrNameP =
    let
        isNameChar c =
            Char.isAlphaNum c || c == ':'
    in
    getChompedString <|
        succeed ()
            |. chompWhile isNameChar


quoteP =
    symbol "\""


quotedStringP : Parser String
quotedStringP =
    let
        isInnerChar c =
            c /= '"'
    in
    getChompedString <|
        succeed ()
            |. quoteP
            |. chompWhile isInnerChar
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
                    |. spaces
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] helper


svgChildrenP : Parser (List SvgAst)
svgChildrenP =
    let
        helper : LoopHelper SvgAst
        helper revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= svgP
                    |. spaces
                , succeed ()
                    |> Parser.map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] helper


svgP : Parser SvgAst
svgP =
    succeed SvgNode
        |. spaces
        |. symbol "<"
        |. oneOf [ declarationP, succeed () ]
        |. spaces
        |. oneOf [ symbol "<", succeed () ]
        |= attrNameP
        |. spaces
        |= attrsP
        |. symbol ">"
        |= succeed []


convertSvgToElm : String -> Result (List DeadEnd) SvgAst
convertSvgToElm svg =
    run svgP svg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SvgCodeChange content ->
            ( { model | svgCode = content }, Cmd.none )

        ConvertSvgToElm ->
            ( { model
                | elmCode =
                    case convertSvgToElm model.svgCode of
                        Err deadEnds ->
                            Debug.toString deadEnds

                        Ok result ->
                            Debug.toString result
              }
            , Cmd.none
            )



-- MAIN --


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
