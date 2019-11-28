module Main exposing (main)

import Browser
import Char
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, readonly, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import ParseSvg exposing (convertSvgToElm)
import Parser
import SvgLogo
import SvgToElm exposing (svgToElm)


type alias Model =
    { elmCode : String
    , svgCode : String
    }


type Msg
    = SvgCodeChange String
    | ConvertSvgToElm


initSvg =
    SvgLogo.svgSrc


init : () -> ( Model, Cmd Msg )
init _ =
    ( { elmCode = toElmCode initSvg
      , svgCode = initSvg
      }
    , Cmd.none
    )


black =
    rgb 0 0 0


textAreaStyle : Style
textAreaStyle =
    Css.batch
        [ resize vertical
        , minHeight (px 200)
        , borderRadius (px 5)
        , border3 (px 6) solid (hex "#fff")
        , padding (px 15)
        , boxShadow5 (px 0) (px 6) (px 0) (px 0) (rgba 0 0 0 0.2)
        , marginBottom (px 30)
        ]


textAreaWrapperStyle : Style
textAreaWrapperStyle =
    Css.batch
        [ width (pct 100)
        , displayFlex
        , flexDirection column
        ]


inputBox attrs children =
    div [ css [ textAreaWrapperStyle ] ]
        (children
            ++ [ textarea (attrs ++ [ css [ textAreaStyle ] ]) []
               ]
        )


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


contents =
    div
        [ css
            [ maxWidth (px 800)
            , width (pct 100)
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
        [ halfLine ]


view : Model -> Browser.Document Msg
view model =
    { title = "Convert SVG to Elm"
    , body =
        List.map toUnstyled
            [ header
            , wrapper
                [ contents
                    [ inputBox [ value model.svgCode, onInput SvgCodeChange ]
                        [ h2 [] [ text "svg" ] ]
                    , inputBox [ value model.elmCode, readonly True ]
                        [ h2 [] [ text "elm" ] ]
                    ]
                ]
            , githubLink
            ]
    }


header =
    div [ css [ displayFlex, justifyContent "center", alignItems center, marginTop (px 30) ] ]
        [ div
            [ css
                [ backgroundColor (hex "#1293D8")
                , width (px 180)
                , padding2 (px 10) (px 30)
                , displayFlex
                , justifyContent "center"
                , alignItems center
                , border3 (px 3) solid (hex "#fff")
                ]
            ]
            [ h1 [ css [ displayFlex, alignItems center, justifyContent "space-evenly", width (pct 100) ] ]
                [ i [] [ text "svg" ]
                , text " ➜ "
                , span [ css [ fontWeight lighter ] ] [ text "elm" ]
                ]
            ]
        ]


githubLink =
    div
        [ css [ displayFlex, justifyContent "center" ]
        ]
        [ a [ href "https://github.com/JEphron/svg-to-elm" ]
            [ img
                [ css [ width (px 30) ]
                , src "assets/github.png"
                ]
                []
            ]
        ]


toElmCode content =
    case convertSvgToElm content of
        Err deadEnds ->
            "parse error: \n"
                ++ workingDeadEndsToString deadEnds

        Ok result ->
            svgToElm result


workingDeadEndsToString : List Parser.DeadEnd -> String
workingDeadEndsToString deadEnds =
    List.map deadEndToString deadEnds |> String.join "\n"


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    problemToString deadEnd.problem
        ++ " (row: "
        ++ String.fromInt deadEnd.row
        ++ ", col: "
        ++ String.fromInt deadEnd.col
        ++ ")"


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting string ->
            "expecting " ++ string

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol string ->
            "expecting symbol \"" ++ string ++ "\""

        Parser.ExpectingKeyword string ->
            "expecting keyword \"" ++ string ++ "\""

        Parser.ExpectingEnd ->
            "unexpected end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem string ->
            string

        Parser.BadRepeat ->
            "bad repeat"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SvgCodeChange content ->
            ( { model
                | elmCode = toElmCode content
                , svgCode = content
              }
            , Cmd.none
            )

        ConvertSvgToElm ->
            ( { model
                | elmCode = toElmCode model.svgCode
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
