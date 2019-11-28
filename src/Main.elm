module Main exposing (main)

import Browser
import Char
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, readonly, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import ParseSvg exposing (convertSvgToElm)
import Parser
import SvgToElm exposing (svgToElm)


type alias Model =
    { elmCode : String
    , svgCode : String
    }


type Msg
    = SvgCodeChange String
    | ConvertSvgToElm


initSvg =
    """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg width="104px" height="36px" viewBox="0 0 104 36" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sketch="http://www.bohemiancoding.com/sketch/ns">
    <title>logo-django</title>
    <description>Created with Sketch (http://www.bohemiancoding.com/sketch)</description>
    <defs></defs>
    <g id="Page-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" sketch:type="MSPage">
        <g id="logo-django" sketch:type="MSArtboardGroup" transform="translate(-8.000000, -5.000000)" fill="#FFFFFF">
            <path d="M20.2602817,5 L25.9509859,5 L25.9509859,31.0824248 C23.0360563,31.6338042 20.8901408,31.8507285 18.5684507,31.8507285 C11.6180282,31.8434735 8,28.7383366 8,22.7747325 C8,17.0287781 11.8377465,13.2997118 17.7847887,13.2997118 C18.7076056,13.2997118 19.4107042,13.3722617 20.2602817,13.5899115 L20.2602817,5 L20.2602817,5 Z M20.2602817,18.1242821 C19.5938028,17.9066323 19.044507,17.8340823 18.3414085,17.8340823 C15.4630986,17.8340823 13.8005634,19.5897906 13.8005634,22.6666331 C13.8005634,25.6622196 15.3898592,27.316358 18.3047887,27.316358 C18.9346479,27.316358 19.4473239,27.2808085 20.2602817,27.1719836 L20.2602817,18.1242821 L20.2602817,18.1242821 Z M34.9960563,13.6987364 L34.9960563,26.7577235 C34.9960563,31.2550936 34.6591549,33.417807 33.6704225,35.2823401 C32.7476056,37.0750489 31.531831,38.2053768 29.0197183,39.453961 L23.7391549,36.9654985 C26.2512676,35.7981701 27.4670423,34.7665101 28.2433803,33.1921767 C29.056338,31.5822938 29.3126761,29.7177606 29.3126761,24.8133855 L29.3126761,13.6987364 L34.9960563,13.6987364 Z M29.3126761,5.02901997 L35.0033803,5.02901997 L35.0033803,10.8112493 L29.3126761,10.8112493 L29.3126761,5.02901997 Z M38.4302535,14.9828702 C40.9430986,13.8148163 43.3453521,13.2997118 45.9673239,13.2997118 C48.8895775,13.2997118 50.8077183,14.0687411 51.6580282,15.5705246 C52.1340845,16.4121037 52.2878873,17.5076077 52.2878873,19.8509704 L52.2878873,31.2993491 C49.7398873,31.6620987 46.5239437,31.922553 44.1649014,31.922553 C39.3970141,31.922553 37.2584225,30.2756696 37.2584225,26.6198787 C37.2584225,22.6659076 40.1008451,20.8376494 47.079831,20.2565245 L47.079831,19.0159207 C47.079831,17.9929667 46.559831,17.6229621 45.124338,17.6229621 C43.0223662,17.6229621 40.6567324,18.2106165 38.4375775,19.3423954 L38.4302535,14.9828702 Z M47.336169,23.9420608 C43.571662,24.3048105 42.3485634,24.8931904 42.3485634,26.3579734 C42.3485634,27.4549284 43.051662,27.9693073 44.604338,27.9693073 C45.4539155,27.9693073 46.2302535,27.8967574 47.3354366,27.7153826 L47.3354366,23.9420608 L47.336169,23.9420608 Z M55.056338,14.5765906 C58.4180282,13.6987364 61.1857465,13.2997118 63.9908169,13.2997118 C66.9057465,13.2997118 69.0157746,13.9599162 70.2674366,15.2367949 C71.4458592,16.4411237 71.8208451,17.7615324 71.8208451,20.5764696 L71.8208451,31.6258237 L66.1294085,31.6258237 L66.1294085,20.8013744 C66.1294085,18.6393866 65.3896901,17.8340823 63.3616901,17.8340823 C62.5846197,17.8340823 61.8822535,17.9066323 60.7397183,18.2403619 L60.7397183,31.6265492 L55.056338,31.6265492 L55.056338,14.5765906 Z M74.0326761,34.7012152 C76.0240563,35.7241692 78.0169014,36.1964692 80.1261972,36.1964692 C83.8540845,36.1964692 85.4433803,34.6946857 85.4433803,31.1107193 L85.4433803,31.0018944 C84.3374648,31.5460188 83.223493,31.7716491 81.7513803,31.7716491 C76.764507,31.7716491 73.5932394,28.5141573 73.5932394,23.3558574 C73.5932394,16.9496987 78.2878873,13.3294573 86.5932394,13.3294573 C89.0321127,13.3294573 91.2878873,13.583382 94.0189859,14.1347615 L92.0708169,18.1975575 C90.5562254,17.9073578 91.9463099,18.1540275 90.804507,18.0452026 L90.804507,18.6328571 L90.8777465,21.0124947 L90.9136338,24.0886117 C90.9509859,24.8583664 90.9509859,25.6259447 90.988338,26.3956994 L90.988338,27.9330324 C90.988338,32.7648576 90.5774648,35.0291409 89.3616901,36.900929 C87.5892958,39.6425908 84.5212958,41 80.1620845,41 C77.943662,41 76.0240563,40.6727998 74.0326761,39.9030451 L74.0326761,34.7012152 L74.0326761,34.7012152 Z M85.3335211,17.8703573 L85.1504225,17.8703573 L84.7395493,17.8703573 C83.6336338,17.8340823 82.3380282,18.1242821 81.4510986,18.6756615 C80.0895775,19.4446908 79.3872113,20.8376494 79.3872113,22.8110074 C79.3872113,25.6252192 80.7934085,27.2365531 83.3055211,27.2365531 C84.0811268,27.2365531 84.7117183,27.0921787 85.4441127,26.8738034 L85.4441127,26.4667983 L85.4441127,24.9294653 C85.4441127,24.269261 85.4067606,23.5365067 85.4067606,22.7674775 L85.3708732,20.17019 L85.3335211,18.3056569 L85.3335211,17.8703573 Z M102.84507,13.2271619 C108.528451,13.2271619 112,16.7748534 112,22.5208077 C112,28.4118619 108.382704,32.1039278 102.617296,32.1039278 C96.9265915,32.1039278 93.4176901,28.5569618 93.4176901,22.8480079 C93.4272113,16.9199532 97.044507,13.2271619 102.84507,13.2271619 Z M102.727887,27.5623023 C104.910423,27.5623023 106.199437,25.7710445 106.199437,22.6586526 C106.199437,19.5825356 104.94631,17.7542774 102.765239,17.7542774 C100.509465,17.7542774 99.2189859,19.5462607 99.2189859,22.6586526 C99.2189859,25.7710445 100.516056,27.5623023 102.727887,27.5623023 L102.727887,27.5623023 Z M102.727887,27.5623023" id="Shape" sketch:type="MSShapeGroup"></path>
        </g>
    </g>
</svg>
"""


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
        , boxShadow5 (px 0) (px 6) (px 0) (px 0) (rgba 0 0 0 0.4)
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
            , githubLink
            , wrapper
                [ contents
                    [ inputBox [ value model.svgCode, onInput SvgCodeChange ]
                        [ h2 [] [ text "SVG" ] ]
                    , inputBox [ value model.elmCode, readonly True ]
                        [ h2 [] [ text "Elm" ] ]
                    ]
                ]
            ]
    }


header =
    div [ css [ displayFlex, justifyContent "center", alignItems center, marginTop (px 30) ] ]
        [ div
            [ css
                [ backgroundColor (hex "#1293D8")
                , width (px 240)
                , padding2 (px 10) (px 30)
                , displayFlex
                , justifyContent "center"
                , alignItems center
                , border3 (px 3) solid (hex "#fff")
                ]
            ]
            [ h1 []
                [ b [] [ text "SVG" ]
                , text " ➜ "
                , i [] [ text "Elm" ]
                ]
            ]
        ]


githubLink =
    div
        [ css
            [ position absolute
            , top (px 8)
            , right (px 8)
            ]
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
