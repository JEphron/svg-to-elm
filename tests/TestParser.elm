module TestParser exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ParseSvg exposing (..)
import Test exposing (..)


expectSvg : String -> SvgAst -> Expectation
expectSvg str expectedAst =
    case convertSvgToElm str of
        Ok parsedAst ->
            Expect.equal expectedAst parsedAst

        Err e ->
            Expect.fail (Debug.toString e)


suite : Test
suite =
    describe "The SVG parser"
        [ test "simple" <|
            \_ ->
                expectSvg "<svg/>" (SvgNode "svg" [] [])
        , test "some spaces ok" <|
            \_ ->
                expectSvg " < svg /> " (SvgNode "svg" [] [])
        , test "tabs ok" <|
            \_ ->
                expectSvg "<svg\t/>" (SvgNode "svg" [] [])
        , test "newline ok" <|
            \_ ->
                expectSvg "<svg\n/>" (SvgNode "svg" [] [])
        , test "1 attr" <|
            \_ ->
                expectSvg """<svg foo="bar"/>"""
                    (SvgNode "svg" [ ( "foo", "bar" ) ] [])
        , test "2 attrs" <|
            \_ ->
                expectSvg """<svg foo="bar" baz="quux"/>"""
                    (SvgNode "svg"
                        [ ( "foo", "bar" )
                        , ( "baz", "quux" )
                        ]
                        []
                    )
        , test "weird attrs" <|
            \_ ->
                expectSvg """<svg fo_o:ba-r="baz!"/>"""
                    (SvgNode "svg"
                        [ ( "fo_o:ba-r", "baz!" )
                        ]
                        []
                    )
        , test "closing tag" <|
            \_ ->
                expectSvg "<svg></svg>"
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "textnode" <|
            \_ ->
                expectSvg "<svg>hello!</svg>"
                    (SvgNode "svg"
                        []
                        [ TextNode "hello!" ]
                    )
        , test "textnode strips leading space" <|
            \_ ->
                expectSvg "<svg>    hello!</svg>"
                    (SvgNode "svg"
                        []
                        [ TextNode "hello!" ]
                    )
        , test "child node" <|
            \_ ->
                expectSvg "<svg><bub/></svg>"
                    (SvgNode "svg"
                        []
                        [ SvgNode "bub" [] []
                        ]
                    )
        , test "multiple children" <|
            \_ ->
                expectSvg "<svg><bub/><beeb/></svg>"
                    (SvgNode "svg"
                        []
                        [ SvgNode "bub" [] []
                        , SvgNode "beeb" [] []
                        ]
                    )
        , test "whitespace ignored" <|
            \_ ->
                expectSvg "<svg> \n \t <bub/> \n \t <beeb/> \n \t </svg>"
                    (SvgNode "svg"
                        []
                        [ SvgNode "bub" [] []
                        , SvgNode "beeb" [] []
                        ]
                    )
        , test "textnodes and children coexist" <|
            \_ ->
                expectSvg "<svg> Mr. <bub>bubub</bub> sez <beeb>bebeb</beeb>!</svg>"
                    (SvgNode "svg"
                        []
                        [ TextNode "Mr. "
                        , SvgNode "bub" [] [ TextNode "bubub" ]
                        , TextNode "sez "
                        , SvgNode "beeb" [] [ TextNode "bebeb" ]
                        , TextNode "!"
                        ]
                    )
        , test "simple comment" <|
            \_ ->
                expectSvg "<svg> <!-- hello! --> </svg>"
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "comment surrounded by tags" <|
            \_ ->
                expectSvg "<svg> <div/> <!-- hello! --> <div/> </svg>"
                    (SvgNode "svg"
                        []
                        [ SvgNode "div" [] [], SvgNode "div" [] [] ]
                    )
        , test "comment at the start" <|
            \_ ->
                expectSvg "<!-- hello! --> <svg></svg>"
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "comment at the end" <|
            \_ ->
                expectSvg "<svg></svg> <!-- hello! -->"
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "commented out tag" <|
            \_ ->
                expectSvg "<svg> <!-- <yolo/> --> </svg>"
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "comment start out of context" <|
            \_ ->
                expectSvg """<svg foo="<!--">  </svg>"""
                    (SvgNode "svg"
                        [ ( "foo", "<!--" ) ]
                        []
                    )

        -- TODO:
        -- , test "comment surrounded by text" <|
        --     \_ ->
        --         expectSvg "<svg> the fruit<!-- <yolo/> -->is round </svg>"
        --             (SvgNode "svg"
        --                 []
        --                 [ TextNode "the fruitis round" ]
        --             )
        -- TODO:
        -- , test "ignores <!DOCTYPE..."
        , test "ignores declaration at the beginning" <|
            \_ ->
                expectSvg """<?xml version="1.0" encoding="utf-8"?><svg>  </svg>"""
                    (SvgNode "svg"
                        []
                        []
                    )
        , test "part of a real SVG" <|
            \_ ->
                expectSvg """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg width="104px" height="36px" viewBox="0 0 104 36" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sketch="http://www.bohemiancoding.com/sketch/ns">
    <title>logo</title>
    <description>Created with Sketch (http://www.bohemiancoding.com/sketch)</description>
    <defs></defs>
    <g id="Page-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" sketch:type="MSPage">
        <g id="logo" sketch:type="MSArtboardGroup" transform="translate(-8.000000, -5.000000)" fill="#FFFFFF">
            <path d="M20.2602817,5 L25.9509859,5 L25.9509859,31.0824248 C23.0360563,31.6338042" id="Shape" sketch:type="MSShapeGroup"></path>
        </g>
    </g>
</svg>
                          """
                    (SvgNode "svg"
                        [ ( "width", "104px" )
                        , ( "height", "36px" )
                        , ( "viewBox", "0 0 104 36" )
                        , ( "version", "1.1" )
                        , ( "xmlns", "http://www.w3.org/2000/svg" )
                        , ( "xmlns:xlink", "http://www.w3.org/1999/xlink" )
                        , ( "xmlns:sketch", "http://www.bohemiancoding.com/sketch/ns" )
                        ]
                        [ SvgNode "title" [] [ TextNode "logo" ]
                        , SvgNode "description"
                            []
                            [ TextNode "Created with Sketch (http://www.bohemiancoding.com/sketch)"
                            ]
                        , SvgNode "defs" [] []
                        , SvgNode "g"
                            [ ( "id", "Page-1" )
                            , ( "stroke", "none" )
                            , ( "stroke-width", "1" )
                            , ( "fill", "none" )
                            , ( "fill-rule", "evenodd" )
                            , ( "sketch:type", "MSPage" )
                            ]
                            [ SvgNode "g"
                                [ ( "id", "logo" )
                                , ( "sketch:type", "MSArtboardGroup" )
                                , ( "transform", "translate(-8.000000, -5.000000)" )
                                , ( "fill", "#FFFFFF" )
                                ]
                                [ SvgNode "path"
                                    [ ( "d", "M20.2602817,5 L25.9509859,5 L25.9509859,31.0824248 C23.0360563,31.6338042" )
                                    , ( "id", "Shape" )
                                    , ( "sketch:type", "MSShapeGroup" )
                                    ]
                                    []
                                ]
                            ]
                        ]
                    )
        ]
