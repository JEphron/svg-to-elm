module TestOutput exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ParseSvg exposing (SvgAst(..))
import SvgToElm exposing (..)
import Test exposing (..)


equal : SvgAst -> String -> Expectation
equal astInput elmCodeOutput =
    Expect.equal (astToElm astInput) elmCodeOutput


suite : Test
suite =
    describe "svgToElm"
        [ test "simple" <|
            \_ ->
                equal (SvgNode "svg" [] []) "svg [] []"
        , test "attrs" <|
            \_ ->
                equal
                    (SvgNode "svg"
                        [ ( "id", "bar" )
                        , ( "class", "quux" )
                        ]
                        []
                    )
                    """svg [id "bar", class "quux"] []"""
        , test "empty attr" <|
            \_ ->
                equal
                    (SvgNode "svg" [ ( "class", "" ) ] [])
                    """svg [class ""] []"""
        , test "children" <|
            \_ ->
                equal
                    (SvgNode "svg"
                        []
                        [ SvgNode "g" [ ( "id", "bar" ) ] []
                        , SvgNode "g" [ ( "class", "quux" ) ] []
                        ]
                    )
                    """svg [] [g [id "bar"] [], g [class "quux"] []]"""
        , test "textnodes all around" <|
            \_ ->
                equal
                    (SvgNode "svg"
                        []
                        [ TextNode "aaa"
                        , SvgNode "g" [ ( "id", "bar" ) ] [ TextNode "bbb" ]
                        , TextNode "ccc"
                        , SvgNode "g" [ ( "id", "quux" ) ] [ TextNode "ddd" ]
                        , TextNode "eee"
                        ]
                    )
                    """svg [] [text "aaa", g [id "bar"] [text "bbb"], text "ccc", g [id "quux"] [text "ddd"], text "eee"]"""
        , test "text node becomes _text" <|
            \_ ->
                equal
                    (SvgNode "text" [] [ TextNode "foo" ])
                    """text_ [] [text "foo"]"""
        , test "kebab-to-camel for attrs (1 kebab)" <|
            \_ ->
                equal
                    (SvgNode "svg" [ ( "glyph-name", "foo" ) ] [])
                    """svg [glyphName "foo"] []"""
        , test "kebab-to-camel for attrs (2 kebab)" <|
            \_ ->
                equal
                    (SvgNode "svg" [ ( "horiz-adv-x", "10px" ) ] [])
                    """svg [horizAdvX "10px"] []"""
        , test "unknown attr discarded" <|
            \_ ->
                equal
                    (SvgNode "svg" [ ( "not-real", "10px" ) ] [])
                    """svg [] []"""
        , test "de-colon-ize attr" <|
            \_ ->
                equal
                    (SvgNode "svg" [ ( "xlink:href", "..." ) ] [])
                    """svg [xlinkHref "..."] []"""
        , test "kebab-to-camel for node names" <|
            \_ ->
                equal
                    (SvgNode "color-profile" [] [])
                    """Svg.colorProfile [] []"""
        , test "ambiguous things gets prefixed" <|
            \_ ->
                equal
                    (SvgNode "path" [ ( "path", "foo" ) ] [])
                    """Svg.path [Attributes.path "foo"] []"""
        , test "attrs with newlines get triple quoted" <|
            \_ ->
                equal
                    (SvgNode "path" [ ( "path", "foo\nbar" ) ] [])
                    """Svg.path [Attributes.path \"\"\"foo
bar\"\"\"] []"""
        , test "textnodes with newlines get triple quoted" <|
            \_ ->
                equal
                    (SvgNode "path" [] [ TextNode "foo\nbar" ])
                    """Svg.path [] [text \"\"\"foo
bar\"\"\"]"""
        , test "unknown node gets output using node function" <|
            \_ ->
                equal
                    (SvgNode "svg" [] [ SvgNode "foo" [] [ TextNode "hello!" ] ])
                    """svg [] [node "foo" [] [text "hello!"]]"""
        ]
