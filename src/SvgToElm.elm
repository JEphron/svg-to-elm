module SvgToElm exposing (astToElm, svgToElm)

import ParseSvg exposing (SvgAst(..))
import Set exposing (Set)


svgToElm : SvgAst -> String
svgToElm node =
    moduleHeader ++ astToElm node


moduleHeader =
    """module MySvg exposing (view)
import Svg exposing (..)
import Svg.Attributes as Attributes exposing (..)

view = """


astToElm : SvgAst -> String
astToElm node =
    case node of
        SvgNode name attrs children ->
            nodeNameToElm name
                ++ " ["
                ++ attrsToStr attrs
                ++ "] ["
                ++ childrenToString children
                ++ "]"

        TextNode text ->
            "text " ++ quote text


nodeNameToElm : String -> String
nodeNameToElm name =
    case name of
        "text" ->
            "text_"

        other ->
            if Set.member (kebabToCamel name) knownNodes then
                disambiguate (kebabToCamel name) "Svg"

            else
                "node " ++ quote name


childrenToString : List SvgAst -> String
childrenToString children =
    List.map astToElm children
        |> String.join ", "


attrsToStr : List ( String, String ) -> String
attrsToStr attrs =
    List.filterMap attrToString attrs
        |> String.join ", "


attrToString : ( String, String ) -> Maybe String
attrToString ( name, value ) =
    let
        camelized =
            kebabToCamel name
    in
    if Set.member camelized knownAttrs then
        Just (disambiguate camelized "Attributes" ++ " " ++ quote value)

    else
        Nothing


quote : String -> String
quote value =
    if String.contains "\n" value then
        "\"\"\"" ++ value ++ "\"\"\""

    else
        "\"" ++ value ++ "\""


disambiguate : String -> String -> String
disambiguate name prefix =
    if Set.member name ambiguousNames then
        prefix ++ "." ++ name

    else
        name


kebabToCamel : String -> String
kebabToCamel name =
    let
        preparedName =
            name |> String.replace ":" "-" |> String.split "-"
    in
    case preparedName of
        head :: tail ->
            head ++ (List.map capitalize tail |> String.join "")

        [] ->
            name


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Just ( char, rest ) ->
            String.cons (Char.toUpper char) rest

        Nothing ->
            str


ambiguousNames : Set String
ambiguousNames =
    Set.intersect knownAttrs knownNodes


knownAttrs : Set String
knownAttrs =
    Set.fromList
        [ "accelerate"
        , "accentHeight"
        , "accumulate"
        , "additive"
        , "alignmentBaseline"
        , "allowReorder"
        , "alphabetic"
        , "amplitude"
        , "arabicForm"
        , "ascent"
        , "attributeName"
        , "attributeType"
        , "autoReverse"
        , "azimuth"
        , "baseFrequency"
        , "baseProfile"
        , "baselineShift"
        , "bbox"
        , "begin"
        , "bias"
        , "by"
        , "calcMode"
        , "capHeight"
        , "class"
        , "clip"
        , "clipPath"
        , "clipPathUnits"
        , "clipRule"
        , "color"
        , "colorInterpolation"
        , "colorInterpolationFilters"
        , "colorProfile"
        , "colorRendering"
        , "contentScriptType"
        , "contentStyleType"
        , "cursor"
        , "cx"
        , "cy"
        , "d"
        , "decelerate"
        , "descent"
        , "diffuseConstant"
        , "direction"
        , "display"
        , "divisor"
        , "dominantBaseline"
        , "dur"
        , "dx"
        , "dy"
        , "edgeMode"
        , "elevation"
        , "enableBackground"
        , "end"
        , "exponent"
        , "externalResourcesRequired"
        , "fill"
        , "fillOpacity"
        , "fillRule"
        , "filter"
        , "filterRes"
        , "filterUnits"
        , "floodColor"
        , "floodOpacity"
        , "fontFamily"
        , "fontSize"
        , "fontSizeAdjust"
        , "fontStretch"
        , "fontStyle"
        , "fontVariant"
        , "fontWeight"
        , "format"
        , "from"
        , "fx"
        , "fy"
        , "g1"
        , "g2"
        , "glyphName"
        , "glyphOrientationHorizontal"
        , "glyphOrientationVertical"
        , "glyphRef"
        , "gradientTransform"
        , "gradientUnits"
        , "hanging"
        , "height"
        , "horizAdvX"
        , "horizOriginX"
        , "horizOriginY"
        , "id"
        , "ideographic"
        , "imageRendering"
        , "in2"
        , "in_"
        , "intercept"
        , "k"
        , "k1"
        , "k2"
        , "k3"
        , "k4"
        , "kernelMatrix"
        , "kernelUnitLength"
        , "kerning"
        , "keyPoints"
        , "keySplines"
        , "keyTimes"
        , "lang"
        , "lengthAdjust"
        , "letterSpacing"
        , "lightingColor"
        , "limitingConeAngle"
        , "local"
        , "markerEnd"
        , "markerHeight"
        , "markerMid"
        , "markerStart"
        , "markerUnits"
        , "markerWidth"
        , "mask"
        , "maskContentUnits"
        , "maskUnits"
        , "mathematical"
        , "max"
        , "media"
        , "method"
        , "min"
        , "mode"
        , "name"
        , "numOctaves"
        , "offset"
        , "opacity"
        , "operator"
        , "order"
        , "orient"
        , "orientation"
        , "origin"
        , "overflow"
        , "overlinePosition"
        , "overlineThickness"
        , "panose1"
        , "path"
        , "pathLength"
        , "patternContentUnits"
        , "patternTransform"
        , "patternUnits"
        , "pointOrder"
        , "pointerEvents"
        , "points"
        , "pointsAtX"
        , "pointsAtY"
        , "pointsAtZ"
        , "preserveAlpha"
        , "preserveAspectRatio"
        , "primitiveUnits"
        , "r"
        , "radius"
        , "refX"
        , "refY"
        , "renderingIntent"
        , "repeatCount"
        , "repeatDur"
        , "requiredExtensions"
        , "requiredFeatures"
        , "restart"
        , "result"
        , "rotate"
        , "rx"
        , "ry"
        , "scale"
        , "seed"
        , "shapeRendering"
        , "slope"
        , "spacing"
        , "specularConstant"
        , "specularExponent"
        , "speed"
        , "spreadMethod"
        , "startOffset"
        , "stdDeviation"
        , "stemh"
        , "stemv"
        , "stitchTiles"
        , "stopColor"
        , "stopOpacity"
        , "strikethroughPosition"
        , "strikethroughThickness"
        , "string"
        , "stroke"
        , "strokeDasharray"
        , "strokeDashoffset"
        , "strokeLinecap"
        , "strokeLinejoin"
        , "strokeMiterlimit"
        , "strokeOpacity"
        , "strokeWidth"
        , "style"
        , "surfaceScale"
        , "systemLanguage"
        , "tableValues"
        , "target"
        , "targetX"
        , "targetY"
        , "textAnchor"
        , "textDecoration"
        , "textLength"
        , "textRendering"
        , "title"
        , "to"
        , "transform"
        , "type_"
        , "u1"
        , "u2"
        , "underlinePosition"
        , "underlineThickness"
        , "unicode"
        , "unicodeBidi"
        , "unicodeRange"
        , "unitsPerEm"
        , "vAlphabetic"
        , "vHanging"
        , "vIdeographic"
        , "vMathematical"
        , "values"
        , "version"
        , "vertAdvY"
        , "vertOriginX"
        , "vertOriginY"
        , "viewBox"
        , "viewTarget"
        , "visibility"
        , "width"
        , "widths"
        , "wordSpacing"
        , "writingMode"
        , "x"
        , "x1"
        , "x2"
        , "xChannelSelector"
        , "xHeight"
        , "xlinkActuate"
        , "xlinkArcrole"
        , "xlinkHref"
        , "xlinkRole"
        , "xlinkShow"
        , "xlinkTitle"
        , "xlinkType"
        , "xmlBase"
        , "xmlLang"
        , "xmlSpace"
        , "y"
        , "y1"
        , "y2"
        , "yChannelSelector"
        , "z"
        , "zoomAndPan"
        ]


knownNodes : Set String
knownNodes =
    Set.fromList
        [ "a"
        , "altGlyph"
        , "altGlyphDef"
        , "altGlyphItem"
        , "animate"
        , "animateColor"
        , "animateMotion"
        , "animateTransform"
        , "circle"
        , "clipPath"
        , "colorProfile"
        , "cursor"
        , "defs"
        , "desc"
        , "ellipse"
        , "feBlend"
        , "feColorMatrix"
        , "feComponentTransfer"
        , "feComposite"
        , "feConvolveMatrix"
        , "feDiffuseLighting"
        , "feDisplacementMap"
        , "feDistantLight"
        , "feFlood"
        , "feFuncA"
        , "feFuncB"
        , "feFuncG"
        , "feFuncR"
        , "feGaussianBlur"
        , "feImage"
        , "feMerge"
        , "feMergeNode"
        , "feMorphology"
        , "feOffset"
        , "fePointLight"
        , "feSpecularLighting"
        , "feSpotLight"
        , "feTile"
        , "feTurbulence"
        , "filter"
        , "font"
        , "foreignObject"
        , "g"
        , "glyph"
        , "glyphRef"
        , "image"
        , "line"
        , "linearGradient"
        , "map"
        , "marker"
        , "mask"
        , "metadata"
        , "mpath"
        , "node"
        , "path"
        , "pattern"
        , "polygon"
        , "polyline"
        , "radialGradient"
        , "rect"
        , "set"
        , "stop"
        , "style"
        , "svg"
        , "switch"
        , "symbol"
        , "text"
        , "textPath"
        , "text_"
        , "title"
        , "tref"
        , "tspan"
        , "use"
        , "view"
        ]
