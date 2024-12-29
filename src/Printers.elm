module Printers exposing (..)

import Model exposing (..)


printBrand : Brand -> String
printBrand brand =
    case brand of
        GroverJackson ->
            "Grover Jackson"

        Jackson ->
            "Jackson"

        JacksonStars ->
            "Jackson Stars"


printYearsOfProduction : YearsOfProduction -> String
printYearsOfProduction yop =
    case yop of
        SingleYear year ->
            String.fromInt year

        MultipleYears from until ->
            String.fromInt from ++ "-" ++ String.fromInt until


printContruction : Construction -> String
printContruction construction =
    case construction of
        NeckThrough ->
            "Neck-through"

        SetNeck ->
            "Set neck"

        BoltOn ->
            "Bolt-on"


printScaleLength : ScaleLength -> String
printScaleLength scaleLength =
    case scaleLength of
        Inches24_75 ->
            "24.75\" (628mm)"

        Inches25_5 ->
            "25.5\" (648mm)"


printHeadstockType : HeadstockType -> String
printHeadstockType type_ =
    case type_ of
        Regular ->
            "Regular"

        Reverse ->
            "Reverse"
