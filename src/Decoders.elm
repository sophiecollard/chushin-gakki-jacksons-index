module Decoders exposing (..)

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map, map2, map3, map5, map6, map7, maybe, oneOf, string, succeed)
import Model exposing (..)


entryDecoder : Decoder Entry
entryDecoder =
    map7 Entry
        (field "brand" brandDecoder)
        (field "model" string)
        (field "variants" (maybe (list string)))
        (field "years_of_production" yearsOfProductionDecoder)
        (field "specs" specsDecoder)
        (field "price" priceDecoder)
        (field "notes" (maybe (list string)))


brandDecoder : Decoder Brand
brandDecoder =
    let
        f : String -> Decoder Brand
        f str =
            case str of
                "Grover Jackson" ->
                    succeed GroverJackson

                "Jackson" ->
                    succeed Jackson

                "Jackson Stars" ->
                    succeed JacksonStars

                _ ->
                    fail "Not a valid brand"
    in
    string |> andThen f


yearsOfProductionDecoder : Decoder YearsOfProduction
yearsOfProductionDecoder =
    oneOf
        [ map2 MultipleYears (field "from" int) (field "until" int)
        , map SingleYear int
        ]


specsDecoder : Decoder Specs
specsDecoder =
    map5 Specs
        (field "neck" neckSpecsDecoder)
        (field "body" bodySpecsDecoder)
        (field "electronics" electronicsSpecsDecoder)
        (field "hardware" hardwareSpecsDecoder)
        (field "finishes" (list string))


neckSpecsDecoder : Decoder NeckSpecs
neckSpecsDecoder =
    map7 NeckSpecs
        (field "material" string)
        (field "construction" constructionDecoder)
        (field "scale_length" scaleLengthDecoder)
        (field "fretboard" fretboardSpecsDecoder)
        (field "inlays" inlaysSpecsDecoder)
        (field "binding" bindingSpecsDecoder)
        (field "headstock" headstockSpecsDecoder)


fretboardSpecsDecoder : Decoder FretboardSpecs
fretboardSpecsDecoder =
    map2 FretboardSpecs
        (field "material" string)
        (field "fret_count" int)


inlaysSpecsDecoder : Decoder InlaysSpecs
inlaysSpecsDecoder =
    map2 InlaysSpecs
        (field "material" string)
        (field "type" string)


bindingSpecsDecoder : Decoder BindingSpecs
bindingSpecsDecoder =
    map BindingSpecs
        (field "colour" string)


headstockSpecsDecoder : Decoder HeadstockSpecs
headstockSpecsDecoder =
    map3 HeadstockSpecs
        (field "type" headstockTypeDecoder)
        (field "finish" string)
        (field "logo_material" string)


headstockTypeDecoder : Decoder HeadstockType
headstockTypeDecoder =
    let
        f : String -> Decoder HeadstockType
        f str =
            case str of
                "regular" ->
                    succeed Regular

                "reverse" ->
                    succeed Reverse

                _ ->
                    fail "Not a valid brand"
    in
    string |> andThen f


constructionDecoder : Decoder Construction
constructionDecoder =
    let
        f : String -> Decoder Construction
        f str =
            case str of
                "neck_through" ->
                    succeed NeckThrough

                "set_neck" ->
                    succeed SetNeck

                "bolt_on" ->
                    succeed BoltOn

                other ->
                    fail ("Not a valid construction value: " ++ other)
    in
    oneOf
        [ string |> andThen f ]


scaleLengthDecoder : Decoder ScaleLength
scaleLengthDecoder =
    let
        f : String -> Decoder ScaleLength
        f str =
            case str of
                "24.75\"" ->
                    succeed Inches24_75

                "628mm" ->
                    succeed Inches24_75

                "25.5\"" ->
                    succeed Inches25_5

                "648mm" ->
                    succeed Inches25_5

                other ->
                    fail ("Not a valid scale length value: " ++ other)
    in
    oneOf
        [ string |> andThen f ]


bodySpecsDecoder : Decoder BodySpecs
bodySpecsDecoder =
    map3 BodySpecs
        (field "material" string)
        (field "top" (maybe string))
        (field "binding" (maybe bindingSpecsDecoder))


electronicsSpecsDecoder : Decoder ElectronicsSpecs
electronicsSpecsDecoder =
    map2 ElectronicsSpecs
        (field "controls" string)
        (field "pickup_configuration" pickupConfigurationDecoder)


hardwareSpecsDecoder : Decoder HardwareSpecs
hardwareSpecsDecoder =
    map2 HardwareSpecs
        (field "colour" string)
        (field "bridge_configuration" bridgeConfigurationDecoder)


pickupConfigurationDecoder : Decoder PickupConfiguration
pickupConfigurationDecoder =
    oneOf
        [ map SimplePickupConfiguration pickupConfigurationValueDecoder
        , map ComplexPickupConfiguration (pickupConfigurationValueDecoder |> variantsDecoder |> list)
        ]


pickupConfigurationValueDecoder : Decoder PickupConfigurationValue
pickupConfigurationValueDecoder =
    map2 PickupConfigurationValue
        (field "neck" (maybe string))
        (field "bridge" string)


bridgeConfigurationDecoder : Decoder BridgeConfiguration
bridgeConfigurationDecoder =
    oneOf
        [ map SimpleBridgeConfiguration string
        , map ComplexBridgeConfiguration (string |> variantsDecoder |> list)
        ]


priceDecoder : Decoder Price
priceDecoder =
    oneOf
        [ map2 (\v y -> SimplePrice { value = v, year = y })
            (field "value" string)
            (field "year" int)
        , map2 (\v y -> ComplexPrice { values = v, year = y })
            (field "values" (string |> variantsDecoder |> list))
            (field "year" int)
        ]


variantsDecoder : Decoder a -> Decoder (Variants a)
variantsDecoder decoderA =
    oneOf
        [ map2
            (\variant value -> SingleVariant { variant = variant, value = value })
            (field "variant" string)
            (field "value" decoderA)
        , map2
            (\variants value -> MultipleVariants { variants = variants, value = value })
            (field "variants" (list string))
            (field "value" decoderA)
        ]
