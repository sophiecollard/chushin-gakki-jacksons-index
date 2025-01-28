module Decoders exposing (..)

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map, map2, map3, map4, map6, map7, maybe, oneOf, string, succeed)
import Model exposing (..)


entryDecoder : Decoder Entry
entryDecoder =
    map7 Entry
        (field "brand" brandDecoder)
        (field "model" string)
        (field "specs" specsDecoder)
        (optionalField "price" priceDecoder)
        (optionalField "pictures" picturesDecoder)
        (optionalField "links" linksDecoder)
        (optionalField "misc" miscDecoder)


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


tagColourDecoder : Decoder TagColour
tagColourDecoder =
    let
        f : String -> Decoder TagColour
        f str =
            case String.toLower str of
                "dark" ->
                    succeed DarkTag

                "light" ->
                    succeed LightTag

                "primary" ->
                    succeed PrimaryTag

                "link" ->
                    succeed LinkTag

                "info" ->
                    succeed InfoTag

                "success" ->
                    succeed SuccessTag

                "warning" ->
                    succeed WarningTag

                "danger" ->
                    succeed DangerTag

                _ ->
                    fail "Not a valid tag colour"
    in
    string |> andThen f


specsDecoder : Decoder Specs
specsDecoder =
    map6 Specs
        (field "neck" neckSpecsDecoder)
        (field "headstock" headstockSpecsDecoder)
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
        (field "nut_width" nutWidthDecoder)
        (field "fretboard" fretboardSpecsDecoder)
        (field "inlays" inlaysSpecsDecoder)
        (optionalField "binding" bindingSpecsDecoder)


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
            case String.toLower str of
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


nutWidthDecoder : Decoder NutWidth
nutWidthDecoder =
    let
        f : String -> Decoder NutWidth
        f str =
            case str of
                "1.625\"" ->
                    succeed Inches1_625

                "41.30mm" ->
                    succeed Inches1_625

                "42mm" ->
                    succeed Inches1_625

                "1.6875\"" ->
                    succeed Inches1_6875

                "42.85mm" ->
                    succeed Inches1_6875

                "43mm" ->
                    succeed Inches1_6875

                other ->
                    fail ("Not a valid nut width value: " ++ other)
    in
    oneOf
        [ string |> andThen f ]


bodySpecsDecoder : Decoder BodySpecs
bodySpecsDecoder =
    map3 BodySpecs
        (field "material" string)
        (optionalField "top" string)
        (optionalField "binding" bindingSpecsDecoder)


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
    map4 PickupConfigurationValue
        (optionalField "neck" string)
        (optionalField "middle" string)
        (field "bridge" string)
        (optionalField "active_electronics" string)


bridgeConfigurationDecoder : Decoder BridgeConfiguration
bridgeConfigurationDecoder =
    oneOf
        [ map SimpleBridgeConfiguration string
        , map ComplexBridgeConfiguration (string |> variantsDecoder |> list)
        ]


priceDecoder : Decoder Price
priceDecoder =
    oneOf
        [ map3 (\v y s -> SimplePrice { value = v, year = y, source = s })
            (field "value" string)
            (field "year" int)
            (field "source" string)
        , map3 (\v y s -> ComplexPrice { values = v, year = y, source = s })
            (field "values" (string |> variantsDecoder |> list))
            (field "year" int)
            (field "source" string)
        ]


picturesDecoder : Decoder Pictures
picturesDecoder =
    map Pictures
        (optionalField "mugshot" mugshotDecoder)


mugshotDecoder : Decoder Mugshot
mugshotDecoder =
    map2 Mugshot
        (field "label" string)
        (field "url" string)


linksDecoder : Decoder Links
linksDecoder =
    map2 Links
        (field "catalogues" (list linkDecoder))
        (field "reverb_listings" (list linkDecoder))


linkDecoder : Decoder Link
linkDecoder =
    map2 Link
        (field "label" string)
        (field "url" string)


miscDecoder : Decoder Misc
miscDecoder =
    map6 Misc
        (field "availability" availabilityDecoder)
        (field "years_of_manufacture" yearsOfManufactureDecoder)
        (optionalField "limited_series" string)
        (optionalField "variants" (list string))
        (optionalField "identification_guide" (list string))
        (optionalField "additional_sections" (list sectionDecoder))


availabilityDecoder : Decoder Availability
availabilityDecoder =
    let
        f : String -> Decoder Availability
        f str =
            case String.toLower str of
                "japan" ->
                    succeed Japan

                "worldwide" ->
                    succeed Worldwide

                "worldwide_excl_japan" ->
                    succeed WorldwideExclJapan

                _ ->
                    fail "Not a valid availability"
    in
    string |> andThen f


yearsOfManufactureDecoder : Decoder YearsOfManufacture
yearsOfManufactureDecoder =
    map2 YearsOfManufacture
        (field "from" int)
        (optionalField "until" int)


sectionDecoder : Decoder Section
sectionDecoder =
    map2 Section
        (field "title" string)
        (field "contents" (list string))


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


optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField fieldName decoder =
    oneOf
        [ field fieldName (maybe decoder)
        , succeed Nothing
        ]
