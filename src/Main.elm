module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Decoders exposing (entryDecoder)
import Html exposing (Html, a, br, div, h1, h2, h3, hr, i, img, li, nav, p, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, href, src, style, target)
import Html.Events exposing (onClick)
import Http
import ListUtils
import MaybeUtils
import Model exposing (..)
import Printers exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { header : Header
    , menu : Menu
    , mainContent : MainContent
    }


type alias Header =
    { mobileMenuIsActive : Bool
    }


type alias Menu =
    { brand : Brand
    , shape : Shape
    }


type MainContent
    = Failure String
    | Loading
    | LoadedEntry Entry


init : () -> ( Model, Cmd Msg )
init _ =
    getEntry
        { header =
            { mobileMenuIsActive = False
            }
        , menu =
            { brand = JacksonStars
            , shape = Rhoads
            }
        , mainContent = Loading
        }
        "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j1.json"



-- UPDATE


type Msg
    = ToggleMobileMenu
    | GetEntry String
    | GotEntry (Result Http.Error Entry)
    | SelectBrand Brand
    | SelectShape Shape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( header, menu ) =
            ( model.header, model.menu )
    in
    case msg of
        ToggleMobileMenu ->
            ( { model | header = { header | mobileMenuIsActive = not header.mobileMenuIsActive } }, Cmd.none )

        GetEntry url ->
            getEntry model url

        GotEntry result ->
            case result of
                Ok entry ->
                    ( { model | mainContent = LoadedEntry entry }, Cmd.none )

                Err _ ->
                    ( { model | mainContent = Failure "Failed to load file as entry" }, Cmd.none )

        SelectBrand brand ->
            ( { model | menu = { menu | brand = brand, shape = Rhoads } }, Cmd.none )

        SelectShape shape ->
            ( { model | menu = { menu | shape = shape } }, Cmd.none )


getEntry : Model -> String -> ( Model, Cmd Msg )
getEntry model url =
    ( { model | mainContent = Loading }
    , Http.get
        -- Use the ORIGIN endpoint not the CDN one!
        { url = url
        , expect = Http.expectJson GotEntry entryDecoder
        }
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        rightColumnContents =
            case model.mainContent of
                Failure error ->
                    [ text error
                    ]

                Loading ->
                    [ text "Loading..."
                    ]

                LoadedEntry entry ->
                    [ h1 [ class "title is-2" ] [ text (printBrand entry.brand ++ " " ++ entry.model) ]
                    , entry.misc |> Maybe.map getTags |> Maybe.withDefault [] |> viewTags
                    , div [ class "columns is-gapless" ]
                        [ div [ class "column is-9" ]
                            [ viewSpecs entry.specs
                            , viewPrice entry.price
                            , viewPrices entry.prices
                            , viewMisc entry.misc
                            , viewLinks entry.links
                            ]
                        , div [ class "column is-3" ]
                            (viewMugshot entry.pictures)
                        ]
                    ]
    in
    div []
        [ viewHeader model.header
        , hr [] []
        , div [ class "container", style "margin-top" "2rem" ]
            [ div [ class "columns" ]
                [ div [ class "column is-3" ] [ viewMenu model.menu ]
                , div [ class "column is-9" ] rightColumnContents
                ]
            ]
        ]


viewHeader : Header -> Html Msg
viewHeader header =
    let
        ( navbarBurgerClass, navbarMenuClass ) =
            if header.mobileMenuIsActive then
                ( "navbar-burger is-active", "navbar-menu is-active" )

            else
                ( "navbar-burger", "navbar-menu" )
    in
    div [ class "hero is-white is-small" ]
        [ div [ class "hero-body", style "padding-bottom" "0px" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ div [ class "navbar-item" ]
                            [ h1 [ class "title is-4" ] [ text "Chushin Gakki Jacksons Index" ]
                            ]
                        , a
                            [ class navbarBurgerClass
                            , attribute "role" "button"
                            , attribute "aria-label" "menu"
                            , attribute "aria-expanded" "false"
                            , onClick ToggleMobileMenu
                            ]
                            [ span [ attribute "aria-hidden" "true" ] []
                            , span [ attribute "aria-hidden" "true" ] []
                            , span [ attribute "aria-hidden" "true" ] []
                            , span [ attribute "aria-hidden" "true" ] []
                            ]
                        ]
                    , div [ class navbarMenuClass ]
                        [ div [ class "navbar-start" ]
                            [ a [ class "navbar-item", onClick (SelectBrand Jackson) ]
                                [ text "Jackson" ]
                            , a [ class "navbar-item", onClick (SelectBrand GroverJackson) ]
                                [ text "Grover Jackson" ]
                            , a [ class "navbar-item", onClick (SelectBrand JacksonStars) ]
                                [ text "Jackson Stars" ]
                            ]
                        , div [ class "navbar-end" ]
                            [ span [ class "navbar-item" ]
                                [ a
                                    [ class "button is-dark is-outlined"
                                    , href "https://github.com/sophiecollard/chushin-gakki-jacksons-index"
                                    , target "blank"
                                    ]
                                    [ span [ class "icon" ]
                                        [ i [ class "fab fa-github" ] []
                                        ]
                                    , span [] [ text "View on Github" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewMenu : Menu -> Html Msg
viewMenu menu =
    case menu.brand of
        GroverJackson ->
            div [ class "menu" ]
                [ p [ class "menu-label" ] [ text "Grover Jackson models" ]
                , viewMenuListHeader "Kelly" Kelly
                , viewUnlessHidden viewGroverJacksonKellyMenuList (menu.shape == Kelly)
                , viewMenuListHeader "King V" KingV
                , viewUnlessHidden viewGroverJacksonKingVMenuList (menu.shape == KingV)
                , viewMenuListHeader "Rhoads" Rhoads
                , viewUnlessHidden viewGroverJacksonRhoadsMenuList (menu.shape == Rhoads)
                , viewMenuListHeader "Soloist" Soloist
                , viewUnlessHidden viewGroverJacksonSoloistMenuList (menu.shape == Soloist)
                ]

        Jackson ->
            div [ class "menu" ]
                [ p [ class "menu-label" ] [ text "Jackson models" ]
                , viewMenuListHeader "King V" KingV
                , viewUnlessHidden viewJacksonKingVMenuList (menu.shape == KingV)
                , viewMenuListHeader "Rhoads" Rhoads
                , viewUnlessHidden viewJacksonRhoadsMenuList (menu.shape == Rhoads)
                , viewMenuListHeader "Soloist" Soloist
                , viewUnlessHidden viewJacksonSoloistMenuList (menu.shape == Soloist)
                ]

        JacksonStars ->
            div [ class "menu" ]
                [ p [ class "menu-label" ] [ text "Jackson Stars models" ]
                , viewMenuListHeader "Archtop Soloist" ArchtopSoloist
                , viewUnlessHidden viewJacksonStarsArchtopSoloistMenuList (menu.shape == ArchtopSoloist)
                , viewMenuListHeader "Kelly" Kelly
                , viewUnlessHidden viewJacksonStarsKellyMenuList (menu.shape == Kelly)
                , viewMenuListHeader "Kelly Star" KellyStar
                , viewUnlessHidden viewJacksonStarsKellyStarMenuList (menu.shape == KellyStar)
                , viewMenuListHeader "King V" KingV
                , viewUnlessHidden viewJacksonStarsKingVMenuList (menu.shape == KingV)
                , viewMenuListHeader "Rhoads" Rhoads
                , viewUnlessHidden viewJacksonStarsRhoadsMenuList (menu.shape == Rhoads)
                , viewMenuListHeader "Soloist" Soloist
                , viewUnlessHidden viewJacksonStarsSoloistMenuList (menu.shape == Soloist)
                , viewMenuListHeader "Warrior" Warrior
                , viewUnlessHidden viewJacksonStarsWarriorMenuList (menu.shape == Warrior)
                ]


viewMenuListHeader : String -> Shape -> Html Msg
viewMenuListHeader label shape =
    p [ class "menu-label", onClick (SelectShape shape) ]
        [ a [ class "has-text-dark" ] [ text label ] ]


viewMenuListEntry : String -> String -> Html Msg
viewMenuListEntry label url =
    li [ onClick (GetEntry url) ] [ a [ class "has-text-grey-dark" ] [ text label ] ]


viewGroverJacksonKellyMenuList : Html Msg
viewGroverJacksonKellyMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Kelly Custom" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-ke-custom.json"
        ]


viewGroverJacksonKingVMenuList : Html Msg
viewGroverJacksonKingVMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Dave Mustaine Professional" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-dave-mustaine.json"
        , viewMenuListEntry "King V Custom (1990-1992)" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-kv-custom-1990-92.json"
        , viewMenuListEntry "King V Custom (1992-1994)" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-kv-custom-1992-94.json"
        ]


viewGroverJacksonRhoadsMenuList : Html Msg
viewGroverJacksonRhoadsMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Dan Spitz Professional" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-dan-spitz.json"
        , viewMenuListEntry "Randy Rhoads Professional" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-professional.json"
        , viewMenuListEntry "Randy Rhoads Custom (1990-1991)" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-custom-1990-91.json"
        , viewMenuListEntry "Randy Rhoads Custom (1991-1994)" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-custom-1991-94.json"
        ]


viewGroverJacksonSoloistMenuList : Html Msg
viewGroverJacksonSoloistMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Soloist" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl.json"
        , viewMenuListEntry "Soloist Jr" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-jr.json"
        , viewMenuListEntry "Soloist Standard" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-standard.json"
        , viewMenuListEntry "Soloist Custom" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-custom.json"
        , viewMenuListEntry "Soloist Special Custom" "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-special-custom.json"
        ]


viewJacksonStarsArchtopSoloistMenuList : Html Msg
viewJacksonStarsArchtopSoloistMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "ASL-J1" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-asl-j1.json"
        , viewMenuListEntry "ASL-TN01 (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-asl-tn01-2007-ltd.json"
        ]


viewJacksonKingVMenuList : Html Msg
viewJacksonKingVMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "King V Elite FSR" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-kv-elite-fsr.json"
        ]


viewJacksonRhoadsMenuList : Html Msg
viewJacksonRhoadsMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Kevin Bond Rhoads" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-kevin-bond-rhoads.json"
        , viewMenuListEntry "Matt Tuck Signature Rhoads" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr-matt-tuck.json"
        , viewMenuListEntry "Rhoads Elite FSR" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr-elite-fsr.json"
        , viewMenuListEntry "RR24 (2006 Limited Edition)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24-2006-ltd.json"
        , viewMenuListEntry "RR24" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24.json"
        , viewMenuListEntry "RR24M" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24m.json"
        , viewMenuListEntry "RR5" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr5.json"
        , viewMenuListEntry "RR5FR" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr5fr.json"
        ]


viewJacksonSoloistMenuList : Html Msg
viewJacksonSoloistMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "Soloist Elite FSR" "https://jackson.ams3.cdn.digitaloceanspaces.com/db/jackson-sl-elite-fsr.json"
        ]


viewJacksonStarsKellyMenuList : Html Msg
viewJacksonStarsKellyMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "KE-J1" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-j1.json"
        , viewMenuListEntry "KE-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-j2.json"
        , viewMenuListEntry "KE-TN01" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn01.json"
        , viewMenuListEntry "KE-TN02" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02.json"
        , viewMenuListEntry "KE-TN02 LTD \"Ghost Flame\" (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02-2007-gf-ltd.json"
        , viewMenuListEntry "KE-TN02 LTD \"Swirl\" (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02-2007-swirl-ltd.json"
        , viewMenuListEntry "KE-TN02STB/EMG FLAME (2009 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02stb-emg-flame.json"
        , viewMenuListEntry "KE-TN02STB/EMG QUILT (2009 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02stb-emg-quilt.json"
        ]


viewJacksonStarsKellyStarMenuList : Html Msg
viewJacksonStarsKellyStarMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "KS-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ks-j2.json"
        ]


viewJacksonStarsKingVMenuList : Html Msg
viewJacksonStarsKingVMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "KV-J1" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-j1.json"
        , viewMenuListEntry "KV-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-j2.json"
        , viewMenuListEntry "KV-TN01" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn01.json"
        , viewMenuListEntry "KV-TN02" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02.json"
        , viewMenuListEntry "KV-TN02 LTD \"Polka Dots\" (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02-2007-pd-ltd.json"
        , viewMenuListEntry "KV-TN02 LTD \"Ghost Flame\" (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02-2007-gf-ltd.json"
        ]


viewJacksonStarsRhoadsMenuList : Html Msg
viewJacksonStarsRhoadsMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "RR-J1" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j1.json"
        , viewMenuListEntry "RR-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j2.json"
        , viewMenuListEntry "RR-J2SP" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j2sp.json"
        , viewMenuListEntry "RR-TN01" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn01.json"
        , viewMenuListEntry "RR-TN01STB" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn01stb.json"
        , viewMenuListEntry "RR-TN02" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02.json"
        , viewMenuListEntry "RR-TN02STB" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb.json"
        , viewMenuListEntry "RR-TN02 LTD (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02-2007-ltd.json"
        , viewMenuListEntry "RR-TN02STB LTD (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-2007-ltd.json"
        , viewMenuListEntry "RR-TN02STB LTD \"Swirl\" (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-2007-swirl-ltd.json"
        , viewMenuListEntry "RR-TN02STB ASH (2009 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-ash.json"
        , viewMenuListEntry "RR-TN02STB WALNUT (2009 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-wal.json"
        , viewMenuListEntry "RR-TN02STB QUILT FC (2009 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-quilt-fc.json"
        ]


viewJacksonStarsSoloistMenuList : Html Msg
viewJacksonStarsSoloistMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "SL-J1" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-j1.json"
        , viewMenuListEntry "SL-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-j2.json"
        , viewMenuListEntry "SL-TN01" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01.json"
        , viewMenuListEntry "SL-TN01 (2007 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-2007-ltd.json"
        , viewMenuListEntry "SL-TN01 (2008 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-2008-ltd.json"
        , viewMenuListEntry "SL-TN01 Quilt (2008 Limited)" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-quilt.json"
        , viewMenuListEntry "SL-TN02" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn02.json"
        , viewMenuListEntry "SL-TN02STB" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn02stb.json"
        ]


viewJacksonStarsWarriorMenuList : Html Msg
viewJacksonStarsWarriorMenuList =
    ul [ class "menu-list" ]
        [ viewMenuListEntry "WR-J2" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-wr-j2.json"
        , viewMenuListEntry "WR-TN02" "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-wr-tn02.json"
        ]


viewUnlessHidden : Html Msg -> Bool -> Html Msg
viewUnlessHidden html show =
    if show then
        html

    else
        div [] []


viewTags : List Tag -> Html msg
viewTags tags =
    div [ class "field is-grouped is-grouped-multiline" ]
        (List.map viewTag tags)


viewTag : Tag -> Html msg
viewTag tag =
    case tag of
        SimpleTag colour content ->
            div [ class "control" ]
                [ div [ class "tags" ]
                    [ span [ class ("tag is-" ++ printTagColour colour) ]
                        [ text content ]
                    ]
                ]

        DoubleTag rightColour leftContent rightContent ->
            div [ class "control" ]
                [ div [ class "tags has-addons" ]
                    [ span [ class "tag" ]
                        [ text leftContent ]
                    , span [ class ("tag is-" ++ printTagColour rightColour) ]
                        [ text rightContent ]
                    ]
                ]


viewSpecs : Specs -> Html msg
viewSpecs specs =
    div []
        (List.concat
            [ [ h2 [ class "title is-4" ] [ text "Specs" ]
              ]
            , viewNeckSpecs specs.neck
            , viewHeadstockSpecs specs.headstock
            , viewBodySpecs specs.body
            , viewElectronicsSpecs specs.electronics
            , viewHardwareSpecs specs.hardware
            , viewFinishes specs.finishes
            ]
        )


viewSpecSubHeader : String -> Html msg
viewSpecSubHeader key =
    p [] [ h3 [ class "title is-5" ] [ text key ] ]


viewSpecKeyValue : String -> String -> Html msg
viewSpecKeyValue key value =
    p [] [ strong [] [ text (key ++ ": ") ], text value ]


viewNeckSpecs : NeckSpecs -> List (Html msg)
viewNeckSpecs neck =
    List.concat
        [ [ viewSpecSubHeader "Neck"
          , viewSpecKeyValue "Material" neck.material
          , viewSpecKeyValue "Construction" (printContruction neck.construction)
          , viewSpecKeyValue "Scale length" (printScaleLength neck.scaleLength)
          , viewSpecKeyValue "Nut width" (printNutWidth neck.nutWidth)
          , viewSpecKeyValue "Binding" (neck.binding |> Maybe.map .colour |> Maybe.withDefault "None")
          , viewSpecKeyValue "Fretboard material" neck.fretboard.material
          , viewSpecKeyValue "Fret count" (String.fromInt neck.fretboard.fretCount)
          ]
        , neck.fretboard.fretMaterial |> Maybe.map (viewSpecKeyValue "Fret material") |> MaybeUtils.toList
        , [ viewSpecKeyValue "Inlays type" neck.inlays.type_
          , viewSpecKeyValue "Inlays material" neck.inlays.material
          ]
        ]


viewHeadstockSpecs : HeadstockSpecs -> List (Html msg)
viewHeadstockSpecs headstock =
    [ br [] []
    , viewSpecSubHeader "Headstock"
    , viewSpecKeyValue "Type" (printHeadstockType headstock.type_)
    , viewSpecKeyValue "Finish" headstock.finish
    , viewSpecKeyValue "Logo" headstock.logoMaterial
    ]


viewBodySpecs : BodySpecs -> List (Html msg)
viewBodySpecs body =
    List.concat
        [ [ br [] []
          , viewSpecSubHeader "Body"
          , viewSpecKeyValue "Material" body.material
          ]
        , body.top |> Maybe.map (viewSpecKeyValue "Top") |> MaybeUtils.toList
        , body.binding |> Maybe.map .colour |> Maybe.map (viewSpecKeyValue "Binding") |> MaybeUtils.toList
        ]


viewElectronicsSpecs : ElectronicsSpecs -> List (Html msg)
viewElectronicsSpecs electronics =
    List.append
        [ br [] []
        , viewSpecSubHeader "Electronics"
        , viewSpecKeyValue "Controls" electronics.controls
        ]
        (viewPickupConfiguration electronics.pickupConfiguration)


viewHardwareSpecs : HardwareSpecs -> List (Html msg)
viewHardwareSpecs hardware =
    List.append
        [ br [] []
        , viewSpecSubHeader "Hardware"
        , viewSpecKeyValue "Colour" hardware.colour
        ]
        (viewBridgeConfiguration hardware.bridgeConfiguration)


viewFinishes : List String -> List (Html msg)
viewFinishes finishes =
    br [] []
        :: viewSpecSubHeader "Finishes"
        :: List.map (\finish -> p [] [ text finish ]) finishes


viewPickupConfiguration : PickupConfiguration -> List (Html msg)
viewPickupConfiguration config =
    let
        viewConfigValue : Maybe String -> Maybe String -> String -> Maybe String -> String
        viewConfigValue maybeNeck maybeMiddle bridge maybeActiveElectronics =
            let
                pickupsText =
                    case ( maybeNeck, maybeMiddle ) of
                        ( Just neck, Just middle ) ->
                            if neck == middle then
                                neck ++ " (neck & middle), " ++ bridge ++ " (bridge)"

                            else
                                neck ++ " (neck), " ++ middle ++ " (middle) & " ++ bridge ++ " (bridge)"

                        ( Just neck, Nothing ) ->
                            if neck == bridge then
                                neck ++ " (neck & bridge)"

                            else
                                neck ++ " (neck) & " ++ bridge ++ " (bridge)"

                        ( Nothing, _ ) ->
                            bridge ++ " (bridge)"

                activeElectronicsText =
                    case maybeActiveElectronics of
                        Just activeElectronics ->
                            ", " ++ activeElectronics

                        Nothing ->
                            ""
            in
            pickupsText ++ activeElectronicsText
    in
    case config of
        SimplePickupConfiguration simple ->
            [ viewSpecKeyValue "Pickups"
                (viewConfigValue simple.neck simple.middle simple.bridge simple.activeElectronics)
            ]

        ComplexPickupConfiguration complex ->
            let
                viewVariants : Variants PickupConfigurationValue -> Html msg
                viewVariants variants_ =
                    case variants_ of
                        SingleVariant { variant, value } ->
                            p [ style "margin-left" "15px" ]
                                [ strong [] [ text (variant ++ ": ") ]
                                , text (viewConfigValue value.neck value.middle value.bridge value.activeElectronics)
                                ]

                        MultipleVariants { variants, value } ->
                            p [ style "margin-left" "15px" ]
                                [ strong [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                                , text (viewConfigValue value.neck value.middle value.bridge value.activeElectronics)
                                ]
            in
            p [] [ strong [] [ text "Pickups: " ] ] :: List.map viewVariants complex


viewBridgeConfiguration : BridgeConfiguration -> List (Html msg)
viewBridgeConfiguration config =
    case config of
        SimpleBridgeConfiguration simple ->
            [ viewSpecKeyValue "Bridge" simple ]

        ComplexBridgeConfiguration complex ->
            let
                viewVariants : Variants String -> Html msg
                viewVariants variants_ =
                    case variants_ of
                        SingleVariant { variant, value } ->
                            p [ style "margin-left" "15px" ]
                                [ strong [] [ text (variant ++ ": ") ]
                                , text value
                                ]

                        MultipleVariants { variants, value } ->
                            p [ style "margin-left" "15px" ]
                                [ strong [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                                , text value
                                ]
            in
            p [] [ strong [] [ text "Bridge: " ] ] :: List.map viewVariants complex


viewPrices : Maybe (List Price) -> Html msg
viewPrices maybePrices =
    let
        viewPrice_ : Price -> Html msg
        viewPrice_ price =
            case price of
                SimplePrice { value, year, source } ->
                    viewSimplePrice value source

                ComplexPrice { values, year, source } ->
                    viewComplexPrice values source

                NewSimplePrice { value, source } ->
                    div [ style "margin-top" "1rem" ]
                        [ p [] [ text value ]
                        , p [] [ i [] [ text "Source: ", a [ href source.url ] [ text source.label ] ] ]
                        ]

                NewComplexPrice { values, source } ->
                    div [ style "margin-top" "1rem" ]
                        (List.concat
                            [ List.map viewPriceVariants values
                            , [ p [] [ i [] [ text "Source: ", a [ href source.url ] [ text source.label ] ] ] ]
                            ]
                        )
    in
    case maybePrices of
        Nothing ->
            div [] []

        Just [] ->
            div [] []

        Just prices ->
            div [ style "margin-top" "2rem" ]
                (h2 [ class "title is-4" ] [ text "Prices" ] :: List.map viewPrice_ prices)


viewSimplePrice : String -> String -> Html msg
viewSimplePrice value source =
    -- FIXME Deprecate
    div [ style "margin-top" "2rem" ]
        [ h2 [ class "title is-4" ] [ text "Price" ]
        , p [] [ text value ]
        , p [] [ i [] [ text ("Source: " ++ source) ] ]
        ]


viewNewSimplePrice : String -> Link -> Html msg
viewNewSimplePrice value source =
    div [ style "margin-top" "2rem" ]
        [ h2 [ class "title is-4" ] [ text "Price" ]
        , p [] [ text value ]
        , p [] [ i [] [ text "Source: ", a [ href source.url ] [ text source.label ] ] ]
        ]


viewComplexPrice : List (Variants String) -> String -> Html msg
viewComplexPrice values source =
    -- FIXME Deprecate
    div [ style "margin-top" "2rem" ]
        (List.concat
            [ [ h2 [ class "title is-4" ] [ text "Price" ] ]
            , List.map viewPriceVariants values
            , [ p [] [ i [] [ text ("Source: " ++ source) ] ] ]
            ]
        )


viewNewComplexPrice : List (Variants String) -> Link -> Html msg
viewNewComplexPrice values source =
    div [ style "margin-top" "2rem" ]
        (List.concat
            [ [ h2 [ class "title is-4" ] [ text "Price" ] ]
            , List.map viewPriceVariants values
            , [ p [] [ i [] [ text "Source: ", a [ href source.url ] [ text source.label ] ] ] ]
            ]
        )


viewPriceVariants : Variants String -> Html msg
viewPriceVariants variants_ =
    case variants_ of
        SingleVariant { variant, value } ->
            p []
                [ strong [] [ text (variant ++ ": ") ]
                , text value
                ]

        MultipleVariants { variants, value } ->
            p []
                [ strong [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                , text value
                ]


viewPrice : Maybe Price -> Html msg
viewPrice maybePrice =
    -- FIXME Deprecate
    case maybePrice of
        Nothing ->
            div [] []

        Just price ->
            case price of
                SimplePrice { value, year, source } ->
                    viewSimplePrice value source

                ComplexPrice { values, year, source } ->
                    viewComplexPrice values source

                NewSimplePrice { value, source } ->
                    viewNewSimplePrice value source

                NewComplexPrice { values, source } ->
                    viewNewComplexPrice values source


viewMisc : Maybe Misc -> Html msg
viewMisc maybeMisc =
    case maybeMisc of
        Nothing ->
            div [] []

        Just misc ->
            div []
                (List.concat
                    [ misc.identificationGuide |> MaybeUtils.toList |> List.filter ListUtils.nonEmpty |> List.map viewIdentficationGuide
                    , misc.additionalSections |> MaybeUtils.toList |> List.concatMap viewAdditionalSections
                    ]
                )


viewIdentficationGuide : List String -> Html msg
viewIdentficationGuide identificationGuide =
    div [ style "margin-top" "2rem" ]
        [ h2 [ class "title is-4" ] [ text "Identification Guide" ]
        , p [] [ text "To identify specimens of this model, look out for the following features:" ]
        , br [] []
        , div [ class "content" ]
            [ ul [] (List.map (\c -> li [] [ text c ]) identificationGuide) ]
        ]


viewAdditionalSections : List Section -> List (Html msg)
viewAdditionalSections sections =
    List.map viewAdditionalSection sections


viewAdditionalSection : Section -> Html msg
viewAdditionalSection section =
    div [ style "margin-top" "2rem" ]
        (h2 [ class "title is-4" ] [ text section.title ]
            :: List.map (\c -> p [] [ text c ]) section.contents
        )


viewLinks : Maybe Links -> Html msg
viewLinks maybeLinks =
    case maybeLinks of
        Nothing ->
            div [] []

        Just links ->
            let
                catalogueLinks =
                    case links.catalogues of
                        [] ->
                            []

                        _ ->
                            [ div [ style "margin-top" "2rem" ]
                                (h2 [ class "title is-4" ] [ text "Catalog Links" ] :: List.map viewLink links.catalogues)
                            ]

                reverbListings =
                    case links.reverbListings of
                        [] ->
                            []

                        _ ->
                            [ div [ style "margin-top" "2rem" ]
                                (h2 [ class "title is-4" ] [ text "Reverb Listings" ] :: List.map viewLink links.reverbListings)
                            ]
            in
            div []
                (List.concat [ catalogueLinks, reverbListings ])


viewLink : Link -> Html msg
viewLink link =
    p [] [ a [ href link.url, target "blank" ] [ text link.label ] ]


viewMugshot : Maybe Pictures -> List (Html msg)
viewMugshot maybePictures =
    case maybePictures of
        Nothing ->
            []

        Just pictures ->
            case pictures.mugshot of
                Nothing ->
                    []

                Just mugshot ->
                    [ img [ src mugshot.url, style "width" "100%" ] []
                    , i [ style "text-align" "center" ] [ p [] [ text mugshot.label ] ]
                    ]
