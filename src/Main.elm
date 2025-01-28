module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Decoders exposing (entryDecoder)
import Html exposing (Html, a, br, div, h1, h2, h3, i, img, li, nav, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, src, style, target)
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
    { menu : Menu
    , mainContent : MainContent
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
        { menu =
            { brand = JacksonStars
            , shape = Rhoads
            }
        , mainContent = Loading
        }
        "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j1.json"



-- UPDATE


type Msg
    = GetEntry String
    | GotEntry (Result Http.Error Entry)
    | SelectBrand Brand
    | SelectShape Shape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        menu =
            model.menu
    in
    case msg of
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
                    , div [ class "columns" ]
                        [ div [ class "column is-9" ]
                            [ viewSpecs entry.specs
                            , viewPrice entry.price
                            , viewMisc entry.misc
                            , viewLinks entry.links
                            ]
                        , div [ class "column is-3" ]
                            (viewMugshot entry.pictures)
                        ]
                    ]
    in
    div []
        [ viewHeader
        , div [ class "container", style "margin-top" "2rem" ]
            [ div [ class "columns" ]
                [ div [ class "column is-3" ] [ viewMenu model.menu ]
                , div [ class "column is-9" ] rightColumnContents
                ]
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "hero is-white is-small" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ div [ class "navbar-item" ]
                            [ h1 [ class "title is-4" ] [ text "Chushin Gakki Jacksons Index" ]
                            ]
                        ]
                    , div [ class "navbar-menu" ]
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
                                    [ class "button is-link is-outlined"
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
                , p [ class "menu-label", onClick (SelectShape Kelly) ] [ a [] [ text "Kelly" ] ]
                , viewUnlessHidden viewGroverJacksonKellyMenuList (menu.shape == Kelly)
                , p [ class "menu-label", onClick (SelectShape KingV) ] [ a [] [ text "King V" ] ]
                , viewUnlessHidden viewGroverJacksonKingVMenuList (menu.shape == KingV)
                , p [ class "menu-label", onClick (SelectShape Rhoads) ] [ a [] [ text "Rhoads" ] ]
                , viewUnlessHidden viewGroverJacksonRhoadsMenuList (menu.shape == Rhoads)
                , p [ class "menu-label", onClick (SelectShape Soloist) ] [ a [] [ text "Soloist" ] ]
                , viewUnlessHidden viewGroverJacksonSoloistMenuList (menu.shape == Soloist)
                ]

        Jackson ->
            div [ class "menu" ]
                [ p [ class "menu-label" ] [ text "Jackson models" ]
                , p [ class "menu-label", onClick (SelectShape Rhoads) ] [ a [] [ text "Rhoads" ] ]
                , viewUnlessHidden viewJacksonRhoadsMenuList (menu.shape == Rhoads)
                ]

        JacksonStars ->
            div [ class "menu" ]
                [ p [ class "menu-label" ] [ text "Jackson Stars models" ]
                , p [ class "menu-label", onClick (SelectShape ArchtopSoloist) ] [ a [] [ text "Archtop Soloist" ] ]
                , viewUnlessHidden viewJacksonStarsArchtopSoloistMenuList (menu.shape == ArchtopSoloist)
                , p [ class "menu-label", onClick (SelectShape Kelly) ] [ a [] [ text "Kelly" ] ]
                , viewUnlessHidden viewJacksonStarsKellyMenuList (menu.shape == Kelly)
                , p [ class "menu-label", onClick (SelectShape KellyStar) ] [ a [] [ text "Kelly Star" ] ]
                , viewUnlessHidden viewJacksonStarsKellyStarMenuList (menu.shape == KellyStar)
                , p [ class "menu-label", onClick (SelectShape KingV) ] [ a [] [ text "King V" ] ]
                , viewUnlessHidden viewJacksonStarsKingVMenuList (menu.shape == KingV)
                , p [ class "menu-label", onClick (SelectShape Rhoads) ] [ a [] [ text "Rhoads" ] ]
                , viewUnlessHidden viewJacksonStarsRhoadsMenuList (menu.shape == Rhoads)
                , p [ class "menu-label", onClick (SelectShape Soloist) ] [ a [] [ text "Soloist" ] ]
                , viewUnlessHidden viewJacksonStarsSoloistMenuList (menu.shape == Soloist)
                , p [ class "menu-label", onClick (SelectShape Warrior) ] [ a [] [ text "Warrior" ] ]
                , viewUnlessHidden viewJacksonStarsWarriorMenuList (menu.shape == Warrior)
                ]


viewGroverJacksonKellyMenuList : Html Msg
viewGroverJacksonKellyMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-ke-custom.json") ]
            [ a [] [ text "Kelly Custom" ] ]
        ]


viewGroverJacksonKingVMenuList : Html Msg
viewGroverJacksonKingVMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-kv-custom-1990-92.json") ]
            [ a [] [ text "King V Custom (1990-1992)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-kv-custom-1992-94.json") ]
            [ a [] [ text "King V Custom (1992-1994)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-dave-mustaine.json") ]
            [ a [] [ text "Dave Mustaine Professional" ] ]
        ]


viewGroverJacksonRhoadsMenuList : Html Msg
viewGroverJacksonRhoadsMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-professional.json") ]
            [ a [] [ text "Randy Rhoads Professional" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-custom-1990-91.json") ]
            [ a [] [ text "Randy Rhoads Custom (1990-1991)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-rr-custom-1991-94.json") ]
            [ a [] [ text "Randy Rhoads Custom (1991-1994)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-dan-spitz.json") ]
            [ a [] [ text "Dan Spitz Professional" ] ]
        ]


viewGroverJacksonSoloistMenuList : Html Msg
viewGroverJacksonSoloistMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl.json") ]
            [ a [] [ text "Soloist" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-jr.json") ]
            [ a [] [ text "Soloist Jr" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-standard.json") ]
            [ a [] [ text "Soloist Standard" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-custom.json") ]
            [ a [] [ text "Soloist Custom" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/grover-jackson-sl-special-custom.json") ]
            [ a [] [ text "Soloist Special Custom" ] ]
        ]


viewJacksonStarsArchtopSoloistMenuList : Html Msg
viewJacksonStarsArchtopSoloistMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-asl-j1.json") ]
            [ a [] [ text "ASL-J1" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-asl-tn01-2007-ltd.json") ]
            [ a [] [ text "ASL-TN01 (2007 Limited)" ] ]
        ]


viewJacksonRhoadsMenuList : Html Msg
viewJacksonRhoadsMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-kevin-bond-rhoads.json") ]
            [ a [] [ text "Kevin Bond Rhoads" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24-2006-ltd.json") ]
            [ a [] [ text "RR24 (2006 Limited Edition)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24.json") ]
            [ a [] [ text "RR24" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-rr24m.json") ]
            [ a [] [ text "RR24M" ] ]
        ]


viewJacksonStarsKellyMenuList : Html Msg
viewJacksonStarsKellyMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-j1.json") ]
            [ a [] [ text "KE-J1" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-j2.json") ]
            [ a [] [ text "KE-J2" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn01.json") ]
            [ a [] [ text "KE-TN01" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02.json") ]
            [ a [] [ text "KE-TN02" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02-2007-gf-ltd.json") ]
            [ a [] [ text "KE-TN02 LTD \"Ghost Flame\" (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02-2007-swirl-ltd.json") ]
            [ a [] [ text "KE-TN02 LTD \"Swirl\" (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02stb-emg-flame.json") ]
            [ a [] [ text "KE-TN02STB/EMG FLAME (2009 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ke-tn02stb-emg-quilt.json") ]
            [ a [] [ text "KE-TN02STB/EMG QUILT (2009 Limited)" ] ]
        ]


viewJacksonStarsKellyStarMenuList : Html Msg
viewJacksonStarsKellyStarMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-ks-j2.json") ]
            [ a [] [ text "KS-J2" ] ]
        ]


viewJacksonStarsKingVMenuList : Html Msg
viewJacksonStarsKingVMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-j1.json") ]
            [ a [] [ text "KV-J1" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-j2.json") ]
            [ a [] [ text "KV-J2" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn01.json") ]
            [ a [] [ text "KV-TN01" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02.json") ]
            [ a [] [ text "KV-TN02" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02-2007-pd-ltd.json") ]
            [ a [] [ text "KV-TN02 LTD \"Polka Dots\" (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-kv-tn02-2007-gf-ltd.json") ]
            [ a [] [ text "KV-TN02 LTD \"Ghost Flame\" (2007 Limited)" ] ]
        ]


viewJacksonStarsRhoadsMenuList : Html Msg
viewJacksonStarsRhoadsMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j1.json") ]
            [ a [] [ text "RR-J1" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j2.json") ]
            [ a [] [ text "RR-J2" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-j2sp.json") ]
            [ a [] [ text "RR-J2SP" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn01.json") ]
            [ a [] [ text "RR-TN01" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn01stb.json") ]
            [ a [] [ text "RR-TN01STB" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02.json") ]
            [ a [] [ text "RR-TN02" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb.json") ]
            [ a [] [ text "RR-TN02STB" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02-2007-ltd.json") ]
            [ a [] [ text "RR-TN02 LTD (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-2007-ltd.json") ]
            [ a [] [ text "RR-TN02STB LTD (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-2007-swirl-ltd.json") ]
            [ a [] [ text "RR-TN02STB LTD \"Swirl\" (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-ash.json") ]
            [ a [] [ text "RR-TN02STB ASH (2009 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-wal.json") ]
            [ a [] [ text "RR-TN02STB WALNUT (2009 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-rr-tn02stb-quilt-fc.json") ]
            [ a [] [ text "RR-TN02STB QUILT FC (2009 Limited)" ] ]
        ]


viewJacksonStarsSoloistMenuList : Html Msg
viewJacksonStarsSoloistMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-j1.json") ]
            [ a [] [ text "SL-J1" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-j2.json") ]
            [ a [] [ text "SL-J2" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01.json") ]
            [ a [] [ text "SL-TN01" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-2007-ltd.json") ]
            [ a [] [ text "SL-TN01 (2007 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-2008-ltd.json") ]
            [ a [] [ text "SL-TN01 (2008 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn01-quilt.json") ]
            [ a [] [ text "SL-TN01 Quilt (2008 Limited)" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn02.json") ]
            [ a [] [ text "SL-TN02" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-sl-tn02stb.json") ]
            [ a [] [ text "SL-TN02STB" ] ]
        ]


viewJacksonStarsWarriorMenuList : Html Msg
viewJacksonStarsWarriorMenuList =
    ul [ class "menu-list" ]
        [ li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-wr-j2.json") ]
            [ a [] [ text "WR-J2" ] ]
        , li
            [ onClick (GetEntry "https://jackson.ams3.digitaloceanspaces.com/db/jackson-stars-wr-tn02.json") ]
            [ a [] [ text "WR-TN02" ] ]
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


viewNeckSpecs : NeckSpecs -> List (Html msg)
viewNeckSpecs neck =
    [ p []
        [ h3 [ class "title is-5" ] [ text "Neck" ]
        ]
    , p []
        [ strong [] [ text "Material: " ]
        , text neck.material
        ]
    , p []
        [ strong [] [ text "Construction: " ]
        , text (printContruction neck.construction)
        ]
    , p []
        [ strong [] [ text "Scale length: " ]
        , text (printScaleLength neck.scaleLength)
        ]
    , p []
        [ strong [] [ text "Nut width: " ]
        , text (printNutWidth neck.nutWidth)
        ]
    , p []
        [ strong [] [ text "Binding: " ]
        , text (neck.binding |> Maybe.map .colour |> Maybe.withDefault "None")
        ]
    , p []
        [ strong [] [ text "Fret count: " ]
        , text (String.fromInt neck.fretboard.fretCount)
        ]
    , p []
        [ strong [] [ text "Fretboard material: " ]
        , text neck.fretboard.material
        ]
    , p []
        [ strong [] [ text "Inlays type: " ]
        , text neck.inlays.type_
        ]
    , p []
        [ strong [] [ text "Inlays material: " ]
        , text neck.inlays.material
        ]
    ]


viewHeadstockSpecs : HeadstockSpecs -> List (Html msg)
viewHeadstockSpecs headstock =
    [ br [] []
    , p []
        [ h3 [ class "title is-5" ] [ text "Headstock" ]
        ]
    , p []
        [ strong [] [ text "Type: " ]
        , text (printHeadstockType headstock.type_)
        ]
    , p []
        [ strong [] [ text "Finish: " ]
        , text headstock.finish
        ]
    , p []
        [ strong [] [ text "Logo: " ]
        , text headstock.logoMaterial
        ]
    ]


viewBodySpecs : BodySpecs -> List (Html msg)
viewBodySpecs body =
    let
        viewTop : Maybe String -> List (Html msg)
        viewTop maybeTop =
            case maybeTop of
                Just top ->
                    [ p []
                        [ strong [] [ text "Top: " ]
                        , text top
                        ]
                    ]

                Nothing ->
                    []

        viewBinding : Maybe BindingSpecs -> List (Html msg)
        viewBinding maybeBinding =
            case maybeBinding of
                Just binding ->
                    [ p []
                        [ strong [] [ text "Binding: " ]
                        , text binding.colour
                        ]
                    ]

                Nothing ->
                    []
    in
    List.concat
        [ [ br [] []
          , p []
                [ h3 [ class "title is-5" ] [ text "Body" ]
                ]
          , p []
                [ strong [] [ text "Material: " ]
                , text body.material
                ]
          ]
        , viewTop body.top
        , viewBinding body.binding
        ]


viewElectronicsSpecs : ElectronicsSpecs -> List (Html msg)
viewElectronicsSpecs electronics =
    List.append
        [ br [] []
        , p []
            [ h3 [ class "title is-5" ] [ text "Electronics" ]
            ]
        , p []
            [ strong [] [ text "Controls: " ]
            , text electronics.controls
            ]
        ]
        (viewPickupConfiguration electronics.pickupConfiguration)


viewHardwareSpecs : HardwareSpecs -> List (Html msg)
viewHardwareSpecs hardware =
    List.append
        [ br [] []
        , p []
            [ h3 [ class "title is-5" ] [ text "Hardware" ]
            ]
        , p []
            [ strong [] [ text "Colour: " ]
            , text hardware.colour
            ]
        ]
        (viewBridgeConfiguration hardware.bridgeConfiguration)


viewFinishes : List String -> List (Html msg)
viewFinishes finishes =
    br [] []
        :: p [] [ h3 [ class "title is-5" ] [ text "Finishes" ] ]
        :: List.map (\finish -> p [] [ text finish ]) finishes


viewPickupConfiguration : PickupConfiguration -> List (Html msg)
viewPickupConfiguration config =
    let
        viewConfigValue : Maybe String -> Maybe String -> String -> Maybe String -> Html msg
        viewConfigValue maybeNeck maybeMiddle bridge maybeActiveElectronics =
            let
                pickupsText =
                    case ( maybeNeck, maybeMiddle ) of
                        ( Just neck, Just middle ) ->
                            if neck == middle then
                                neck ++ " (neck & middle), " ++ bridge ++ " (bridge)"

                            else
                                neck ++ " (neck), " ++ middle ++ " (middle), " ++ bridge ++ " (bridge)"

                        ( Just neck, Nothing ) ->
                            if neck == bridge then
                                neck ++ " (neck & bridge)"

                            else
                                neck ++ " (neck), " ++ bridge ++ " (bridge)"

                        ( Nothing, _ ) ->
                            bridge ++ " (bridge)"

                activeElectronicsText =
                    case maybeActiveElectronics of
                        Just activeElectronics ->
                            ", " ++ activeElectronics

                        Nothing ->
                            ""
            in
            text (pickupsText ++ activeElectronicsText)
    in
    case config of
        SimplePickupConfiguration simple ->
            [ p []
                [ strong [] [ text "Pickups: " ]
                , viewConfigValue simple.neck simple.middle simple.bridge simple.activeElectronics
                ]
            ]

        ComplexPickupConfiguration complex ->
            let
                viewVariants : Variants PickupConfigurationValue -> Html msg
                viewVariants variants_ =
                    case variants_ of
                        SingleVariant { variant, value } ->
                            p []
                                [ i [] [ text (variant ++ ": ") ]
                                , viewConfigValue value.neck value.middle value.bridge value.activeElectronics
                                ]

                        MultipleVariants { variants, value } ->
                            p []
                                [ i [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                                , viewConfigValue value.neck value.middle value.bridge value.activeElectronics
                                ]
            in
            p [] [ strong [] [ text "Pickups: " ] ] :: List.map viewVariants complex


viewBridgeConfiguration : BridgeConfiguration -> List (Html msg)
viewBridgeConfiguration config =
    case config of
        SimpleBridgeConfiguration simple ->
            [ p []
                [ strong [] [ text "Bridge: " ]
                , text simple
                ]
            ]

        ComplexBridgeConfiguration complex ->
            let
                viewVariants : Variants String -> Html msg
                viewVariants variants_ =
                    case variants_ of
                        SingleVariant { variant, value } ->
                            p []
                                [ i [] [ text (variant ++ ": ") ]
                                , text value
                                ]

                        MultipleVariants { variants, value } ->
                            p []
                                [ i [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                                , text value
                                ]
            in
            p [] [ strong [] [ text "Bridge: " ] ] :: List.map viewVariants complex


viewPrice : Maybe Price -> Html msg
viewPrice maybePrice =
    case maybePrice of
        Nothing ->
            div [] []

        Just price ->
            let
                viewValue : String -> Html msg
                viewValue value =
                    text value

                viewVariants : Variants String -> Html msg
                viewVariants variants_ =
                    case variants_ of
                        SingleVariant { variant, value } ->
                            p []
                                [ i [] [ text (variant ++ ": ") ]
                                , viewValue value
                                ]

                        MultipleVariants { variants, value } ->
                            p []
                                [ i [] [ variants |> List.intersperse ", " |> String.concat |> (\s -> s ++ ": ") |> text ]
                                , viewValue value
                                ]
            in
            case price of
                SimplePrice { value, year, source } ->
                    div [ style "margin-top" "2rem" ]
                        [ h2 [ class "title is-4" ] [ text "Price" ]
                        , p [] [ strong [] [ text ("Source: " ++ source) ] ]
                        , p [] [ viewValue value ]
                        ]

                ComplexPrice { values, year, source } ->
                    div [ style "margin-top" "2rem" ]
                        (h2 [ class "title is-4" ] [ text "Price" ]
                            :: p [] [ strong [] [ text ("Source: " ++ source) ] ]
                            :: List.map viewVariants values
                        )


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
                                (h2 [ class "title is-4" ] [ text "Past Reverb Listings" ] :: List.map viewLink links.reverbListings)
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
                    [ img [ src mugshot.url ] []
                    , i [ style "text-align" "center" ] [ p [] [ text mugshot.label ] ]
                    ]
