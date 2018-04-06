module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { hydra : Hydra
    , fighters : List FighterGroup
    , events : List Event
    }


type alias Health =
    { current : Float
    , max : Float
    , regen : Float
    }


type Head
    = Regrowing Time
    | Mature Health


type alias Hydra =
    { heads : List Head
    , body : Health
    , level : Int
    }


type alias FighterGroup =
    { tier : Tier
    , status : Status
    , size : Int
    }


type Tier
    = Villager
    | Footman
    | Archer
    | Knight


type Status
    = Reloading Time
    | ReadyToAttack Time


type alias Event =
    { tier : Tier
    , damage : Float
    }


generateHydra : Int -> Hydra
generateHydra level =
    let
        floatLevel =
            toFloat level

        headHp =
            floatLevel * 10

        headRegen =
            floatLevel * 1

        bodyHp =
            floatLevel * 100

        bodyRegen =
            floatLevel * 3

        headCount =
            level ^ 3

        head =
            Mature (Health headHp headHp headRegen)

        heads =
            List.repeat headCount head

        body =
            Health bodyHp bodyHp bodyRegen

        hydra =
            Hydra heads body level
    in
        hydra


initialModel : Model
initialModel =
    { hydra = generateHydra 1
    , fighters = initialFighters
    , events = []
    }


initialFighters : List FighterGroup
initialFighters =
    [ FighterGroup Villager (ReadyToAttack 0) 1
    ]


view : Model -> Html.Html a
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ hydraView model.hydra
                , fighterView model.fighters
                  --, eventView model.events
                ]
            , Grid.col [] []
            , Grid.col [] []
            ]
        ]


hydraView : Hydra -> Html.Html a
hydraView hydra =
    Html.div []
        [ Html.h1 [] [ Html.text "Hydra" ]
        , viewHydra hydra (List.head hydra.heads)
        ]


viewHydra : Hydra -> Maybe Head -> Html.Html a
viewHydra hydra head =
    case head of
        Just head ->
            viewHydraHead hydra head

        Nothing ->
            viewHydraBody hydra.body



{-
   progressStyle : Style
   progressStyle =
       Css.batch
           [ Css.width (px 300)
           , Css.height (px 30)
           , backgroundColor (hex "#fff")
           , Css.position Css.relative
           ]


   barStyle : Style
   barStyle =
       Css.batch
           [ backgroundColor (hex "#0f0")
           , Css.height (px 30)
           , Css.color (hex "#fff")
           ]


   spanStyle : Style
   spanStyle =
       Css.batch
           [ position absolute
           , top (px 5)
           , Css.zIndex (int 2)
           , color (hex "#000")
           , Css.textAlign center
           , Css.width (pct 100)
           ]
-}


viewHealth : Health -> Html.Html a
viewHealth health =
    let
        percentageHealth =
            (health.current / health.max * 100.0)
    in
        Html.div
            [ class "progress"
            , style
                [ ( "width", "100%" )
                , ( "height", "30px" )
                , ( "background-color", "#fff" )
                , ( "position", "relative" )
                , ( "border-width", "1px" )
                , ( "border-color", "black" )
                , ( "border-style", "solid" )
                ]
            ]
            [ Html.div
                [ class "bar"
                , style
                    [ ( "background-color", "#00b000" )
                    , ( "height", "30px" )
                    , ( "color", "#fff" )
                    , ( "width", (toString percentageHealth) ++ "%" )
                    , ( "border-width", "0px 1px 0px 0px" )
                    , ( "border-color", "black" )
                    , ( "border-style", "solid" )
                    ]
                ]
                [ Html.span
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "5px" )
                        , ( "z-index", "2" )
                        , ( "color", "#000" )
                        , ( "text-align", "center" )
                        , ( "width", "100%" )
                        ]
                    ]
                    [ Html.text
                        (toString health.current
                            ++ "/"
                            ++ toString health.max
                            ++ " (+"
                            ++ toString health.regen
                            ++ ")"
                        )
                    ]
                ]
            ]


viewHydraBody : Health -> Html.Html a
viewHydraBody body =
    Html.p [] [ Html.text "All heads are slain, it's body is vulnerable!", viewHealth body ]


viewHydraHead : Hydra -> Head -> Html.Html a
viewHydraHead hydra head =
    case head of
        Regrowing _ ->
            viewHydraBody hydra.body

        Mature health ->
            let
                headCount =
                    List.length hydra.heads

                headString =
                    if headCount > 1 then
                        "Slaying head after head... There are " ++ toString headCount ++ " heads left."
                    else
                        "There is only one head left!"
            in
                Html.p []
                    [ Html.text headString
                    , viewHealth health
                    ]


fighterView : List FighterGroup -> Html.Html a
fighterView fighters =
    Html.div []
        [ Html.p [] [ Html.text "It is being attacked by the following brave fighters:" ]
        , Html.ul [] <| List.map viewFighterGroup fighters
        ]


viewFighterGroup : FighterGroup -> Html.Html a
viewFighterGroup group =
    Html.li []
        [ Html.text <|
            "A group of "
                ++ toString group.tier
                ++ " with a size of "
                ++ toString
                    group.size
        ]



-- eventView : List Event -> Html a
-- eventView events =
--     div []
--         [ h2 [] [ text "Event Log" ]
--         , ul [] <| List.map viewEvent events
--         ]


viewEvent : Event -> Html.Html a
viewEvent event =
    Html.li [] [ Html.text <| toString event.tier ++ " has hit for " ++ toString event.damage ++ " damage" ]


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            model
                |> processAttacks diff
                |> processRegens diff
                |> \m -> ( m, Cmd.none )


processAttacks : Time -> Model -> Model
processAttacks diff model =
    let
        attackModeFighters =
            startAttacking diff model.fighters

        newHydra =
            model.hydra |> takeDamageFrom attackModeFighters

        --newEvents = logAttacks attackModeFighters
        afterAttackFighters =
            startReloading attackModeFighters
    in
        { model
            | hydra = newHydra
            , fighters =
                afterAttackFighters
                --, events = newEvents ++ model.events
        }


processRegens : Time -> Model -> Model
processRegens diff model =
    model


startAttacking : Time -> List FighterGroup -> List FighterGroup
startAttacking diff fighters =
    List.map (attemptToAttack diff) fighters


attemptToAttack : Time -> FighterGroup -> FighterGroup
attemptToAttack diff fighter =
    case fighter.status of
        Reloading soFar ->
            { fighter
                | status = applyDiffToStatus (diff + soFar) (reloadTimeFor fighter)
            }

        ReadyToAttack _ ->
            fighter


applyDiffToStatus : Time -> Time -> Status
applyDiffToStatus sinceLastAttack reloadTime =
    if sinceLastAttack < reloadTime then
        Reloading sinceLastAttack
    else
        ReadyToAttack (sinceLastAttack - reloadTime)


reloadTimeFor : FighterGroup -> Time
reloadTimeFor fighter =
    case fighter.tier of
        Villager ->
            Time.millisecond * 1000

        Footman ->
            Time.millisecond * 2000

        Archer ->
            Time.millisecond * 1500

        Knight ->
            Time.millisecond * 2000


takeDamageFrom : List FighterGroup -> Hydra -> Hydra
takeDamageFrom fighters hydra =
    List.foldl singleTargetDamage hydra fighters


singleTargetDamage : FighterGroup -> Hydra -> Hydra
singleTargetDamage fighter hydra =
    let
        heads =
            case hydra.heads of
                firstHead :: rest ->
                    case fighter.status of
                        ReadyToAttack _ ->
                            attackHead fighter hydra firstHead rest

                        Reloading _ ->
                            hydra.heads

                [] ->
                    hydra.heads

        body =
            case fighter.status of
                ReadyToAttack _ ->
                    case List.head hydra.heads of
                        Just (Mature _) ->
                            hydra.body

                        _ ->
                            attackBody fighter hydra

                Reloading _ ->
                    hydra.body
    in
        { hydra | heads = heads, body = body }


attackBody : FighterGroup -> Hydra -> Health
attackBody group hydra =
    let
        body =
            hydra.body

        remainingHp =
            body.current - damageFor group

        newBody =
            { body | current = Basics.max 0.0 remainingHp }
    in
        newBody


attackHead : FighterGroup -> Hydra -> Head -> List Head -> List Head
attackHead fighter hydra head rest =
    case head of
        Regrowing _ ->
            head :: rest

        Mature health ->
            let
                remainingHp =
                    health.current - damageFor fighter

                newHealth =
                    { health | current = Basics.max 0.0 remainingHp }
            in
                if remainingHp <= 0 then
                    (newHead hydra) ++ rest
                else
                    (Mature newHealth) :: rest


newHead : Hydra -> List Head
newHead hydra =
    List.repeat
        (floor (toFloat hydra.level * 0.5) + 1)
        (Regrowing 0)


damageFor : FighterGroup -> Float
damageFor group =
    let
        tierDamage =
            case group.tier of
                Villager ->
                    1.0

                Footman ->
                    2.0

                Archer ->
                    1.0

                Knight ->
                    3.0
    in
        tierDamage * (toFloat group.size)



--
-- logAttacks : List FighterGroup -> List Event
-- logAttacks fighters =
--     List.filterMap eventFromFighter fighters
--
--
-- eventFromFighter : FighterGroup -> Maybe Event
-- eventFromFighter fighter =
--     case fighter.status of
--         ReadyToAttack _ ->
--             Just { tier = fighter.tier, damage = damageFor fighter }
--
--         Reloading _ ->
--             Nothing


startReloading : List FighterGroup -> List FighterGroup
startReloading fighters =
    List.map reloadSingleFighter fighters


reloadSingleFighter : FighterGroup -> FighterGroup
reloadSingleFighter fighter =
    case fighter.status of
        Reloading _ ->
            fighter

        ReadyToAttack soFar ->
            { fighter | status = Reloading soFar }



-- See commit message on
-- https://github.com/JoelQ/safe-tea/commit/2b614c437c990cdc2b3240d0d5dd481f75172149
-- for more information on why using animation frame diffs is useful


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



{- TODO
   Credits:
        - joelq, https://twitter.com/joelquen, https://robots.thoughtbot.com/authors/joel-quenneville

   About section
-}
