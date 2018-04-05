module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Time exposing (Time)
import Bootstrap.CDN
import Bootstrap.Grid
import Bootstrap.Progress


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


view : Model -> Html a
view model =
    Bootstrap.Grid.container []
        [ Bootstrap.CDN.stylesheet
        , Bootstrap.Grid.row []
            [ Bootstrap.Grid.col []
                [ hydraView model.hydra
                , fighterView model.fighters
                  --, eventView model.events
                ]
            ]
        ]


hydraView : Hydra -> Html a
hydraView hydra =
    if hydra.body.current <= 0.0 then
        deadHydraView
    else
        liveHydraView hydra


deadHydraView : Html a
deadHydraView =
    div []
        [ h1 [] [ text "The hydra is DEAD" ]
        ]


liveHydraView : Hydra -> Html a
liveHydraView hydra =
    div []
        [ h1 [] [ text "The hydra is ALIVE" ]
        , viewHydra hydra (List.head hydra.heads)
        ]


viewHydra : Hydra -> Maybe Head -> Html a
viewHydra hydra head =
    case head of
        Just head ->
            viewHydraHead hydra head

        Nothing ->
            viewHydraBody hydra.body


viewHealth : Health -> Html a
viewHealth health =
    div []
        [ Bootstrap.Progress.progress
            [ Bootstrap.Progress.height 15
            , Bootstrap.Progress.label
                (toString health.current
                    ++ "/"
                    ++ toString health.max
                    ++ " (+"
                    ++ toString health.regen
                    ++ ")"
                )
            , Bootstrap.Progress.value (health.current / health.max * 100)
            ]
        ]


viewHydraBody : Health -> Html a
viewHydraBody body =
    p [] [ text "All heads are slain, it's body is vulnerable!", viewHealth body ]


viewHydraHead : Hydra -> Head -> Html a
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
                p []
                    [ text headString
                    , viewHealth health
                    ]


fighterView : List FighterGroup -> Html a
fighterView fighters =
    div []
        [ p [] [ text "It is being attacked by the following brave fighters:" ]
        , ul [] <| List.map viewFighterGroup fighters
        ]


viewFighterGroup : FighterGroup -> Html a
viewFighterGroup group =
    li []
        [ text <|
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


viewEvent : Event -> Html a
viewEvent event =
    li [] [ text <| toString event.tier ++ " has hit for " ++ toString event.damage ++ " damage" ]


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
    { hydra
        | heads =
            case hydra.heads of
                firstHead :: rest ->
                    case fighter.status of
                        ReadyToAttack _ ->
                            attackHead fighter hydra firstHead rest

                        Reloading _ ->
                            hydra.heads

                [] ->
                    hydra.heads
    }


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
                    { health | current = remainingHp }
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
