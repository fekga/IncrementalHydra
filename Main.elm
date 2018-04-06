module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup


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
            level ^ 3 + 1

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
            Html.div []
                [ viewHydraHead hydra head
                , viewHydraBody hydra.body
                ]

        Nothing ->
            viewHydraBody hydra.body


viewHealth : Health -> Html.Html a
viewHealth health =
    let
        progressPercentage =
            toString (health.current / health.max * 100.0)

        progressLabel =
            (toString health.current
                ++ "/"
                ++ toString health.max
                ++ " (+"
                ++ toString health.regen
                ++ ")"
            )
    in
        viewProgressBar progressPercentage progressLabel "#00b000"


viewHydraBody : Health -> Html.Html a
viewHydraBody body =
    Html.p []
        [ Html.text "Body:"
        , viewHealth body
        ]


viewHydraHead : Hydra -> Head -> Html.Html a
viewHydraHead hydra head =
    case head of
        Regrowing _ ->
            Html.p [] []

        Mature health ->
            let
                headCount =
                    (List.length hydra.heads) - 1

                headString =
                    if headCount > 1 then
                        "The hydra has  " ++ toString headCount ++ " more heads."
                    else if headCount == 1 then
                        "The hydra has  " ++ toString headCount ++ " more head."
                    else
                        "There is only one head left!"
            in
                Html.p []
                    [ Html.text "Current head:"
                    , viewHealth health
                    , Html.text headString
                    ]


viewProgressBar : String -> String -> String -> Html a
viewProgressBar percentage label color =
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
                [ ( "background-color", color )
                , ( "height", "30px" )
                , ( "color", "#fff" )
                , ( "width", percentage ++ "%" )
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
                [ Html.text label
                ]
            ]
        ]


fighterView : List FighterGroup -> Html.Html a
fighterView fighters =
    Html.div []
        [ Html.p [] [ Html.text "It is being attacked by the following brave fighters:" ]
        , ListGroup.ul <| List.map viewFighterGroup fighters
        ]


viewFighterGroup : FighterGroup -> ListGroup.Item a
viewFighterGroup group =
    ListGroup.li []
        [ Html.text <|
            toString group.tier
                ++ " with a size of "
                ++ toString group.size
        ]


type Msg
    = Tick Time
    | Buy {- todo -}
    | Sell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            model
                |> processAttacks diff
                |> processRegens diff
                |> \m -> ( m, Cmd.none )

        _ ->
            model
                |> \m -> ( m, Cmd.none )


processAttacks : Time -> Model -> Model
processAttacks diff model =
    let
        attackModeFighters =
            startAttacking diff model.fighters

        newHydra =
            model.hydra |> takeDamageFrom attackModeFighters

        afterAttackFighters =
            startReloading attackModeFighters
    in
        { model
            | hydra = newHydra
            , fighters =
                afterAttackFighters
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
        - elm-slack, https://elmlang.slack.com/messages/C192T0Q1E/
        - joelq, https://twitter.com/joelquen, https://robots.thoughtbot.com/authors/joel-quenneville

    Regeneration
    Regrowing of heads
    Reward for kills
    New hydra when killed

    Some buttons to actually play the game

   About section
   Donation section
-}
