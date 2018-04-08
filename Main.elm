module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Progress as Progress
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)


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
    , status : FightStatus
    , size : Int
    }


type Tier
    = Villager
    | Footman
    | Archer
    | Knight


type FightStatus
    = Reloading Time
    | ReadyToAttack Time


type alias Event =
    { tier : Tier
    , damage : Float
    }


generateHydra : Int -> Hydra
generateHydra level =
    let
        headCount =
            level ^ 3 + 1

        head =
            generateHead level

        heads =
            List.repeat headCount head

        body =
            generateBody level

        hydra =
            Hydra heads body level
    in
        hydra


generateHead : Int -> Head
generateHead level =
    let
        floatLevel =
            toFloat level

        hp =
            floatLevel * 10

        regen =
            floatLevel * 1
    in
        Mature (generateHealth hp regen)


generateBody : Int -> Health
generateBody level =
    let
        floatLevel =
            toFloat level

        hp =
            floatLevel * 100

        regen =
            floatLevel * 3
    in
        generateHealth hp regen


damageFor : FighterGroup -> Float
damageFor group =
    let
        tierDamage =
            case group.tier of
                Villager ->
                    3.5

                Footman ->
                    2.0

                Archer ->
                    1.0

                Knight ->
                    3.0
    in
        tierDamage * (toFloat group.size)


reloadTimeFor : FighterGroup -> Time
reloadTimeFor fighter =
    case fighter.tier of
        Villager ->
            Time.millisecond * 500

        Footman ->
            Time.millisecond * 2000

        Archer ->
            Time.millisecond * 1500

        Knight ->
            Time.millisecond * 2000


regrowTime : Hydra -> Time
regrowTime hydra =
    Time.millisecond * (8000 + 2000 * toFloat hydra.level)


regrowHead : Time -> Hydra -> Head -> Head
regrowHead diff hydra head =
    case head of
        Mature _ ->
            head

        Regrowing elapsedTime ->
            if elapsedTime >= (regrowTime hydra) then
                generateHead hydra.level
            else
                Regrowing (elapsedTime + diff)


newHead : Hydra -> List Head
newHead hydra =
    List.repeat
        (floor (toFloat hydra.level * 1.5) + 2)
        (Regrowing 0)


generateHealth : Float -> Float -> Health
generateHealth hp regen =
    (Health hp hp regen)


initialModel : Model
initialModel =
    { hydra = generateHydra 1
    , fighters = initialFighters
    , events = []
    }


initialFighters : List FighterGroup
initialFighters =
    [ FighterGroup Villager (Reloading 0) 1
    ]


view : Model -> Html.Html a
view model =
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col [ Col.md4 ]
                [ hydraView model.hydra
                , fighterView model.fighters
                ]
            , Grid.col [ Col.md4 ] []
            , Grid.col [ Col.md4 ] []
            ]
        ]


hydraView : Hydra -> Html.Html a
hydraView hydra =
    Html.div []
        [ Html.h2 [] [ Html.text "Hydra" ]
        , viewHydra hydra (List.head hydra.heads)
        ]


viewHydra : Hydra -> Maybe Head -> Html.Html a
viewHydra hydra head =
    case head of
        Just head ->
            Html.div []
                [ viewHydraHead hydra head
                , viewHydraRegrowingHead hydra
                , viewHydraBody hydra.body
                ]

        Nothing ->
            viewHydraBody hydra.body


viewHydraRegrowingHead : Hydra -> Html a
viewHydraRegrowingHead hydra =
    let
        regrowingHeads =
            List.filter isheadRegrowing hydra.heads

        firstRegrowingHead =
            List.head regrowingHeads
    in
        case firstRegrowingHead of
            Just firstRegrowingHead ->
                viewRegrowingHead hydra firstRegrowingHead

            Nothing ->
                div [] []


viewRegrowingHead : Hydra -> Head -> Html a
viewRegrowingHead hydra head =
    div [] <|
        case head of
            Mature _ ->
                []

            Regrowing elapsedTime ->
                let
                    progressPercentage =
                        (elapsedTime / (regrowTime hydra) * 100)

                    progressLabel =
                        (format healthLocale progressPercentage) ++ "%"
                in
                    [ h4 [] [ text "Most regrown head:" ]
                    , viewProgressBar (toString progressPercentage) progressLabel "#a030f0"
                    ]


isheadRegrowing : Head -> Bool
isheadRegrowing head =
    case head of
        Mature _ ->
            False

        Regrowing _ ->
            True


isheadMature : Head -> Bool
isheadMature head =
    not (isheadRegrowing head)


healthLocale : Locale
healthLocale =
    { usLocale
        | decimals = 1
    }


viewHealth : Health -> Html.Html a
viewHealth health =
    let
        progressPercentage =
            toString (health.current / health.max * 100.0)

        progressLabel =
            (format healthLocale health.current
                ++ "/"
                ++ format healthLocale health.max
                ++ " (+"
                ++ format healthLocale health.regen
                ++ ")"
            )
    in
        viewProgressBar progressPercentage progressLabel "#00b000"


viewHydraBody : Health -> Html.Html a
viewHydraBody body =
    Html.p []
        [ Html.h4 [] [ text "Body:" ]
        , viewHealth body
        ]


viewHydraHead : Hydra -> Head -> Html.Html a
viewHydraHead hydra head =
    Html.p [] <|
        let
            matureHeadCount =
                List.length hydra.heads

            regrowingHeadCount =
                List.length (List.filter isheadRegrowing hydra.heads)

            totalHeadString =
                "The hydra has a total of " ++ toString matureHeadCount ++ " heads,"

            regrowingHeadString =
                " from which " ++ toString regrowingHeadCount ++ " heads are regrowing."
        in
            case head of
                Regrowing _ ->
                    [ Html.text totalHeadString
                    , div [] [ Html.text regrowingHeadString ]
                    ]

                Mature health ->
                    [ Html.text "Current head:"
                    , viewHealth health
                    , Html.text totalHeadString
                    , div [] [ Html.text regrowingHeadString ]
                    ]


viewProgressBar : String -> String -> String -> Html a
viewProgressBar percentage label color =
    Html.div
        [ class "progress"
        , style
            [ ( "height", "30px" )
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
                [ Html.b [] [ text label ]
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
                ++ " : "
                ++ toString group.size
        ]


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick diff ->
            let
                newDiff =
                    -- discard long diffs
                    if diff > 16 then
                        16
                    else
                        diff
            in
                model
                    |> processAttacks newDiff
                    |> processRegens newDiff
                    |> processRegrows newDiff
                    |> \m -> ( m, Cmd.none )



{- _ ->
   model
       |> \m -> ( m, Cmd.none )
-}


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
    { model | hydra = regenHydra diff model.hydra }


regenHydra : Time -> Hydra -> Hydra
regenHydra diff hydra =
    let
        newBody =
            regenHealth diff hydra.body

        headRegen =
            regenHead diff

        newHeads =
            List.map headRegen hydra.heads
    in
        { hydra | body = newBody, heads = newHeads }


regenHead : Time -> Head -> Head
regenHead diff head =
    case head of
        Mature health ->
            Mature (regenHealth diff health)

        Regrowing _ ->
            head


regenHealth : Time -> Health -> Health
regenHealth diff health =
    { health | current = Basics.clamp 0 health.max (health.current + diff * health.regen / 1000.0) }


processRegrows : Time -> Model -> Model
processRegrows diff model =
    { model | hydra = regrowHydra diff model.hydra }


regrowHydra : Time -> Hydra -> Hydra
regrowHydra diff hydra =
    let
        headRegrow =
            regrowHead diff hydra

        newHeads =
            List.map headRegrow hydra.heads
    in
        { hydra | heads = newHeads }


startAttacking : Time -> List FighterGroup -> List FighterGroup
startAttacking diff fighters =
    List.map (attemptToAttack diff) fighters


attemptToAttack : Time -> FighterGroup -> FighterGroup
attemptToAttack diff fighter =
    case fighter.status of
        Reloading soFar ->
            { fighter
                | status = applyDiffToFightStatus (diff + soFar) (reloadTimeFor fighter)
            }

        ReadyToAttack _ ->
            fighter


applyDiffToFightStatus : Time -> Time -> FightStatus
applyDiffToFightStatus sinceLastAttack reloadTime =
    if sinceLastAttack < reloadTime then
        Reloading sinceLastAttack
    else
        ReadyToAttack (sinceLastAttack - reloadTime)


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
                    -- move it to the end so only alive heads are at the front
                    rest ++ (newHead hydra)
                else
                    (Mature newHealth) :: rest


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

   Regrowing of heads
   Reward for kills
   New hydra when killed

   Some buttons to actually play the game

   About section
   Donation section
-}
