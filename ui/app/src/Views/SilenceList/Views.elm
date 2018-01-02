module Views.SilenceList.Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Silences.Types exposing (Silence, State(..), stateToString, SilenceId)
import Types exposing (Msg(MsgForSilenceList, Noop, UpdateFilter))
import Utils.Api exposing (withDefault)
import Utils.String as StringUtils
import Utils.Types exposing (ApiData(..), Matcher)
import Utils.Views exposing (buttonLink, checkbox, error, formField, formInput, iconButtonMsg, loading, textField)
import Views.FilterBar.Views as FilterBar
import Views.SilenceList.SilenceView
import Views.SilenceList.Types exposing (Model, SilenceListMsg(..))
import Html.Lazy exposing (lazy2, lazy3)


view : Model -> Html Msg
view { filterBar, tab, silences, showConfirmationDialog } =
    div []
        [ div [ class "mb-4" ]
            [ label [ class "mb-2", for "filter-bar-matcher" ] [ text "Filter" ]
            , Html.map (MsgForFilterBar >> MsgForSilenceList) (FilterBar.view filterBar)
            ]
        , lazy2 tabsView tab silences
        , lazy3 silencesView showConfirmationDialog tab silences
        ]


tabsView : State -> ApiData (List Silence) -> Html Msg
tabsView tab silences =
    List.map (tabView tab) (groupSilencesByState (withDefault [] silences))
        |> ul [ class "nav nav-tabs mb-4" ]


tabView : State -> ( State, List a ) -> Html Msg
tabView currentState ( state, silences ) =
    Utils.Views.tab state currentState (SetTab >> MsgForSilenceList) <|
        case List.length silences of
            0 ->
                [ text (StringUtils.capitalizeFirst (stateToString state)) ]

            n ->
                [ text (StringUtils.capitalizeFirst (stateToString state))
                , span
                    [ class "badge badge-pillow badge-default align-text-top ml-2" ]
                    [ text (toString n) ]
                ]


silencesView : Maybe SilenceId -> State -> ApiData (List Silence) -> Html Msg
silencesView showConfirmationDialog state silences =
    case silences of
        Success sils ->
            let
                silencesInTab =
                    filterSilencesByState state sils
            in
                if List.isEmpty silencesInTab then
                    Utils.Views.error "No silences found"
                else
                    ul [ class "list-group" ]
                        (List.map
                            (\silence ->
                                Views.SilenceList.SilenceView.view
                                    (showConfirmationDialog == Just silence.id)
                                    silence
                            )
                            silencesInTab
                        )

        Failure msg ->
            error msg

        _ ->
            loading


groupSilencesByState : List Silence -> List ( State, List Silence )
groupSilencesByState silences =
    List.map (\state -> ( state, filterSilencesByState state silences )) states


states : List State
states =
    [ Active, Pending, Expired ]


filterSilencesByState : State -> List Silence -> List Silence
filterSilencesByState state =
    List.filter (.status >> .state >> (==) state)
