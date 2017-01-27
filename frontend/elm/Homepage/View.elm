module Homepage.View exposing (view)

import GitHubOAuthConfig
import Html exposing (..)
import Html.Attributes exposing (..)
import Homepage.Types exposing (..)
import Markdown


view : Model -> Html Msg
view model =
    div []
        [ hero
        , homepageContent model
        ]


hero : Html Msg
hero =
    div [ class "hero" ]
        [ div [ class "homepage-header" ]
            [ h1 []
                [ img [ src "/img/elm-logo-mono.svg", alt "Das Elm-Logo" ] []
                , span [] [ text "elm-lang.de" ]
                ]
            , div [ class "homepage-subheader" ]
                [ p [ class "homepage-subheader-large" ] [ text "Die deutsche Elm-Community" ]
                , p [] [ text "Alles rund um Elm im deutschprachigen Raum" ]
                ]
            ]
        ]


homepageContent : Model -> Html Msg
homepageContent model =
    {-
       TODO
          Hier müsste jetzt jede Menge Content kommen...

          * Fetter Link auf das Entwicklerverzeichnis
          * Call to Action, um sich einzutragen
          * Die neusten drei Entwickler
    -}
    let
        gitHubOAuthUrl =
            GitHubOAuthConfig.gitHubOAuthUrl model Nothing

        signInWithGitHub =
            case gitHubOAuthUrl of
                Just url ->
                    "[GitHub anmelden]("
                        ++ url
                        ++ ")"

                otherwise ->
                    "GitHub anmelden"
    in
        -- TODO Der Verweis auf die Anmeldung per GitHub und das Anlegen eines
        -- Entwicklerprofils ist etwas überflüssig, wenn der Benutzer chon
        -- angemeldet ist.
        Markdown.toHtml [ class "homepage-content" ]
            ("""
Herzlich willkommen auf elm-lang.de, dem Hub für die deutschsprachige Elm-Community. Hier kannst du dich mit anderen Elm-Entwicklern vernetzen, und sehen, was rund um Elm in deiner Umgebung passiert.

### Entwicklerverzeichnis

Du interessierst dich für Elm, hast vielleicht schon erste Projekte damit umgesetzt oder setzt es sogar produktiv ein? Trag dich auf jeden Fall ins Entwicklerverzeichnis ein! Um ein Profil anzulegen, musst du dich nur mit """
                ++ signInWithGitHub
                ++ """, dann wird automatisch ein Entwickler-Profil für dich angelegt.

Um zu sehen, wer sonst noch so was mit Elm macht, wirf einen Blick ins [Entwicklerverzeichnis](#developers).

### Events

Alle Meetups, Talks und Konferenzen mit Bezug zu Elm findest du unter [Termine](#events).
        """
            )
