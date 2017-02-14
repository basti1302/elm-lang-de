module Events.View exposing (view)

import Html exposing (..)
import Events.Types exposing (..)
import Markdown


view : Model -> Html Msg
view model =
    Markdown.toHtml []
        """
Termine
=======

### [Elm Netherlands - 3rd Edition - Tooling](https://www.meetup.com/Elm-Netherlands/events/236871202)

* <span class="fa fa-calendar"></span> 08.03.2017, 18:00 Uhr
* <span class="fa fa-globe"></span> Arthur van Schendelstraat 500, Utrecht
* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Netherlands/events/236871202

### [Workshop: Elm für Einsteiger](https://react-days.de/session/elm-fuer-einsteiger/)

* Workshop im Rahmen der React Days 2017
* <span class="fa fa-calendar"></span> 22.03.2017, 13:30 - 17:00
* <span class="fa fa-globe"></span> React Days 2017, Holiday Inn München – Unterhaching, Inselkammerstraße 7-9, München
* <span class="fa fa-link"></span> https://react-days.de/session/elm-fuer-einsteiger/

### [Elm Europe](https://elmeurope.org)

* <span class="fa fa-calendar"></span> 08. - 09.06.2017
* <span class="fa fa-link"></span> https://elmeurope.org
* <span class="fa fa-globe"></span> Paris/Villejuif, Frankreich

## Regelmäßige Meetups mit Bezug zu Elm

* <span class="fa fa-meetup"></span> Übersicht: https://www.meetup.com/topics/elm/all

### [Elmoin](https://www.meetup.com/Elmoin)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elmoin
* <span class="fa fa-twitter"></span> https://twitter.com/elmoinmoin

### [Functional Programming Berlin](https://www.meetup.com/Functional-Programming-Berlin)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Functional-Programming-Berlin

### [Elm Berlin](https://www.meetup.com/https://www.meetup.com/Elm-Berlin/)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Berlin/

### [Berlin Elm Hackathon](https://www.meetup.com/berlin-elm-hackathon/)

* <span class="fa fa-meetup"></span> https://www.meetup.com/berlin-elm-hackathon/

### [Berlin Elixir Elm Developers](https://www.meetup.com/Berlin-Elixir-Elm-Developers)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Berlin-Elixir-Elm-Developers

### [Elm Meetup Munich](https://www.meetup.com/Elm-Meetup-Munich)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Meetup-Munich

### [Elm Amsterdam](https://www.meetup.com/Elm-Amsterdam)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Amsterdam

### [Elm Zurich](https://www.meetup.com/Elm-Zurich)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Zurich

### [Elm Netherlands](https://www.meetup.com/Elm-Netherlands)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Elm-Netherlands

### [Elm Meetup Brussels](https://www.meetup.com/Meetup-Elm-Brussels)

* <span class="fa fa-meetup"></span> https://www.meetup.com/Meetup-Elm-Brussels

"""
