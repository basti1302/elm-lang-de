module Imprint.View exposing (view)

import Html exposing (..)
import Imprint.Types exposing (..)


-- Actually, this could be just a servant-ede template. Or even just static
-- HTML, served by Servant's StaticFile handler. Well, whatever.


view : Html Msg
view =
    div []
        [ h1 [] [ text "Impressum" ]
        , span [] [ text "Bastian Krol" ]
        , br [] []
        , span [] [ text "Winterkamp 26" ]
        , br [] []
        , span [] [ text "44267 Dortmund" ]
        , br [] []
        , span [] [ text "Deutschland" ]
        , h2 [] [ text "Contact" ]
        , span [] [ text "E-Mail: bastian.krol@codecentric.de" ]
        ]
