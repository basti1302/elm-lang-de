module GitHubOAuthConfig exposing (GitHubOAuthConfig, gitHubOAuthUrl)

import RemoteData exposing (RemoteData(..))


type alias GitHubOAuthConfig =
    { clientId : String
    , redirectUrl : String
    }


gitHubOAuthUrl :
    { someModel | gitHubOAuthConfig : RemoteData e GitHubOAuthConfig }
    -> Maybe String
    -> Maybe String
gitHubOAuthUrl model localFragment =
    case model.gitHubOAuthConfig of
        Success gitHubOAuthConfig ->
            let
                redirectUrl =
                    case localFragment of
                        Just fragment ->
                            gitHubOAuthConfig.redirectUrl ++ fragment

                        Nothing ->
                            gitHubOAuthConfig.redirectUrl

                clientId =
                    gitHubOAuthConfig.clientId
            in
                "https://github.com/login/oauth/authorize?client_id="
                    ++ clientId
                    ++ "&redirect_uri="
                    ++ redirectUrl
                    |> Just

        otherwise ->
            Nothing
