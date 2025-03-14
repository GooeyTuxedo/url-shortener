module Types exposing (..)

import TestableNavigation
import Time


type alias Flags =
    { apiUrl : String
    , windowWidth : Int
    }


type alias ShortUrl =
    { shortUrl : String
    , originalUrl : String
    , shortCode : String
    , createdAt : Time.Posix
    , expiresAt : Maybe Time.Posix
    , clickCount : Int
    , qrCodeUrl : String
    , clientId : String
    }


type alias CreateShortUrlRequest =
    { longUrl : String
    , customAlias : Maybe String
    , expiresIn : Maybe Int
    }


type alias Model =
    { apiUrl : String
    , windowWidth : Int
    , page : Page
    , urls : List ShortUrl
    , currentUrl : Maybe ShortUrl
    , shortenForm : ShortenForm
    , notification : Maybe Notification
    , isLoading : Bool
    , errorMessage : Maybe String
    , navKey : TestableNavigation.Key
    , clientId : Maybe String
    , tempClientId : String
    }


type Page
    = HomePage
    | UrlDetailsPage String
    | ClientDashboardPage
    | NotFoundPage


type alias ShortenForm =
    { url : String
    , customAlias : String
    , useCustomAlias : Bool
    , expiresIn : Maybe Int
    }


type alias Notification =
    { message : String
    , notificationType : NotificationType
    }


type NotificationType
    = Success
    | Error
    | Info
    | Warning


type alias ClipboardStatus =
    { success : Bool
    , text : String
    }


type alias QrCodeDownloadStatus =
    { success : Bool
    }
