module Msg exposing (Msg(..))

import Browser
import Http
import Time
import Types exposing (ClipboardStatus, NotificationType, QrCodeDownloadStatus, ShortUrl)
import Url


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | WindowResized Int
    | UrlInputChanged String
    | CustomAliasInputChanged String
    | ToggleCustomAlias Bool
    | ExpiresInChanged (Maybe Int)
    | SubmitForm
    | UrlCreated (Result Http.Error ShortUrl)
    | LoadUrls
    | UrlsLoaded (Result Http.Error (List ShortUrl))
    | LoadUrlDetails String
    | UrlDetailsLoaded (Result Http.Error ShortUrl)
    | NavigateTo String
    | ShowNotification String NotificationType
    | DismissNotification
    | CopyToClipboard String
    | ClipboardResult ClipboardStatus
    | DownloadQrCode String String
    | QrCodeDownloaded QrCodeDownloadStatus
    | TimeReceived Time.Posix
    -- New messages for client ID functionality
    | ClientIdChanged String
    | SaveClientId
    | LoadClientUrls
    | ClientUrlsLoaded (Result Http.Error (List ShortUrl))
    | StoredClientIdReceived String