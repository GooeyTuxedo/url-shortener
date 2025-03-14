port module Ports exposing
    ( clipboardStatus
    , copyToClipboard
    , downloadQrCode
    , qrCodeDownloaded
    , storeClientId
    , storedClientId
    , windowResize
    )

import Types exposing (ClipboardStatus, QrCodeDownloadStatus)



-- Outgoing ports (Elm to JS)


port copyToClipboard : String -> Cmd msg


port downloadQrCode : { url : String, filename : String } -> Cmd msg


port storeClientId : String -> Cmd msg  -- New port to store client ID



-- Incoming ports (JS to Elm)


port windowResize : (Int -> msg) -> Sub msg


port clipboardStatus : (ClipboardStatus -> msg) -> Sub msg


port qrCodeDownloaded : (QrCodeDownloadStatus -> msg) -> Sub msg


port storedClientId : (String -> msg) -> Sub msg  -- New port to receive stored client ID
