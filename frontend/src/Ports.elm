port module Ports exposing
    ( clipboardStatus
    , copyToClipboard
    , downloadQrCode
    , qrCodeDownloaded
    , windowResize
    )

import Types exposing (ClipboardStatus, QrCodeDownloadStatus)



-- Outgoing ports (Elm to JS)


port copyToClipboard : String -> Cmd msg


port downloadQrCode : { url : String, filename : String } -> Cmd msg



-- Incoming ports (JS to Elm)


port windowResize : (Int -> msg) -> Sub msg


port clipboardStatus : (ClipboardStatus -> msg) -> Sub msg


port qrCodeDownloaded : (QrCodeDownloadStatus -> msg) -> Sub msg
