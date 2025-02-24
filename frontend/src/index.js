import { Elm } from './Main.elm';

// Initialize the Elm application
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    apiUrl: process.env.ELM_APP_API_URL || 'http://localhost:8080',
    windowWidth: window.innerWidth
  }
});

// Add event listeners for window resize to update Elm app
window.addEventListener('resize', () => {
  app.ports.windowResize.send(window.innerWidth);
});

// Listen for copy to clipboard requests from Elm
if (app.ports && app.ports.copyToClipboard) {
  app.ports.copyToClipboard.subscribe(text => {
    navigator.clipboard.writeText(text)
      .then(() => {
        app.ports.clipboardStatus.send({ success: true, text });
      })
      .catch(err => {
        console.error('Failed to copy text: ', err);
        app.ports.clipboardStatus.send({ success: false, text });
      });
  });
}

// Listen for download QR code requests from Elm
if (app.ports && app.ports.downloadQrCode) {
  app.ports.downloadQrCode.subscribe(({ url, filename }) => {
    fetch(url)
      .then(response => response.blob())
      .then(blob => {
        const blobUrl = URL.createObjectURL(blob);
        const link = document.createElement('a');
        link.href = blobUrl;
        link.download = filename || 'qrcode.png';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
        URL.revokeObjectURL(blobUrl);
        app.ports.qrCodeDownloaded.send({ success: true });
      })
      .catch(err => {
        console.error('Failed to download QR code: ', err);
        app.ports.qrCodeDownloaded.send({ success: false });
      });
  });
}