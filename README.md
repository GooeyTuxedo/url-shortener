# Haskell URL Shortener with Elm Frontend

A robust URL shortening service built with Haskell backend and Elm frontend.

## Features

- Create short URLs from long ones
- Optional custom aliases
- URL expiration
- Visit tracking
- QR code generation for shortened URLs
- Modern Elm single-page application frontend
- RESTful API
- Containerized development environment
- Rate limiting to prevent abuse
- Content filtering for malicious URLs
- Security checks for URL validation

## Tech Stack

### Backend
- **Language:** Haskell
- **Web Framework:** Servant
- **Database:** PostgreSQL with Persistent ORM
- **Security:** Rate limiting middleware, URL content filtering

### Frontend
- **Language:** Elm
- **UI Library:** elm-ui
- **Bundler:** Parcel
- **Design:** Responsive CSS, mobile-friendly layout

### Development
- Docker & Docker Compose for containerization

## Getting Started

### Prerequisites

- Docker and Docker Compose

### Running the Full Stack Application

1. Clone this repository
2. Start the application:

```bash
docker compose up
```

This will start:
- PostgreSQL database
- Haskell backend API
- Elm frontend development server

The frontend will be available at http://localhost:3000, and the backend API at http://localhost:8080.

### Running Only the Backend

If you want to run just the backend API:

```bash
docker compose up backend
```

The API will be available at http://localhost:8080.

### Development Workflow

#### Backend Development
1. Make changes to Haskell files
2. The application will automatically rebuild and restart

#### Frontend Development
1. Make changes to Elm files in the `frontend/src` directory
2. Parcel will automatically rebuild the application
3. Refresh your browser to see changes

## API Documentation

### Create a Short URL

**Endpoint:** `POST /api/shorten`

**Request Body:**
```json
{
  "longUrl": "https://example.com/very/long/url/that/needs/shortening",
  "customAlias": "mylink",  // Optional
  "expiresIn": 30  // Optional (days)
}
```

**Response:**
```json
{
  "shortUrl": "http://localhost:8080/mylink",
  "originalUrl": "https://example.com/very/long/url/that/needs/shortening",
  "shortCode": "mylink",
  "createdAt": "2025-02-24T12:00:00Z",
  "expiresAt": "2025-03-26T12:00:00Z",
  "clickCount": 0
}
```

### Get URL Information

**Endpoint:** `GET /api/urls/{shortCode}`

**Response:**
```json
{
  "shortUrl": "http://localhost:8080/mylink",
  "originalUrl": "https://example.com/very/long/url/that/needs/shortening",
  "shortCode": "mylink",
  "createdAt": "2025-02-24T12:00:00Z",
  "expiresAt": "2025-03-26T12:00:00Z",
  "clickCount": 5,
  "qrCodeUrl": "http://localhost:8080/api/qrcode/mylink"
}
```

### Get QR Code for URL

**Endpoint:** `GET /api/qrcode/{shortCode}`

**Optional Query Parameters:**
- `size`: QR code size in pixels (min: 100, max: 1000, default: 300)

**Response:**
Binary PNG image data of the QR code that links to the shortened URL.

### Redirect to Original URL

**Endpoint:** `GET /{shortCode}`

Redirects to the original URL associated with the given short code.

## Project Structure

### Backend
- `backend/`: Haskell application
  - `app/`: Application entry point
  - `src/`: Application source code
    - `Api.hs`: API endpoints using Servant
    - `App.hs`: Application environment setup
    - `Config.hs`: Configuration loading
    - `Models.hs`: Database models with Persistent
    - `Shortener.hs`: URL shortening logic
    - `Utils.hs`: Helper functions
    - `RateLimiter.hs`: Rate limiting implementation
    - `AbuseProtection.hs`: URL content filtering
    - `Middleware.hs`: Security middleware
    - `QRGenerator.hs`: QR code generation
  - `test/`: Test suite
  - `Dockerfile`: Docker configuration
- `docker-compose.yml`: Development environment setup

### Frontend
- `frontend/`: Elm application
  - `src/`: Source code
    - `Main.elm`: Entry point and application setup
    - `Api.elm`: Backend API communication
    - `Types.elm`: Type definitions
    - `Msg.elm`: Message types for update function
    - `Ports.elm`: JavaScript interop
    - `Route.elm`: URL routing
    - `View/`: UI components
      - `Layout.elm`: Main layout components
      - `Home.elm`: Home page view
      - `Shortener.elm`: URL shortener form
      - `UrlList.elm`: Created URLs list
      - `UrlDetails.elm`: URL details page
      - `NotFound.elm`: 404 page
    - `static/`: Static assets
  - `index.html`: HTML entry point
  - `Dockerfile`: Frontend container configuration

## Security Features

### Rate Limiting
- Configurable request limits per IP address
- Prevents abuse of the API
- Customizable time windows and request thresholds

### URL Content Filtering
- Blacklist-based URL filtering
- Blocks potentially malicious domains and patterns
- Configurable maximum URL length

### Security Headers
- HTTP security headers to prevent common web vulnerabilities
- Protection against XSS, clickjacking, and content type sniffing

## Future Enhancements

Potential areas for future development:

- User authentication and personal URL dashboards
- Advanced analytics with geographic data
- Custom QR code styling
- Link grouping and tagging
- Social sharing integration
- Campaign tracking parameters
- Custom link expiration notifications

## License

This project is licensed under the MIT License.