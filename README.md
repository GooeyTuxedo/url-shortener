# Haskell URL Shortener

A robust URL shortening service built with Haskell, Servant, and PostgreSQL.

## Features

- Create short URLs from long ones
- Optional custom aliases
- URL expiration
- Visit tracking
- RESTful API
- Containerized development environment
- Rate limiting to prevent abuse
- Content filtering for malicious URLs
- Security checks for URL validation

## Tech Stack

- **Language:** Haskell
- **Web Framework:** Servant
- **Database:** PostgreSQL with Persistent ORM
- **Development:** Docker & Docker Compose

## Getting Started

### Prerequisites

- Docker and Docker Compose

### Running the Development Environment

1. Clone this repository
2. Start the application:

```bash
docker-compose up
```

The application will be available at http://localhost:8080.

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
  "clickCount": 5
}
```

### Redirect to Original URL

**Endpoint:** `GET /{shortCode}`

Redirects to the original URL associated with the given short code.

## Project Structure

- `app/`: Application entry point
- `src/`: Application source code
  - `Api.hs`: API endpoints using Servant
  - `App.hs`: Application environment setup
  - `Config.hs`: Configuration loading
  - `Models.hs`: Database models with Persistent
  - `Shortener.hs`: URL shortening logic
  - `Utils.hs`: Helper functions
- `test/`: Test suite
- `Dockerfile`: Docker configuration
- `docker-compose.yml`: Development environment setup

## License

This project is licensed under the MIT License.