# URL Shortener Test Suite

This directory contains comprehensive tests for the URL shortener application.

## Running Tests

To run the entire test suite:

```bash
stack test
```

To run a specific test file:

```bash
stack test --ta '-p "QRGenerator"'
```

## Test Organization

The test suite is organized into the following components:

### Unit Tests

- **ShortenerSpec.hs**: Tests for the URL shortening functions
- **UtilsSpec.hs**: Tests for utility functions like URL validation
- **QRGeneratorSpec.hs**: Tests for QR code generation
- **RateLimiterSpec.hs**: Tests for the rate limiting functionality
- **AbuseProtectionSpec.hs**: Tests for URL content filtering and security

### Integration Tests

- **ApiSpec.hs**: End-to-end tests for the HTTP API
- **ModelsSpec.hs**: Tests for database models and operations

### Property Tests

- **PropertyTests.hs**: QuickCheck property-based tests for various components

## Test Database

Integration tests that require a database use a test-specific PostgreSQL database. Before running the tests, ensure your database is set up:

```bash
createdb urlshortener_test
```

Or use environment variables to configure a different test database:

```bash
TEST_DB_HOST=localhost TEST_DB_PORT=5432 TEST_DB_USER=test TEST_DB_PASSWORD=test TEST_DB_NAME=urlshortener_test stack test
```

## Adding New Tests

1. Create a new test file in the `test/` directory
2. Name it according to the component being tested (e.g., `NewFeatureSpec.hs`)
3. Include a `spec` function exported by the module
4. Run `stack test` to verify your new tests

## Mocking

For tests that need to mock external services:

- Use the `withMockManager` function to create a mock HTTP manager
- Use the `withTestEnv` function to create a test environment with mocks

## Coverage

To generate a test coverage report:

```bash
stack test --coverage
```

This will generate an HTML report in the `.stack-work/coverage` directory.
