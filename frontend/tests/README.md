# URL Shortener Frontend Test Suite

This document explains how to run and extend the test suite for the Elm frontend of the URL Shortener application.

## Running Tests

To run all tests:

```bash
npm test
# or
npx elm-test
```

To run specific test modules:

```bash
npx elm-test tests/ApiTests.elm
```

To run tests with watch mode (tests rerun when files change):

```bash
npx elm-test --watch
```

## Test Coverage

The test suite covers the following areas:

1. **API Module Tests** - Tests for API endpoint construction and request encoding
2. **Route Module Tests** - Tests for URL parsing and route generation
3. **Types Module Tests** - Tests for data structures and type validation
4. **Main Module Tests** - Tests for update function and application logic
5. **View Module Tests** - Tests for UI rendering and element structure
6. **Ports Tests** - Tests for JavaScript interop through ports
7. **Form Tests** - Tests for form validation and input handling
8. **Integration Tests** - Tests for user flows and complex scenarios

## Test Organization

Tests are organized by module responsibility:

- **Unit Tests** - Small tests that focus on a single function or feature
- **Integration Tests** - Larger tests that verify multiple components working together
- **Helper Functions** - Shared test utilities in `TestHelpers.elm`

## Mocks and Stubs

The tests use several types of test doubles:

- **Model Mocks** - Predefined model states for testing
- **Navigation Mocks** - `Nav.dummy` for navigation testing
- **HTTP Response Mocks** - Simulated API responses

## Common Testing Patterns

### Testing Update Function

```elm
test "Description" <|
    \_ ->
        let
            initialModel = mockModel
            
            (newModel, cmd) =
                Main.update SomeMsg initialModel
        in
        Expect.equal newModel.someField expectedValue
```

### Testing Views

```elm
test "Description" <|
    \_ ->
        View.someView mockModel
            |> elementToHtml
            |> Query.fromHtml
            |> Query.has [ Selector.text "Expected Text" ]
```

### Testing Form Submissions

```elm
test "Description" <|
    \_ ->
        let
            modelWithFilledForm = { mockModel | someField = someValue }
            
            (newModel, cmd) =
                Main.update SubmitForm modelWithFilledForm
        in
        -- assertions here
```

## Extending the Tests

When adding new features, extend the test suite by:

1. **Adding Test Cases** - Add new test cases to existing modules
2. **Adding Test Modules** - Create new test modules for major features
3. **Updating Mocks** - Add or update mock data in `TestHelpers.elm`

## Best Practices

- Test both success and failure paths
- Keep tests independent and deterministic
- Use descriptive test names that explain what's being tested
- Group related tests with `describe`
- Use helper functions to reduce repetition

## Troubleshooting

If tests are failing, check:

1. **API Changes** - The test might be expecting a different API structure
2. **Element Structure** - UI changes might break view tests
3. **Model Structure** - Changes to the model might break mocks
4. **Missing Dependencies** - Ensure all test dependencies are installed

## Continuous Integration

These tests are designed to run in a CI environment. Add the following to your CI configuration:

```yaml
# Example GitHub Actions workflow
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v2
    - uses: actions/setup-node@v2
      with:
        node-version: '16'
    - run: npm ci
    - run: npm test
```