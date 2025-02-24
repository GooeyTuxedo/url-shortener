FROM haskell:9.8 as builder

WORKDIR /app

# Copy package configuration files
COPY stack.yaml stack.yaml.lock* ./
COPY package.yaml ./

# Build dependencies only (better caching)
RUN stack setup
RUN stack build --only-dependencies

# Copy source code
COPY . .

# Build the application
RUN stack build

# Create a smaller runtime image
FROM debian:bullseye-slim as runtime

# Install required system libraries
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libpq-dev \
    ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the binary from builder stage
COPY --from=builder /app/.stack-work/dist/*/build/url-shortener-exe/url-shortener-exe .

# Set environment variables
ENV PORT=8080

EXPOSE 8080

# Run the application
CMD ["/app/url-shortener-exe"]