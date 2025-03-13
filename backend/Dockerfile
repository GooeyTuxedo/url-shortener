FROM haskell:9.2.8 AS builder

WORKDIR /app

# Copy package configuration files
COPY stack.yaml stack.yaml.lock* ./
COPY package.yaml ./

# Build dependencies only (better caching)
RUN stack setup
RUN stack build --only-dependencies

# Copy source code
COPY app/ ./app/
COPY src/ ./src/
COPY test/ ./test/

# Build the application with optimizations for production
RUN stack build --ghc-options="-O2"

# # Create a smaller runtime image
# FROM debian:bullseye-slim AS runtime

# # Install required system libraries and cleanup in one layer
# RUN apt-get update && \
#     apt-get install -y --no-install-recommends \
#     libpq-dev \
#     ca-certificates && \
#     apt-get clean && \
#     rm -rf /var/lib/apt/lists/*

# WORKDIR /app

# Copy the binary from builder stage
# COPY --from=builder /app/.stack-work/dist/x86_64-linux/ghc-9.2.8/build/url-shortener-exe/url-shortener-exe .

# Create a non-root user to run the application
# RUN useradd -m appuser && \
#     chown -R appuser:appuser /app

# # Switch to non-root user
# USER appuser

# Set environment variables (these will be overridden in Kubernetes)
ENV PORT=8080

EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:${PORT}/health || exit 1

# Run the application
CMD ["/app/.stack-work/dist/x86_64-linux/ghc-9.2.8/build/url-shortener-exe/url-shortener-exe"]
