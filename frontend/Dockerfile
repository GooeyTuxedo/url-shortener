# Build stage
FROM node:22-alpine AS builder

WORKDIR /app

# Install dependencies
COPY package.json package-lock.json* ./
RUN npm ci

# Add rest of the client code
COPY elm.json ./
COPY index.html ./
COPY static ./static
COPY src ./src

# Build the production assets
RUN npm run build

# Serve stage - using a minimal static file server
FROM node:22-alpine

WORKDIR /app

# Install a lightweight static file server
RUN npm install -g serve

# Copy the built assets from the builder stage
COPY --from=builder /app/dist /app/dist

# Expose port
EXPOSE 80

# Start static file server
CMD ["serve", "-s", "dist", "-l", "80"]