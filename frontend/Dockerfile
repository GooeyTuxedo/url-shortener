FROM node:latest AS builder

WORKDIR /app

# Install dependencies
COPY package.json package-lock.json* ./
RUN npm install

# Add rest of the client code
COPY elm.json ./
COPY index.html ./
COPY static ./static
COPY src ./src

RUN npm run build

# Make port 3000 available outside the container
EXPOSE 3000

# Start the development server
CMD ["npm", "start"]