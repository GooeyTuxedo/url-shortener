FROM node:21-alpine

WORKDIR /app

# Install dependencies
COPY package.json package-lock.json* ./
RUN npm install

# Add rest of the client code
COPY . .

# Make port 3000 available outside the container
EXPOSE 3000

# Start the development server
CMD ["npm", "start"]