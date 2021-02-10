FROM node:15.0.1-alpine3.10

WORKDIR /app

# `package.json` と `package-lock.json` （あれば）を両方コピーする
COPY package*.json ./

RUN npm install -g elm@latest-0.19.1

