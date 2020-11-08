FROM node:15.0.1-alpine3.10

WORKDIR /app

RUN npm install -g elm@latest-0.19.0

COPY . .

RUN elm make src/Main.elm --output=main.js

CMD [ "elm", "reactor" ]



