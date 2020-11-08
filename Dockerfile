FROM node:15.0.1-alpine3.10

WORKDIR /app

RUN npm install -g elm@latest-0.19.0

COPY . .

# 環境設定ファイルのコピー(いつか消す)
RUN cp src/Config/Env.elm.sample src/Config/Env.elm

RUN elm make src/Main.elm --output=main.js

CMD [ "elm", "reactor" ]



