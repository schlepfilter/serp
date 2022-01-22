spago bundle-app --to web/index.js --watch

python3 -m http.server -d web 8080
