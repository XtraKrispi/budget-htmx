target: watch-app watch-tailwind
watch-app:
	ghcid -c 'stack ghci' \
      --reload=./Main.hs \
      -T Main.main \
      --restart=./budget.cabal

watch-tailwind:
	npx tailwindcss -i ./input.css -o ./static/app.css --watch